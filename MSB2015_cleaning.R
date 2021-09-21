
#----------------#
#   User input   #
#----------------#

#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","tidyr","pcaMethods")
if (!requireNamespace("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
  BiocManager::install(version = "3.13") 
}

for (p in packages){
  if (!require(p, character.only = TRUE)){
    BiocManager::install(p,update = FALSE)
    if(!require(p,character.only = TRUE)) {stop("Package not found")}
  }
}

#------------------------#
#   Defining functions   #
#------------------------#

#Extracting any type of data from the GBD dataset, created for flexibility during testing
gbd_extract <- function(dat,measure,numType,disease,countries,years){
  dat2<- dat[dat$measure == measure,]
  dat2 <- dat2[dat2$metric == numType,]
  dat2 <- dat2[dat2$cause == disease,]
  dat2 <- dat2[dat2$location %in% commonCountries,]
  dat2 <- dat2[dat2$year %in% years,]
  dat2 <- dat2[,c(2,7,8)]
  dat2 <- dat2[order(dat2$location,dat2$year,dat2$val),]
  
  return(dat2)
}

#--------------------#
#   Importing data   #
#--------------------#
datasets <- vector("list",18)
names(datasets) <-  c("CH4","CO2","mining","forest","GHG","GhgRelative","hazardousWastePerCap",
                      "hazardousGenerated","hazardousBurned","hazardousLandfilled","hazardousRecycled",
                      "hazardousTreated","N20","NO","Renewable","SO2","municipalWasteServed","GbdData")
files <- list.files(path = paste0(getwd(),"/data"))
for (d in 1:length(datasets)){
  datasets[[d]] <- read.csv(file = paste0(getwd(),"/data/",files[d]),header = TRUE,na.strings = c("...",""))
}


#checking missing values per measure
for (i in 1:length(datasets)){
  naMatrix <- as.matrix(is.na.data.frame(datasets[[i]]))
  naProp <- colSums(naMatrix)/nrow(naMatrix)
  plot(naProp,main = names(datasets)[i])
}

#Cutting of "useless" data
for (i in c(1,2,5,13,14,16)){ #For GH gases
  datasets[[i]] <- datasets[[i]][-1,-c(1,32:35)]
  colnames(datasets[[i]]) <- c("country",1990:2018)
}
datasets[["forest"]] <- datasets[["forest"]][-1,-c(1,8:11)]
datasets[[6]] <- NULL #removing the GhgRelative dataset
for (i in c(6:11,14,16)){
  index2017 <- which.max(colnames(datasets[[i]] )== "X2017")
  datasets[[i]] <- datasets[[i]][,-c(1,index2017+1 : ncol(datasets[[i]]))]
}

#checking missing values per measure
for (i in 1:length(datasets)){
  naMatrix <- as.matrix(is.na.data.frame(datasets[[i]]))
  naProp <- colSums(naMatrix)/nrow(naMatrix)
  plot(naProp,main = names(datasets)[i])
}

#Checking missing values per country
enoughDataCountries <- vector("list",16)
names(enoughDataCountries) <- names(datasets)[-17]
for (i in 1:16){
  Na30p <- is.na.data.frame(datasets[[i]])
  Na30p <- apply(Na30p[,-1],1,function(x){
    sum(x) / length(x)
  })
  enoughDataCountries[[i]] <- datasets[[i]][Na30p < 0.3,1]
}
commonCountries <- Reduce(intersect,enoughDataCountries)

# Removing hazardous waste from calculation
enoughDataCountriesNoWaste <- enoughDataCountries[c(1:3,5,12:15)]
commonCountriesNoWaste <- Reduce(intersect,enoughDataCountriesNoWaste) #Better than when keeping waste


#selection of datasets
datasets <- datasets[c(1:3,5,12:15,17)]
#Cutting year 2018 from datasets
for (i in c(1:6,8)){
  datasets[[i]] <- datasets[[i]][,-c(30)]
}

#Checking missing values per country again
enoughDataCountries <- vector("list",8)
names(enoughDataCountries) <- names(datasets)[-9]
for (i in 1:8){
  Na30p <- is.na.data.frame(datasets[[i]])
  Na30p <- apply(Na30p[,-1],1,function(x){
    sum(x) / length(x)
  })
  enoughDataCountries[[i]] <- datasets[[i]][Na30p < 0.3,1]
}
commonCountries <- Reduce(intersect,enoughDataCountries)


#Removing unusable countries and cleaning variable names and 
#data type for the environment data, reorganizing data
datasetsClean <- lapply(datasets[-9], function(x){
  dat <- x[x[,1] %in% commonCountries,]
  colnames(dat) <-  c("country",1990:2017)
  dat[,2:ncol(dat)] <- apply(dat[,2:ncol(dat)],2,function(x2){
    x2 <- gsub(",","",x2)
    x2 <- as.numeric(x2)
  })
  
  dat <- pivot_longer(dat,cols = -1,names_to = "year", values_to = "Value")
  dat$year <- as.numeric(dat$year)
  return(dat)
})

#extracting needed GBD data
copdDailys <- gbd_extract(datasets[[9]],"DALYs (Disability-Adjusted Life Years)",
                          "Number","Chronic obstructive pulmonary disease",
                          commonCountries,
                          1990:2017)
datasetsClean[[9]] <- copdDailys
names(datasetsClean)[9] <- "copdDalys"
#Combining the data 
Data <- data.frame(datasetsClean[[1]],datasetsClean[[2]][,3],
                   datasetsClean[[3]][,3],datasetsClean[[4]][,3],datasetsClean[[5]][,3],
                   datasetsClean[[6]][,3],datasetsClean[[7]][,3],datasetsClean[[8]][,3],
                   datasetsClean[[9]][,3])
colnames(Data)[-c(1,2)] <- names(datasetsClean)


# #check countries with too many missing values over too many variables
# rowWiseNa <- apply(naDat[,-c(1,2)],1,sum)
# missingDataCountries <- unique(naDat[(rowWiseNa / 9) > 0.3,1])
# Data <- Data[!(Data$country %in% missingDataCountries),]

#Removing all rows with missing values
naDat <- is.na.data.frame(Data)
naDat <- rowSums(naDat) == 0

Data <- Data[naDat,]

#Transforming data into ratio (year/previous year)
countries <- unique(Data$country)

for (i in 1:length(countries)){
  dataTemp <- Data[Data$country == countries[i],]
  dataTemp2 <- dataTemp[-nrow(dataTemp),]
  dataTemp <- dataTemp[-1,]
  year <- paste0(dataTemp2$year,"_to_",dataTemp$year)
  dataTemp3 <- as.data.frame(mapply('-',dataTemp[,-c(1,2)],dataTemp2[,-c(1,2)]))
  dataTemp3 <- data.frame(rep(countries[i],nrow(dataTemp)),year,dataTemp3)
  colnames(dataTemp3)[1] <- "country"
  if (i == 1){
    DataChange <- dataTemp3
  }
  else{
    DataChange <- rbind(DataChange,dataTemp3)
  }
}
#min-max Scaling
DataChange[,-c(1,2)] <- apply(DataChange[,-c(1,2)],2,function(x){
   (x - min(x))/(max(x)-min(x))
})

#Exploring relationship
for (var in colnames(DataChange)[-c(1,2,11)]){
  plot(DataChange[,var],DataChange$copdDalys,xlab = var,ylab = "COPD Dalys")
}

#creating shifted data
dataShifted <- vector(mode = "list",5)
shifts <- c(1,5,7,10,15)
for (i in 1:length(shifts)){
  for (ii in 1:length(countries)){
    dataTemp <- DataChange[DataChange$country == countries[ii],]
    predTemp <- dataTemp[-((nrow(dataTemp)-shifts[i]+1):nrow(dataTemp)),-ncol(dataTemp)]
    critTemp <- dataTemp[-(1:shifts[i]),ncol(dataTemp)]
    dataTemp <- data.frame(predTemp,critTemp)
    if (ii == 1){
      dataTemp2 <- dataTemp
    }
    else{
      dataTemp2 <- rbind(dataTemp2,dataTemp)
    }
  }
  dataShifted[[i]] <- dataTemp2
  names(dataShifted)[i] <- paste0("YearMinus",shifts[i])
  colnames(dataShifted[[i]])[11] <- "copdDalys"
}
for (var in colnames(dataShifted[[i]])[-c(1,2,11)]){
  for (i in 1:length(dataShifted)){
  
    plot(dataShifted[[i]][,var],dataShifted[[i]]$copdDalys,
         main = names(dataShifted)[i],xlab = var,ylab = "COPD Dalys")
  }
}
