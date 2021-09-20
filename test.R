library("ggplot2")
library("imputeTS")
library("Amelia")
library("tidyr")
library("parallel")
library("caret")
#Functions
pareto <- function(x){
  m <- colMeans(x,na.rm = TRUE)
  square_dev <- apply(x,2,function(x2){
    y <- sqrt(sd(x2,na.rm = TRUE))
    return(y)
  })
  scaled <- (x-m)/square_dev
  return(scaled)
}
#Importing data
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
commonCountriesNoWaste <- Reduce(intersect,enoughDataCountriesNoWaste)

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
  #test scaling
  #dat[,2:ncol(dat)] <- pareto(dat[,2:ncol(dat)])
  #
  dat <- pivot_longer(dat,cols = -1,names_to = "year", values_to = "Value")
  dat$year <- as.numeric(dat$year)
  return(dat)
})
#extracting needed GBD data
#Defining function to easily extract data
gbd_extract <- function(dat,measure,numType,disease,countries,years){
  dat2<- dat[dat$measure == measure,]
  dat2 <- dat2[dat2$metric == numType,]
  dat2 <- dat2[dat2$cause == disease,]
  dat2 <- dat2[dat2$location %in% commonCountries,]
  dat2 <- dat2[dat2$year %in% years,]
  dat2 <- dat2[,c(2,7,8)]
  dat2 <- dat2[order(dat2$location,dat2$year,dat2$val),]
  # dat2 <- pivot_wider(dat2,names_from = 2,values_from = 3)
  # dat2[,-1] <- pareto(dat2[,-1])
  # dat2 <- pivot_longer(dat2,cols = -1,names_to = "year",values_to = "val")
  return(dat2)
}
copdDailys <- gbd_extract(datasets[[9]],"DALYs (Disability-Adjusted Life Years)",
                          "Number","Chronic obstructive pulmonary disease",
                          commonCountries,
                          1990:2017)
datasetsClean[[9]] <- copdDailys
names(datasetsClean)[9] <- "copdDalys"



#checking missing values per measure
for (i in 1:length(datasetsClean)){
  naMatrix <- as.matrix(is.na.data.frame(datasetsClean[[i]]))
  naProp <- colSums(naMatrix)/nrow(naMatrix)
  plot(naProp,main = names(datasetsClean)[i])
}

#Combining the data 
Data <- data.frame(datasetsClean[[1]],datasetsClean[[2]][,3],
                   datasetsClean[[3]][,3],datasetsClean[[4]][,3],datasetsClean[[5]][,3],
                   datasetsClean[[6]][,3],datasetsClean[[7]][,3],datasetsClean[[8]][,3],
                   datasetsClean[[9]][,3])
colnames(Data)[-c(1,2)] <- names(datasetsClean)
#Visualization again
naDat <- Data
naDat[,-c(1,2)] <- is.na.data.frame(naDat[,-c(1,2)])
plots <- vector("list",9)
for (i in 3:11){
  plot <- ggplot(data = naDat,aes_string(x = "year",y = colnames(naDat)[i],color = "country")) +
    geom_point()
  print(plot)
}
####################
#check countries with too many missing values over too many variables
rowWiseNa <- apply(naDat[,-c(1,2)],1,sum)
missingDataCountries <- unique(naDat[(rowWiseNa / 9) > 0.3,1])
Data <- Data[!(Data$country %in% missingDataCountries),]
###
#remove all rows with nas
noMissData <- Data[]

# clust <- parallel::makePSOCKcluster(detectCores()-1)
# system.time(
#   test <- amelia(Data,p2s = FALSE,m = 100,cs = 1,ts = 2, polytime = 3,
#                  parallel = "snow",c1 = clust)
# )
# parallel::stopCluster(clust)
system.time(
  test <- amelia(Data,p2s = FALSE,m = 100,cs = 1,ts = 2, polytime = 2)
)
tscsPlot(test,cs = "Croatia",var = 9)
par(mfrow= c(2,2))
compare.density(test,var = 9)

#####
data
