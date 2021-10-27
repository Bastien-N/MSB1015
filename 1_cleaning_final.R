#=====================#
#   Bastien Nihant    #
#   MSB1015 project   #
#   Script 1/2        #
#   27-10-2021        #
#=====================#

#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","tidyr","pcaMethods","ggfortify","isotree")
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

#-------------------------------#
#   Missing values management   #
#-------------------------------#

#checking missing values per columns for every measure
for (i in 1:length(datasets)){
  naMatrix <- as.matrix(is.na.data.frame(datasets[[i]]))
  naProp <- colSums(naMatrix)/nrow(naMatrix)
  plot(naProp,main = names(datasets)[i])
}

#Cutting of "useless" columns
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

#checking missing values per columns for every measure
for (i in 1:length(datasets)){
  naMatrix <- as.matrix(is.na.data.frame(datasets[[i]]))
  naProp <- colSums(naMatrix)/nrow(naMatrix)
  plot(naProp,main = names(datasets)[i])
}

#Checking total missing values per measures
naPerDataset <- sapply(datasets, function(d){
  naMatrix <- as.matrix(is.na.data.frame(d))
  nCell <- ncol(naMatrix)*nrow(naMatrix)
  naTot <- sum(colSums(naMatrix))
  naProp <- naTot / nCell
  return(naProp)
})
plotData <- data.frame(names(datasets),naPerDataset)
colnames(plotData) <- c("Dataset","NA_proportion")
ggplot(plotData, aes(Dataset,NA_proportion))+
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust=1))+
  xlab("dataset")+
  ylab("NA(%)")
ggsave("figures/point_data_na.jpeg")

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
print(commonCountries)
# Removing hazardous waste from calculation due to NA values
enoughDataCountriesNoWaste <- enoughDataCountries[c(1:3,5,12:15)]
commonCountriesNoWaste <- Reduce(intersect,enoughDataCountriesNoWaste) #Better than when keeping waste
print(commonCountriesNoWaste)

# Selection of datasets
# Waste data is removed (too many NAs)
# Removing forest for having the wrong format
datasets <- datasets[c(1:3,5,12:15,17)]
#Cutting year 2018 from datasets which have it, as the "Renewable" dataset does not cover it
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
print(commonCountries)

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

#--------------------------------------------------------------#
#   Getting GBD data and adding it to the rest, removing Nas   #
#--------------------------------------------------------------#

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

#Removing all rows with missing values
naDat <- is.na.data.frame(Data)
naDat <- rowSums(naDat) == 0
Data <- Data[naDat,]
#---------------------------------------------#
#   Examining data distribution through PCA   #
#              Transforming data              #
#---------------------------------------------#

pcaDat <- Data[,-c(1,2)]
pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
substring <- substr(Data$country,1,2)
pcaRes <- data.frame(Data, pcaDat$x,substring)
ggplot(pcaRes,aes(PC1,PC2,color = copdDalys))+
  geom_point()
################################
# stopped at line 162 of old cleaning file
#Transforming data into change (year-previous year)
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

# Examining Change data in PCA
pcaDat <- DataChange[,-c(1,2)]
pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
substring <- substr(DataChange$country,1,2)
pcaRes <- data.frame(DataChange, substring)
autoplot(pcaDat,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
         label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)

#Creating complete model data (Y , Y-1)
for (i in 1:length(countries)){
  Y <- DataChange[DataChange$country == countries[i],]
  Y <- Y[-1,]
  YminusOne <- DataChange[DataChange$country == countries[i],]
  YminusOne <- YminusOne[-nrow(YminusOne),-c(1,2)]
  colnames(YminusOne) <- paste0("prev_",colnames(YminusOne))
  
  dataTemp <- cbind(Y,YminusOne)
  if (i == 1){DataY_YminusOne <- dataTemp
  }else {DataY_YminusOne <-  rbind(DataY_YminusOne ,dataTemp)}
}
# Examining complete model data in PCA
pcaDat <- DataY_YminusOne[,-c(1,2)]
pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
substring <- substr(DataY_YminusOne$country,1,2)
pcaRes <- data.frame(DataY_YminusOne, substring)
autoplot(pcaDat,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
         label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)
# Saving complete model data
write.csv(DataY_YminusOne,file = "Data_Y_Y_minus_One.csv",quote = FALSE)

#--------------------------------------------#
#   Isolation forest  to identify outliers   #
#--------------------------------------------#
#Running isolation forest with set seed for reproducability
set.seed(2021)
urf <- isolation.forest(DataY_YminusOne[,-c(1,2)],output_score = TRUE)
# Extracting outlier score and plotting to examine patterns across data structure
out <- urf[['scores']]
plot(out,ylab = "IF outlier score")
names(out) <- 1:length(out)
topOut <- names(sort(out,decreasing = TRUE)[1:15]) #Selecting top 15 outliers
outlying <- rep(FALSE,length(out))
outlying[as.numeric(topOut)] <- TRUE

#Runningand plotting PCA again with outliers in blue
pcaResTemp <- data.frame(pcaRes,outlying)

plot <- autoplot(pcaDat,data = pcaResTemp , label = TRUE,shape = FALSE,label.label = 'substring',
                 label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE,label.colour = 'outlying')
print(plot)

#Assessing place of outliers in the data
dataOut <- data.frame(DataY_YminusOne,outlying)
DataY_YminusOne[outlying,1]
ggplot(data = dataOut[dataOut$country == "Russian Federation",],aes(x = year,y = copdDalys,color = outlying)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggplot(data = dataOut[dataOut$country == "Uzbekistan",],aes(x = year,y = copdDalys,color = outlying)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggplot(data = dataOut[dataOut$country == "Republic of Moldova",],aes(x = year,y = copdDalys,color = outlying)) +
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

#Removing outliers
noOutDataY_YminusOne <- DataY_YminusOne[!outlying,]
write.csv(noOutDataY_YminusOne,file = "noOutData_Y_Y_minus_One.csv",quote = FALSE) #Data without outliers
write.csv(DataY_YminusOne[outlying,],file = "outlying_data.csv",quote = FALSE) #Data with outliers
