
#----------------#
#   User input   #
#----------------#

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
pcaDat <- Data[,-c(1,2)]
pcaDat <- pca(pcaDat,nPcs = 6,scale = "pareto")
substring <- substr(Data$country,1,2)
pcaRes <- data.frame(Data[,c(1,2)],pcaDat@scores,substring)

ggplot(pcaRes,aes(x = PC1,y = PC2,color = year)) +
  geom_text(aes(label = substring)) 
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
# pcaDat <- DataChange[,-c(1,2)]
# pcaDat <- pca(pcaDat,nPcs = 6,scale = "pareto")
# substring <- substr(DataChange$country,1,2)
# pcaRes <- data.frame(DataChange,pcaDat@scores, substring)
# plot(pcaDat@loadings)
# ggplot(pcaRes,aes(x = PC1,y = PC2,color = copdDalys)) +
#   geom_text(aes(label = substring)) +
#   geom_segment(data=pcaD, aes(x=0, y=0, xend=PC1, yend=PC2)
#                , arrow=arrow(length=unit(0.2,"cm")), alpha=0.25)
# ggbiplot(as.data.frame(pcaDat@scores),as.data.frame(pcaDat@loadings))

pcaDat <- DataChange[,-c(1,2)]
pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
substring <- substr(DataChange$country,1,2)
pcaRes <- data.frame(DataChange, substring)
autoplot(pcaDat,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
         label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)

######################################"

####################################""
# #Removing samples with the two smallest PC1
# PC1 <- pcaDat$x[,'PC1']
# smallestTwo <- sort(PC1)[c(1,2)]
# DataChangeTemp <- data.frame(DataChange,PC1 %in% smallestTwo)
# colnames(DataChangeTemp)[12] <- 'Outlier'
# ggplot(DataChangeTemp[DataChangeTemp$country=="Russian Federation",],
#        aes(year,GHG,colour = Outlier))+
#   geom_point()+
#   theme(axis.text.x = element_text(angle = 90))
# 
# DataChange <- DataChange[!(PC1 %in% smallestTwo),]
# 
# 
# pcaDat <- DataChange[,-c(1,2)]
# pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
# substring <- substr(DataChange$country,1,2)
# pcaRes <- data.frame(DataChange, substring)
# autoplot(pcaDat,x = 1,y = 2,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
#          label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)
# 
# 
# #Removing samples with the two largest PC1
# PC1 <- pcaDat$x[,'PC1']
# largestTwo <- sort(PC1, decreasing = TRUE)[c(1,2)]
# DataChangeTemp <- data.frame(DataChange,PC1 %in% largestTwo)
# colnames(DataChangeTemp)[12] <- 'Outlier'
# ggplot(DataChangeTemp[DataChangeTemp$country=="Russian Federation",],
#        aes(year,CO2,colour = Outlier))+
#   geom_point()+
#   theme(axis.text.x = element_text(angle = 90))

#DataChange <- DataChange[!(PC1 %in% largestTwo),]



# 
# #min-max Scaling
# DataChange[,-c(1,2)] <- apply(DataChange[,-c(1,2)],2,function(x){
#    (x - min(x))/(max(x)-min(x))
# })
# 
# pcaDat <- DataChange[,-c(1,2)]
# pcaDat <- pca(pcaDat,nPcs = 6,scale = "none")
# substring <- substr(DataChange$country,1,2)
# pcaRes <- data.frame(DataChange[,c(1,2)],pcaDat@scores,substring)
# 
# ggplot(pcaRes,aes(x = PC1,y = PC2,color = country)) +
#   geom_text(aes(label = substring)) 
# #Exploring relationship
# for (var in colnames(DataChange)[-c(1,2,11)]){
#   plot(DataChange[,var],DataChange$copdDalys,xlab = var,ylab = "COPD Dalys")
# }
# 
# #creating shifted data
# dataShifted <- vector(mode = "list",5)
# shifts <- 1:5
# for (i in 1:length(shifts)){
#   for (ii in 1:length(countries)){
#     dataTemp <- DataChange[DataChange$country == countries[ii],]
#     predTemp <- dataTemp[-((nrow(dataTemp)-shifts[i]+1):nrow(dataTemp)),-ncol(dataTemp)]
#     critTemp <- dataTemp[-(1:shifts[i]),ncol(dataTemp)]
#     dataTemp <- data.frame(predTemp,critTemp)
#     if (ii == 1){
#       dataTemp2 <- dataTemp
#     }
#     else{
#       dataTemp2 <- rbind(dataTemp2,dataTemp)
#     }
#   }
#   dataShifted[[i]] <- dataTemp2
#   names(dataShifted)[i] <- paste0("YearMinus",shifts[i])
#   colnames(dataShifted[[i]])[11] <- "copdDalys"
# }
# for (var in colnames(dataShifted[[i]])[-c(1,2,11)]){
#   for (i in 1:length(dataShifted)){
#   
#     plot(dataShifted[[i]][,var],dataShifted[[i]]$copdDalys,
#          main = names(dataShifted)[i],xlab = var,ylab = "COPD Dalys")
#   }
# }
# 
# ##Aggregating years:
# #Per three years
# dataTemp <- dataTemp2 <- dataTemp3 <- DataAggThree<- NULL
# for (i in 1:length(countries)){
#   
#   dataTemp <- DataChange[DataChange$country == countries[i],]
#   years <- unique(dataTemp$year)
#   for (ii in 1:9){
#     yearsTemp <- years[(1+(ii-1)*3):(ii*3)]
#     dataTemp2 <- dataTemp[dataTemp$year %in% yearsTemp,]
#     
#     if (nrow(dataTemp2) == 3){
# 
#       dataTemp2 <- data.frame(dataTemp2[1,1],
#                               paste0(substr(yearsTemp[1],1,4),'_to_',substr(yearsTemp[3],1,4)),
#                                      t(colSums(dataTemp2[,-c(1,2)])))
#       colnames(dataTemp2) <- colnames(DataChange)
#       if (ii == 1){
#         dataTemp3 <- dataTemp2
#       }
#       else{
#         dataTemp3 <- rbind(dataTemp3,dataTemp2)
#       }
#     }
# 
#   }
#   if (i == 1){DataAggThree <- dataTemp3}
#   else{DataAggThree <- rbind(DataAggThree,dataTemp3)}
# }
# 
# #PCA
# pcaDat <- DataAggThree[,-c(1,2)]
# pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
# substring <- substr(DataAggThree$country,1,2)
# pcaRes <- data.frame(DataAggThree, substring)
# autoplot(pcaDat,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
#          label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)
# #Exploring relationship
# for (var in colnames(DataAggThree)[-c(1,2,11)]){
#   plot(DataAggThree[,var],DataAggThree$copdDalys,xlab = var,ylab = "COPD Dalys")
# }
# ##Aggregating years:
# #Per nine years
# dataTemp <- dataTemp2 <- dataTemp3 <- DataAggNine<- NULL
# for (i in 1:length(countries)){
#   
#   dataTemp <- DataChange[DataChange$country == countries[i],]
#   years <- unique(dataTemp$year)
#   for (ii in 1:3){
#     yearsTemp <- years[(1+(ii-1)*9):(ii*9)]
#     dataTemp2 <- dataTemp[dataTemp$year %in% yearsTemp,]
#     
#     if (nrow(dataTemp2) == 9){
#       
#       dataTemp2 <- data.frame(dataTemp2[1,1],
#                               paste0(substr(yearsTemp[1],1,4),'_to_',substr(yearsTemp[3],1,4)),
#                               t(colSums(dataTemp2[,-c(1,2)])))
#       colnames(dataTemp2) <- colnames(DataChange)
#       if (ii == 1){
#         dataTemp3 <- dataTemp2
#       }
#       else{
#         dataTemp3 <- rbind(dataTemp3,dataTemp2)
#       }
#     }
#     
#   }
#   if (i == 1){DataAggNine <- dataTemp3}
#   else{DataAggNine <- rbind(DataAggNine,dataTemp3)}
# }
# 
# #PCA
# pcaDat <- DataAggNine[,-c(1,2)]
# pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
# substring <- substr(DataAggNine$country,1,2)
# pcaRes <- data.frame(DataAggNine, substring)
# autoplot(pcaDat,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
#          label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)
# #Exploring relationship
# for (var in colnames(DataAggNine)[-c(1,2,11)]){
#   plot(DataAggNine[,var],DataAggNine$copdDalys,xlab = var,ylab = "COPD Dalys")
# }
# 

#Creating complete shifted model Y + Y-1
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
pcaDat <- DataY_YminusOne[,-c(1,2)]
pcaDat <- prcomp(pcaDat, scale. = TRUE,center = TRUE)
substring <- substr(DataY_YminusOne$country,1,2)
pcaRes <- data.frame(DataY_YminusOne, substring)
autoplot(pcaDat,data = pcaRes , label = TRUE,shape = FALSE,label.label = 'substring',
         label.alpha = 0.6,loadings = TRUE,loadings.label = TRUE)
write.csv(DataY_YminusOne,file = "Data_Y_Y_minus_One.csv",quote = FALSE)

#
urf <- isolation.forest(DataY_YminusOne[,-c(1,2)],output_score = TRUE)
plot(urf[['scores']])
out <- urf[['scores']]
names(out) <- 1:length(out)
topOut <- names(sort(out,decreasing = TRUE)[1:10])
outlying <- rep(FALSE,length(out))
outlying[as.numeric(topOut)] <- TRUE
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

#Removing outliers
noOutDataY_YminusOne <- DataY_YminusOne[outlying,]
write.csv(noOutDataY_YminusOne,file = "noOutData_Y_Y_minus_One.csv",quote = FALSE)
