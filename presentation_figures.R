
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
naTest <- sapply(datasets,function(d){
  nCell <- nrow(d)*ncol(d)
  na <- sum(rowSums(is.na(d)))
  nap <- na/nCell
  return(nap)
})
naTest <- data.frame(naTest,names(datasets))
colnames(naTest) <- c('na','dat')
ggplot(naTest,aes(dat,na))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("dataset")+
  ylab("NA(%)")
ggsave("point_data_na.jpeg")


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
commonCountries <- c(commonCountries,"Afghanistan")

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

DataRed <- Data[Data$country == "Afghanistan" | Data$country == "Belgium",]
Data <- Data[Data$country != "Afghanistan",]
naDat<- Data
naDat[,3:11]<- is.na.data.frame(naDat[,3:11])
naDat[,12] <- rowSums(naDat[,3:11])

ggplot(naDat,aes(year,V12)) +
  geom_point()+
  ylab("number of NAs")
ggsave("na_plot_year.jpeg")
