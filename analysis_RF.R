#----------------#
#   User input   #
#----------------#
dat <- "data_change.csv" #Which version of the cleaned dataset to be used
#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","randomForest","caret","dplyr")
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

#------------------#
#   Loading data   #
#------------------#
data <- read.csv(dat)
data <- data[,-1]
#--------------#
#   Analysis   #
#--------------#
set.seed(2021)
#Training and test set
param.mtry <- c(1,3,5)
param.nodesize <- c(3,5,7)
param.maxnodes <- c(0,10,100)
param <- expand.grid(param.mtry,param.nodesize,param.maxnodes)
trainSets <- caret::groupKFold(group = data$country,k = 39)
for (i in length(param)){
  mtry <- param[i,1]
  nodesize <- param[i,2]
  if (param[i,3] == 0){maxnodes <- NULL}
  else{maxnodes <- param[i,3]}
  
  rf <- lapply(trainSets,function(trainSet)){
    xtest <- data[-trainSet,-c(1,11)]
    xtrain <- data[trainSet,-c(1,11)]
    ytest <- data[-trainSet,11]
    ytrain <- data[trainSet,11]
    
    res <- randomForest::randomForest(x = xtrain,y = ytrain,xtest = xtest,ytest = ytest,
                                      mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
                                      keep.forest = TRUE)
    
  }
  
}

trainSets <- caret::groupKFold(group = data$country,k = 15)
rf <- lapply(trainSets,function(trainSet){
  test <- data[-trainSet,]
  train <- data[trainSet,]
  res <- randomForest::randomForest(x = train[,-c(1,ncol(train))],y = train[,ncol(train)],
                                    ntree = 10,keep.forest = TRUE)
  return(res)
  
}) 
rfPred <- vector('list',length(rf))
for (i in 1:length(rf)){
  rfPred[[i]] <- predict(rf[[i]],data[-trainSets[[i]],-1])
}
