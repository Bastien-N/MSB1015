#----------------#
#   User input   #
#----------------#
dat <- "data_change.csv" #Which version of the cleaned dataset to be used
#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","randomForest","caret","dplyr","parallel")
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
param <- split(param,1:nrow(param))
trainSets <- caret::groupKFold(group = data$country,k = 39)
testSets <- lapply(trainSets,function(trainSet){testSet <- setdiff(1:nrow(data),trainSet)})

xtest <- lapply(testSets,function(testSet){dat <- data[testSet,-c(1,11)]})
xtrain <- lapply(trainSets,function(trainSet){dat <- data[trainSet,-c(1,11)]})
ytest <- lapply(testSets,function(testSet){dat <- data[testSet,11]})
ytrain <- lapply(trainSets,function(trainSet){dat <- data[trainSet,11]})
system.time({ 
  for (i in 1:length(trainSets)){
    xte <- xtest[[i]]
    xtr <- xtrain[[i]]
    yte <- ytest[[i]]
    ytr <- ytrain[[i]]
    rf <- lapply(param,function(p){
      mtry <- p[1,1]
      nodesize <- p[1,2]
      if (p[1,3] == 0){maxnodes <- NULL}
      else{maxnodes <- p[1,3]}
      
      res <- randomForest::randomForest(x = xtrain[[i]],y = ytrain[[i]],
                                        xtest = xtest[[i]],ytest = ytest[[i]],
                                        mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
                                        keep.forest = TRUE,ntree = 100)
      
    })
    assign(paste0('rf_',i),rf,pos = .GlobalEnv)
  }
  })
system.time({
  coreNum <- parallel::detectCores()
  clust <- parallel::makeCluster(coreNum-1)
  clusterExport(cl = clust,varlist = c("xtest","xtrain","ytest","ytrain","param"))
  for (i in 1:length(trainSets)){
    clusterExport(cl = clust,varlist = c("i"))
    xte <- xtest[[i]]
    xtr <- xtrain[[i]]
    yte <- ytest[[i]]
    ytr <- ytrain[[i]]
    rf <- parLapply(cl = clust,X = param,function(p){
      mtry <- p[1,1]
      nodesize <- p[1,2]
      if (p[1,3] == 0){maxnodes <- NULL}
      else{maxnodes <- p[1,3]}
      
      res <- randomForest::randomForest(x = xtrain[[i]],y = ytrain[[i]],
                                        xtest = xtest[[i]],ytest = ytest[[i]],
                                        mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
                                        keep.forest = TRUE,ntree = 100)
      
    })
    assign(paste0('rf_',i),rf,pos = .GlobalEnv)
  }
})
# for (i in 1:length(trainSets)){
#   xte <- xtest[[i]]
#   xtr <- xtrain[[i]]
#   yte <- ytest[[i]]
#   ytr <- ytrain[[i]]
#   rf <- lapply(param,function(p){
#     mtry <- p[1,1]
#     nodesize <- p[1,2]
#     if (p[1,3] == 0){maxnodes <- NULL}
#     else{maxnodes <- p[1,3]}
#     
#     res <- randomForest::randomForest(x = xtrain[[i]],y = ytrain[[i]],
#                                       xtest = xtest[[i]],ytest = ytest[[i]],
#                                       mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
#                                       keep.forest = TRUE)
#     
#   })
#   assign(paste0('rf_',i),rf,pos = .GlobalEnv)
# }
# 
# 
# 
# system.time({
# for (i in 1:nrow(param)){
#   mtry <- param[i,1]
#   nodesize <- param[i,2]
#   if (param[i,3] == 0){maxnodes <- NULL}
#   else{maxnodes <- param[i,3]}
# 
#   rf <- lapply(trainSets,function(trainSet){
#     xtest <- data[-trainSet,-c(1,11)]
#     xtrain <- data[trainSet,-c(1,11)]
#     ytest <- data[-trainSet,11]
#     ytrain <- data[trainSet,11]
# 
#     res <- randomForest::randomForest(x = ,y = ytrain,xtest = xtest,ytest = ytest,
#                                       mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
#                                       keep.forest = TRUE,ntree = 100)
# 
#   })
#   assign(paste0('rf_',i),rf,pos = .GlobalEnv)
# }
# })
# trainSets <- caret::groupKFold(group = data$country,k = 15)
# rf <- lapply(trainSets,function(trainSet){
#   test <- data[-trainSet,]
#   train <- data[trainSet,]
#   res <- randomForest::randomForest(x = train[,-c(1,ncol(train))],y = train[,ncol(train)],
#                                     ntree = 10,keep.forest = TRUE)
#   return(res)
#   
# }) 
rfPred <- vector('list',length(rf))
for (i in 1:length(rf)){
  rfPred[[i]] <- predict(rf[[i]],data[-trainSets[[i]],-1])
}
