#----------------#
#   User input   #
#----------------#
#dat <- "data_change.csv"
dat <- "noOutData_Y_Y_minus_One.csv" #Which version of the cleaned dataset to be used
nRun <- 20 #Number of separate runs
#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","randomForest","caret","dplyr","tidyr","parallel")
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
allData <- read.csv(dat)
allData <- allData[,-1]
#--------------#
#   Analysis   #
#--------------#
set.seed(2021)


#Removing two countries as test group
mainTest <- data[allData$country == "Poland"|allData$country == "Netherlands",]
data <- allData[allData$country != "Poland" & allData$country != "Netherlands",]
#Setting parameter grid
param.mtry <- c(round((ncol(data)-2)/9),
                round((ncol(data)-2)/3),
                round((ncol(data)-2)/1.5))
param.nodesize <- c(3,5,7)
param.maxnodes <- c(0,10,100)
param <- expand.grid(param.mtry,param.nodesize,param.maxnodes)

#Setting the training and test sets for nruns and 
trainSets <- vector("list",nRun)
for (i in 1:nRun){
  trainSets[[i]] <- caret::groupKFold(group = data$country,k = 37)
  names(trainSets[[i]]) <- paste0("Run_",i,"_Fold_",1:length(trainSets[[i]]))
}

#aggregating them for computation efficiency
trainSets <- do.call(c, trainSets)
#Checking countries appear in the testing sets a sufficient number of times
tests <- lapply(trainSets,function(trainSet){
  test <- setdiff(1:nrow(data),trainSet)
  test <- unique(data[test,1])
})
tests <- unlist(tests)
table(tests)

#Aggregating them for computation efficiency
RFdata <- lapply(trainSets,function(trainSet){
  fold <- vector("list",6)
  fold[[1]] <- trainSet
  fold[[2]] <- setdiff(1:nrow(data),trainSet)
  fold[[3]] <- data[-trainSet,-c(1,11)] #xtest
  fold[[4]] <- data[trainSet,-c(1,11)] #xtrain
  fold[[5]] <- data[-trainSet,11] #ytest
  fold[[6]] <- data[trainSet,11] #ytrain
  return(fold)
})
system.time({ 
  coreNum <- parallel::detectCores()
  clust <- parallel::makeCluster(coreNum-1)
  rf <- vector('list',nrow(param))
  for (i in 1:nrow(param)){
    print(paste0("Running over parameter set ",i,"..."))
    mtry <- as.numeric(param[i,1])
    nodesize <- as.numeric(param[i,2])
    if (param[i,3] == 0){maxnodes <- NULL}
    else{maxnodes <- param[i,3]}
    
    
    clusterExport(cl = clust,varlist = c("mtry","nodesize","maxnodes"))
    rf[[i]] <- parLapply(cl = clust,RFdata,function(fold){
      
      xtest <- fold[[3]]
      xtrain <- fold[[4]]
      ytest <- fold[[5]]
      ytrain <- fold[[6]]
      
      res <- randomForest::randomForest(x = xtrain,y = ytrain,
                                        xtest = xtest,ytest = ytest,
                                        mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
                                        keep.forest = FALSE,ntree = 1500)
      return(res)  
    })
    print(paste0("Running over parameter set ",i,"...","DONE"))
    #assign(paste0('rf_',i),rf,pos = .GlobalEnv)
  }
  stopCluster(cl = clust)
})
save(data,mainTest,allData,rf,RFdata,file = "RF_saved_data.RData")
#######################################################
# trainSets <- caret::groupKFold(group = data$country,k = 39)
# #makin
# testSets <- lapply(trainSets,function(trainSet){testSet <- setdiff(1:nrow(data),trainSet)})
# 
# xtest <- lapply(testSets,function(testSet){dat <- data[testSet,-c(1,11)]})
# xtrain <- lapply(trainSets,function(trainSet){dat <- data[trainSet,-c(1,11)]})
# ytest <- lapply(testSets,function(testSet){dat <- data[testSet,11]})
# ytrain <- lapply(trainSets,function(trainSet){dat <- data[trainSet,11]})
# system.time({ 
#   for (i in 1:length(trainSets)){
#     xte <- xtest[[i]]
#     xtr <- xtrain[[i]]
#     yte <- ytest[[i]]
#     ytr <- ytrain[[i]]
#     rf <- lapply(param,function(p){
#       mtry <- p[1,1]
#       nodesize <- p[1,2]
#       if (p[1,3] == 0){maxnodes <- NULL}
#       else{maxnodes <- p[1,3]}
#       
#       res <- randomForest::randomForest(x = xtrain[[i]],y = ytrain[[i]],
#                                         xtest = xtest[[i]],ytest = ytest[[i]],
#                                         mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
#                                         keep.forest = TRUE,ntree = 100)
#       
#     })
#     assign(paste0('rf_',i),rf,pos = .GlobalEnv)
#   }
#   })
# system.time({
#   coreNum <- parallel::detectCores()
#   clust <- parallel::makeCluster(coreNum-1)
#   clusterExport(cl = clust,varlist = c("xtest","xtrain","ytest","ytrain","param"))
#   for (i in 1:length(trainSets)){
#     clusterExport(cl = clust,varlist = c("i"))
#     xte <- xtest[[i]]
#     xtr <- xtrain[[i]]
#     yte <- ytest[[i]]
#     ytr <- ytrain[[i]]
#     rf <- parLapply(cl = clust,X = param,function(p){
#       mtry <- p[1,1]
#       nodesize <- p[1,2]
#       if (p[1,3] == 0){maxnodes <- NULL}
#       else{maxnodes <- p[1,3]}
#       
#       res <- randomForest::randomForest(x = xtrain[[i]],y = ytrain[[i]],
#                                         xtest = xtest[[i]],ytest = ytest[[i]],
#                                         mtry = mtry,nodesize = nodesize,maxnodes = maxnodes,
#                                         keep.forest = TRUE,ntree = 500)
#       
#     })
#     assign(paste0('rf_',i),rf,pos = .GlobalEnv)
#   }
# })
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
############################################################
#Gathering prediction results
cvPredByParam <- lapply(rf,function(pSet){
  dataTemp <- as.data.frame(matrix(rep(0,nrow(data)*(length(rf))),ncol = length(rf)))
  for (i in 1:length(RFdata)){
    samples <- RFdata[[i]][[2]]
    predicted <- pSet[[i]][['test']][['predicted']]
    temp <- rep(NA,nrow(data))
    temp[samples] <- predicted
    dataTemp[,i] <- temp
  }
  colnames(dataTemp) <- names(RFdata)
  return(dataTemp)
})
names(cvPredByParam) <- paste0('p_set_',1:length(rf))
#Calculating means
meanRes <- rowMeans(cvPredByParam[[27]],na.rm = TRUE)
plot(data$copdDalys,meanRes)
cor(data$copdDalys,meanRes)
###########################################################"
# cvPredByParam <- vector('list',length = length(param))
# samples <- 1:nrow(data)
# predictionDf <- data.frame(data[,ncol(data)],row.names = samples)
# for (i in 1:length(cvPredByParam)){
#  for (ii in 1:length(testSets)){
#     testSamples <- testSets[[ii]]
#     predicted <- eval(parse(text = paste0("rf_",ii)))[[i]][['test']][['predicted']]
#     temp <- data.frame(predicted,row.names = testSamples)
#     if (ii ==1){
#       temp2 <- temp
#     }else{
#       temp2 <- merge.data.frame(temp2,temp,by = 0,all = TRUE)
#       row.names(temp2) <- temp2$Row.names
#       temp2 <- temp2[,-1]
#       colnames(temp2) <- 1:ncol(temp2)
#     }
#     
#   }
#   temp3 <-  merge.data.frame(predictionDf,temp2,by = 0,all = TRUE)
#   row.names(temp3) <- temp3$Row.names
#   temp3 <- temp3[,-1]
#   colnames(temp3) <- c('copd',paste0("pred_Fold_",seq(length(testSets))))
#   cvPredByParam[[i]] <- temp3
# }
# m <- rowMeans(cvPredByParam[[1]][,-1],na.rm = TRUE)
# s <- apply(cvPredByParam[[1]][,-1],1,sd,na.rm = TRUE)
# plotData <- data.frame(cvPredByParam[[1]][,1],m,s)
#                        
