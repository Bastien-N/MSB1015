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
mainTest <- allData[allData$country == "Poland"|allData$country == "Netherlands",]
data <- allData[allData$country != "Poland" & allData$country != "Netherlands",]

#Verifying that the test set lies within he train set
pcaRes <- prcomp(data[,-c(1,2)],scale. = TRUE,center = TRUE)
loads <- pcaRes$rotation
m <- pcaRes$center
s <- pcaRes$scale
scaledTest <-mainTest[,-c(1,2)]
scaledTest <- apply(scaledTest,1,function(mat){
  mat2<- mat - m
  mat2 <- mat2/s
  return(mat2)
})
pcaTest <-   t(t(loads) %*% scaledTest)
pcaDat <- data.frame(pcaRes$x)
pcaDat <- rbind(pcaDat,pcaTest)
isTest <- rep(FALSE,nrow(pcaDat))
isTest[(nrow(pcaDat)-nrow(pcaTest)):nrow(pcaDat)] <- TRUE
pcaDat <- data.frame(pcaDat,isTest)
ggplot(pcaDat,aes(PC1,PC2,colour = isTest))+
  geom_point()
ggsave("figures/pca_test-train.jpeg")
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

#Gathering prediction results

cvPredByParam <- lapply(rf,function(pSet){
  samples <- vector('list',length(RFdata))
  predicted <- vector('list',length(RFdata))
  real <- vector('list',length(RFdata))
  for (i in 1:length(RFdata)){
    samples[[i]] <- RFdata[[i]][[2]]
    predicted[[i]] <- pSet[[i]][['test']][['predicted']]
    real[[i]] <- RFdata[[i]][[5]]

  }
  samples <- unlist(samples)
  predicted <- unlist(predicted)
  real <- unlist(real)
  dataTemp <- data.frame(samples,predicted,real)
  dataTemp <- dataTemp[order(dataTemp[,1],dataTemp[,2]),]
  return(dataTemp)
})

#Saving results and data to temporary location to keep it safe
save(data,file = "results_temp/data.RData")
save(cvPredByParam, file = "results_temp/cvPredByParam.RData")
save(mainTest,file = "results_temp/mainTest.RData")
save(allData,file = "results_temp/allData.RData")
save(RFdata,file = "results_temp/RFdata.RData")
save(param,file = "results_temp/param.RData")


#---------------------#
#   Result analysis   #
#---------------------#
files <- dir(path = "results_temp/")
for(f in files){
  load(file = paste0("results_temp/",f),envir = .GlobalEnv)
}
#Getting mean prediction
meanPredByParam <- sapply(cvPredByParam,function(cv){
  pred <- rep(NA,nrow(data))
  for (i in 1:nrow(data)){
    m <- mean(cv[cv[,1] == i,2])
    pred[i] <- m
  }
  return(pred)
})
plotData1 <- data.frame(1:nrow(data),data$copdDalys,meanPredByParam)
setNames <- paste0("P",1:27)
colnames(plotData1) <- c("sample","real",setNames)
plotData1 <- tidyr::pivot_longer(plotData1,cols = 3:29,names_to = "Parameter_set")
colnames(plotData1)[4] <- "Predicted"

squaredE <- (plotData1$real - plotData1$Predicted)^2
plotData1[,5] <- squaredE
plotData1[,6] <- 1:nrow(plotData1)
plotData1[,7] <- data[plotData1$sample,1]
colnames(plotData1)[5:7] <- c("SE","index","country")


ggplot(plotData1,aes(country,SE))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("figures/point_country_SE.jpeg")  

#Mean by param
wideData <- pivot_wider(plotData1,names_from = 3,values_from = 5)
MSE <- colMeans(wideData[,-c(1:5)],na.rm = TRUE)
plotData2 <- data.frame(setNames,MSE)
plotData2$setNames <- factor(plotData2$setNames,levels = setNames)
ggplot(plotData2,aes(setNames,MSE)) +
  geom_point() +
  xlab("Parameter set")
ggsave("figures/point_param_mse.jpeg")
#Param set 1 seems good
set.seed(2021)
rf2 <- randomForest(x = data[,-c(1,11)],y = data[,11],mtry = param[1,1],nodesize = param[1,2],ntree = 1500,
                    xtest = mainTest[,-c(1,11)],ytest = mainTest[,11])
res <- data.frame(rownames(mainTest),mainTest$country,rf2[['test']][['predicted']],mainTest$copdDalys)
res[,5] <- (res[,4] - res[,3])^2
colnames(res) <- c('sample','country','predicted','real','SE')

ggplot(res,aes(real,SE,color = country))+
  geom_point()
ggsave("figures/point_country_SE.jpeg")

#aggregating test and train(param 1)

aggRes <- rbind(res[,c(1,2,5,4)],plotData[plotData1$Parameter_set == "P1", c(1,7,5,2)])
aggRes[,5] <- c(rep("Test",sum(aggRes$country == "Poland" | aggRes$country == "Netherlands")),
                rep("Train",sum(aggRes$country != "Poland" & aggRes$country != "Netherlands")))
aggRes[,6] <- sqrt(aggRes$SE) 
colnames(aggRes)[c(5,6)] <- c("Group","absE")

ggplot(aggRes, aes(country,SE,color = Group))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("figures/point_country_SE_test-train.jpeg")
ggplot(aggRes[aggRes$country != "Russian Federation",], aes(country,SE,color = Group))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("figures/NO_RU_point_country_SE_test-train.jpeg")
ggplot(aggRes, aes(real,SE,color = Group))+
  geom_point(alpha = 1,shape = 1)+
  facet_grid(cols = vars(Group))
ggsave("figures/point_real_SE_test-train.jpeg")

ggplot(aggRes,aes(real,absE,color = Group))+
  geom_point(shape = 1)+
  facet_grid(cols = vars(Group))

aggResLim <- aggRes[aggRes$country == "Poland" | aggRes$country == "Netherlands" |
  aggRes$country == "Belgium" | aggRes$country == "Czechia" | aggRes$country == "Germany",]
ggplot(aggResLim, aes(country,SE,color = Group))+
  geom_boxplot()
ggsave("figures/boxplot_country_SE_test-train-lim.jpeg")  

#what about outliers?
out <- read.csv("outlying_data.csv", row.names = 1)
set.seed(2021)
rf3 <- randomForest(x = data[,-c(1,11)],y = data[,11],mtry = param[1,1],nodesize = param[1,2],ntree = 1500,
                           xtest = out[,-c(1,11)],ytest = out[,11])
res2 <- data.frame(rownames(out),out$country,rf3[['test']][['predicted']],out$copdDalys)
res2[,5] <- (res2[,4] - res2[,3])^2
colnames(res2) <- colnames(res)
plotData <- rbind(res,res2)
plotData[,6] <- c(rep("test",nrow(res)),rep("outliers",nrow(res2)))
colnames(plotData)[6] <- "Group"
ggplot(plotData,aes(Group,SE))+
  geom_boxplot()
ggsave("figures/outlier_res.jpeg")
