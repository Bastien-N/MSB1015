#=====================#
#   Bastien Nihant    #
#   MSB1015 project   #
#   Script 2/2        #
#   27-10-2021        #
#=====================#
#Note, it is important that the working directory contains output directories named 
#"figures" and "results_temp" 

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

#----------------------#
#   Data preparation   #
#----------------------#
set.seed(2021)

#Removing two countries as test group
mainTest <- allData[allData$country == "Poland"|allData$country == "Netherlands",]
data <- allData[allData$country != "Poland" & allData$country != "Netherlands",]

#Verifying that the test set lies within he train set
#PCA on the training set
pcaRes <- prcomp(data[,-c(1,2)],scale. = TRUE,center = TRUE)
loads <- pcaRes$rotation
m <- pcaRes$center
s <- pcaRes$scale
#Scaling test set accorting to training set
scaledTest <-mainTest[,-c(1,2)]
scaledTest <- apply(scaledTest,1,function(mat){
  mat2<- mat - m
  mat2 <- mat2/s
  return(mat2)
})
#PCA transformation of test set
pcaTest <-   t(t(loads) %*% scaledTest)
pcaDat <- data.frame(pcaRes$x)
pcaDat <- rbind(pcaDat,pcaTest)
isTest <- rep(FALSE,nrow(pcaDat))
isTest[(nrow(pcaDat)-nrow(pcaTest)):nrow(pcaDat)] <- TRUE
pcaDat <- data.frame(pcaDat,isTest)
#Plotting
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

#-----------------------------------------------------------------------#
#   Random forest Cross-validation (every parameter set for all runs)   #
#-----------------------------------------------------------------------#
# ! Can take several hours, skip to line 178 if you wish to avoid that
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
#and avoid having to re-run the long analysis
save(data,file = "results_temp/data.RData")
save(cvPredByParam, file = "results_temp/cvPredByParam.RData")
save(mainTest,file = "results_temp/mainTest.RData")
save(allData,file = "results_temp/allData.RData")
save(RFdata,file = "results_temp/RFdata.RData")
save(param,file = "results_temp/param.RData")


#---------------------------------------#
#   Cross validation results analysis   #
#---------------------------------------#
#Loading backed-up intermediate data
files <- dir(path = "results_temp/")
for(f in files){
  load(file = paste0("results_temp/",f),envir = .GlobalEnv)
}
#Getting mean prediction by sample for each parameter set
meanPredByParam <- sapply(cvPredByParam,function(cv){
  pred <- rep(NA,nrow(data))
  for (i in 1:nrow(data)){
    m <- mean(cv[cv[,1] == i,2])
    pred[i] <- m
  }
  return(pred)
})

#Preparing data for result plots
plotData1 <- data.frame(1:nrow(data),data$copdDalys,meanPredByParam)
setNames <- paste0("P",1:27)
colnames(plotData1) <- c("sample","real",setNames)
plotData1 <- tidyr::pivot_longer(plotData1,cols = 3:29,names_to = "Parameter_set")
colnames(plotData1)[4] <- "Predicted"

squaredE <- (plotData1$real - plotData1$Predicted)^2 #Calculating squared error
plotData1[,5] <- squaredE
plotData1[,6] <- 1:nrow(plotData1)
plotData1[,7] <- data[plotData1$sample,1]
colnames(plotData1)[5:7] <- c("SqE","index","country")

#Plotting those squared errors by country
ggplot(plotData1,aes(country,SqE))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90,vjust = 0.3))
ggsave("figures/point_country_SE.jpeg")  

#Reorganizing to examine squared error by parameter set
wideData <- pivot_wider(plotData1,names_from = 3,values_from = 5)
MSE <- colMeans(wideData[,-c(1:5)],na.rm = TRUE)
plotData2 <- data.frame(setNames,MSE)
plotData2$setNames <- factor(plotData2$setNames,levels = setNames)
ggplot(plotData2,aes(MSE,setNames)) +
  geom_point() +
  ylab("Parameter set")
ggsave("figures/point_param_mse.jpeg")
# ==> Parameter set 1 has the lowest MSE and is therefore chosen

#--------------------------------------------------------------#
#   Running RF on complete training set with parameter set 1   #
#--------------------------------------------------------------#
#Random forest
set.seed(2021)
rf2 <- randomForest(x = data[,-c(1,11)],y = data[,11],mtry = param[1,1],nodesize = param[1,2],ntree = 1500,
                    xtest = mainTest[,-c(1,11)],ytest = mainTest[,11])
res <- data.frame(rownames(mainTest),mainTest$country,rf2[['test']][['predicted']],mainTest$copdDalys)
res[,5] <- (res[,4] - res[,3])^2 #Calculating SqE
colnames(res) <- c('sample','country','predicted','real','SqE')

#Plotting results (real copdDalys values vs predicted values and vs SqE)
ggplot(res,aes(real,predicted,color = country))+
  geom_point()
ggsave("figures/point_real_pred.jpeg")
ggplot(res,aes(real,SqE,color = country))+
  geom_point()
ggsave("figures/point_real_SqE.jpeg")

#Getting out of bag (oob) predictions of training set and calculating SqE
oobRes <- data.frame(1:nrow(data),data[,1],rf2[['predicted']],data[,11])
oobRes[,5] <- (oobRes[,4] - oobRes[,3])^2
colnames(oobRes) <- c('sample','country','predicted','real','SqE')

#aggregating test predictions results and oob predictions results
aggRes <- rbind(res,oobRes)
aggRes[,6] <- c(rep("Test",sum(aggRes$country == "Poland" | aggRes$country == "Netherlands")),
                rep("Train",sum(aggRes$country != "Poland" & aggRes$country != "Netherlands")))
aggRes[,7] <- sqrt(aggRes$SqE) #Getting absolute value of error
colnames(aggRes)[c(6,7)] <- c("Group","absE")

#Plotting by country
ggplot(aggRes, aes(country,SqE,color = Group))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("figures/point_country_SE_test-train.jpeg")
#Excluding Russia to prevent stretched scale
ggplot(aggRes[aggRes$country != "Russian Federation",], aes(country,SqE,color = Group))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
ggsave("figures/NO_RU_point_country_SE_test-train.jpeg")
#Plotting real and predicted values for the test and training sets
#With black identity line for reference
ggplot(aggRes, aes(real,predicted,color = Group))+
  geom_point(alpha = 1,shape = 1)+
  facet_grid(cols = vars(Group)) +
  geom_abline(slope = 1)
ggsave("figures/point_real_pred_test-train.jpeg")
#Plotting real values and squared errors for the test and training sets
ggplot(aggRes, aes(real,SqE,color = Group))+
  geom_point(alpha = 1,shape = 1)+
  facet_grid(cols = vars(Group))
ggsave("figures/point_real_SE_test-train.jpeg")

#Plotting real values and absolute errors for the test and training sets
#With black identity and reversed identity lines
ggplot(aggRes,aes(real,absE,color = Group))+
  geom_point(shape = 1)+
  facet_grid(cols = vars(Group))+
  geom_abline(slope = 1)+
  geom_abline(slope = -1)

#Boxplots/violin plot of squared error for the test countries and a few geographical neighbors
aggResLim <- aggRes[aggRes$country == "Poland" | aggRes$country == "Netherlands" |
  aggRes$country == "Belgium" | aggRes$country == "Czechia" | aggRes$country == "Germany",]
ggplot(aggResLim, aes(country,SqE,color = Group))+
  geom_violin()+
  geom_boxplot(fill = NA)
ggsave("figures/boxplot_country_SE_test-train-lim.jpeg")  

#----------------------------------------------------------------------------------------#
#   Attempting to predict in outliers to check if they perform as well as the test data  #
#----------------------------------------------------------------------------------------#
#Loading data
out <- read.csv("outlying_data.csv", row.names = 1)
#Running RF with outliers as test
set.seed(2021)
rf3 <- randomForest(x = data[,-c(1,11)],y = data[,11],mtry = param[1,1],nodesize = param[1,2],ntree = 1500,
                           xtest = out[,-c(1,11)],ytest = out[,11])
#Examining results
res2 <- data.frame(rownames(out),out$country,rf3[['test']][['predicted']],out$copdDalys)
res2[,5] <- (res2[,4] - res2[,3])^2
colnames(res2) <- colnames(res)
plotData <- rbind(res,res2)
plotData[,6] <- c(rep("test",nrow(res)),rep("outliers",nrow(res2)))
colnames(plotData)[6] <- "Group"
#Plotting
ggplot(plotData,aes(Group,SqE))+
  geom_boxplot()
ggsave("figures/outlier_res.jpeg")
# ==> Outliers are extremely poorly predicted

#------------------------------------------------------------------------#
#   Attempting to predict in test after training with ouliers included   #
#   to check if they were rightfully removed                             #
#------------------------------------------------------------------------#
#Running RF with outliers included in training set
set.seed(2021)
rf4 <- randomForest(x = rbind(data[,-c(1,11)],out[,-c(1,11)]),
                    y = c(data[,11],out[,11]),mtry = param[1,1],
                    nodesize = param[1,2],ntree = 1500,
                    xtest = mainTest[,-c(1,11)],ytest = mainTest[,11])
#Examining results
res3 <- data.frame(rownames(mainTest),mainTest$country,rf4[['test']][['predicted']],mainTest$copdDalys)
res3[,5] <- (res3[,4] - res3[,3])^2
colnames(res3) <- colnames(res)
plotData <- rbind(res,res3)
plotData[,6] <- c(rep("no-Outliers",nrow(res)),rep("Outliers",nrow(res3)))
colnames(plotData)[6] <- "Group"
#Plotting
ggplot(plotData,aes(Group,SqE))+
  geom_boxplot()
ggsave("figures/outlier_inTrain_res.jpeg")
# ==> Accuracy appears as good, if not marginally better, without removing outliers.