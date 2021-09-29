#----------------#
#   User input   #
#----------------#
dat <- "data_change.csv" #Which version of the cleaned dataset to be used
#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","randomForest","caret")
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

#Training and test set

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
