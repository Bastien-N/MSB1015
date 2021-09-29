#----------------#
#   User input   #
#----------------#
dat <- "data_change.csv" #Which version of the cleaned dataset to be used
#-----------------------#
#   Loading libraries   #
#-----------------------#
packages <- c("ggplot2","randomForest")
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


