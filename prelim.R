#===================================================
#Initialization borrowed from Gene Leynes @geneorama
#===================================================

## Remove all objects; perform garbage collection
rm(list=ls())
gc(reset=TRUE)

## Check for dependencies
if(!"geneorama" %in% rownames(installed.packages())){
  if(!"devtools" %in% rownames(installed.packages())){
    install.packages('devtools')
  }
  devtools::install_github('geneorama/geneorama')
}

## Load libraries
geneorama::detach_nonstandard_packages()
# geneorama::loadinstall_libraries(c("geneorama", "knitr", "caret", "gbm", 
#     						   "glmnet", "ROCR", "pROC", "plyr", "class", 
# 								   "hmeasure", "randomForest", 
# 								   "AppliedPredictiveModeling", "data.table", 
# 								   "doParallel", "e1071", "rpart"))
geneorama::loadinstall_libraries(c("geneorama", "knitr", "data.table", "ggplot2"))

#===================================================
#load in data
#===================================================
chi <- read.csv(file = "chicago.csv", header = T, stringsAsFactors = F)
chi <- as.data.table(chi)

str(chi)


