

library(caret)
library(ggplot2)
library(doParallel)
library(randomForest)
setwd("e:/R/pml-project/")
data <- read.csv("pml-training.csv") # 160 Variables

# Set up parallel processing

set.seed(31313)
inTraining <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTraining,]
testing <- data[-inTraining,]

# Clean data
remove <- nearZeroVar(training)
training <- training[,-remove] # 105 Variables
na.columns <- sapply(training, function(x) any(is.na(x)))
training <- training[,-which(na.columns)] # 59 variables
training <- training[,7:59] # 53 Variables

subTrain <- sample(dim(training)[1], 6000, replace=FALSE)
subTrain <- training[subTrain,]
cl <- makeCluster(detectCores())
registerDoParallel(cl)

Sys.time(); modFit.partial <- train(classe ~ .,data=subTrain,method="rf",prox=TRUE); Sys.time()    
# Sys.time(); modFit <- train(classe ~ .,data=training,method="rf",prox=TRUE); Sys.time()   
stopCluster(cl)

confusionMatrix(testing$classe, predict(modFit.partial, testing))
# Subset 1000, run time 4 minutes, accuracy 90.54%
# Subset 2500, run time 13 minutes, accuracy  95.12%
# Subset 6000, parallel run time 15 minutes, accuracy 98.2%

