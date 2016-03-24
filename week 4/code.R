library(caret)
library(rpart)
library(randomForest)

##Read Train Data
trainingDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainingData <- read.csv(trainingDataURL)
dim(trainingData) ##19622   160


##Read Test Data
testingDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testingData <- read.csv(testingDataURL)
dim(testingData) ##20 160

##Remove Missing Values
trainingDataMS <- trainingData[,colSums(is.na(trainingData))==0]
testingDataMS <- testingData[,colSums(is.na(trainingData))==0]

##Remove variables
trainRemove <- grepl("problem_id|^X|_x$|_y$|_z$", colnames(trainingDataMS))
testRemove <- grepl("problem_id|^X|_x$|_y$|_z$", colnames(testingDataMS))
cleanTrainData <- trainingDataMS[ , !trainRemove]
cleanTestData <- testingDataMS[ , !testRemove]
classe <- cleanTrainData$classe
testCleaned <- cleanTestData[, sapply(cleanTestData, is.numeric)]
trainCleaned <- cleanTrainData[, sapply(cleanTrainData, is.numeric)]
trainCleaned$classe <- classe

##Split the data
set.seed(22519)
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]


##Build model
controlRf <- trainControl(method="cv", 5)
modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
predictRf <- predict(modelRf, testData)
confusionMatrix(cleanTestData$classe, predictRf)

##Predict on test
result <- predict(modelRf, testCleaned)
result