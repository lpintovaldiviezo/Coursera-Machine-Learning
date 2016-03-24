install.packages("AppliedPredictiveModeling")
install.packages("caret")
install.packages("ElemStatLearn")
install.packages("pgmm")
install.packages("rpart")

##Q1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
trainData <- segmentationOriginal[segmentationOriginal$Case == "Train", ]
testData <- segmentationOriginal[segmentationOriginal$Case == "Test", ]
set.seed(125)
model <- train(Class ~ ., method = "rpart", data = trainData)
library(rattle)
library(rpart.plot)
fancyRpartPlot(model$finalModel)

##Q2
library(pgmm)
data(olive)
olive = olive[,-1]
model <- train(Area ~ ., method = "rpart", data = olive)
library(rpart.plot)
fancyRpartPlot(model$finalModel)
newdata = as.data.frame(t(colMeans(olive)))
predict(model, newdata = newdata)

##Q3
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)


modelSA <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,  data = trainSA, method = "glm", family = "binomial")
missClass(testSA$chd, predict(modelSA, newdata = testSA))