trainingIL <- training[,grepl("^IL", names(training))]
names(trainingIL)
new_training <- cbind(trainingIL, training$diagnosis)
colnames(new_training)[13] <- "diagnosis"
non_pca_model <- train(diagnosis ~ ., data=new_training, method="glm")

testIL <- testing[,grepl("^IL", names(testing))]
names(testIL)

new_testing <- cbind(testIL, testing$diagnosis)
oolnames(new_testing)[13] <- "diagnosis"
non_pca_result <- confusionMatrix(new_testing[, 13], predict(non_pca_model, new_testing[, -13]))

non_pca_model <- train(diagnosis ~ ., data=new_training, method="glm")

pca_model <- train(diagnosis ~ ., data=new_training, method="glm", preProcess ="pca")

non_pca_result <- confusionMatrix(new_testing[, 13], predict(non_pca_model, new_testing[, -13]))
pca_result <- confusionMatrix(new_testing[, 13], predict(pca_model, new_testing[, -13]))

