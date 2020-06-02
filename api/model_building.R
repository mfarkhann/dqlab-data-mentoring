library(caret)
set.seed(26)

# Split the data
sample <- createDataPartition(iris$Species, p=0.80, list=FALSE)

# Create training data
iris_train <- iris[sample,]

# Create test data
iris_test <- iris[-sample,]

control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'


fit.knn <- train(Species~., data=iris_train, method='knn', 
                 trControl=control, metric=metric)


iris_prediction <- predict(fit.knn, iris_test)
confusionMatrix(iris_prediction, iris_test$Species)

saveRDS(fit.knn, here::here('api','data','model_knn.rds'))
