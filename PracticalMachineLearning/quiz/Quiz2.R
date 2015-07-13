library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
IL <- names(training)[startsWith(names(training),'IL')]
IL
ex <- training[, c(IL, 'diagnosis')]
mod1 <- train(diagnosis ~ ., data=ex, method='glm')
mod1
confusionMatrix(testing$diagnosis, predict(mod1, newdata=testing))
mod2 <- train(diagnosis ~ ., data=ex, method='glm', preProcess='pca', 
              trControl = trainControl(preProcOptions = list(thresh = 0.8)))
mod2
confusionMatrix(testing$diagnosis, predict(mod2, newdata=testing))
