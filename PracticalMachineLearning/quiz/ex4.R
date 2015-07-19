set.seed(62433)
mod1 <- train(diagnosis~., data=training, method='rf')
mod2 <- train(diagnosis~., data=training, method='gbm')
mod3 <- train(diagnosis~., data=training, method='lda')

# train combiner
pred1 <- predict(mod1,newdata=training);
pred2 <- predict(mod2,newdata=training)
pred3 <- predict(mod3,newdata=training)
predDF <- data.frame(pred1,pred2,pred3,diagnosis=training$diagnosis)
combModFit <- train(diagnosis~.,method="rf",data=predDF)

# use combiner
pred1 <- predict(mod1,newdata=testing);
pred2 <- predict(mod2,newdata=testing)
pred3 <- predict(mod3,newdata=testing)
predDF <- data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combPred <- predict(combModFit, newdata=predDF)

sum(pred1 == testing$diagnosis) / length(pred1)  # rf
sum(pred2 == testing$diagnosis) / length(pred2)  # gbm
sum(pred3 == testing$diagnosis) / length(pred3)  # lda

sum(combPred == testing$diagnosis) / length(combPred)

