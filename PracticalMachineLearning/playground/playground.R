library(caret)
data(faithful)
dim(faithful)


inTrain <- createDataPartition(faithful$waiting, p=0.5, list=F)
training <- faithful[inTrain,]
testing <- faithful[-inTrain,]

# train in R
lm1 <- lm(eruptions ~ ., data=training)
summary(lm1)

# train in caret
modelFit <- train(eruptions ~ waiting, method='lm', data=training)
summary(modelFit)

# plotting
par(mfrow=c(1,2))
plot(training$waiting, training$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(training$waiting, predict(lm1), lwd=3)
plot(testing$waiting, testing$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(testing$waiting, predict(lm1, newdata=testing), lwd=3)

# rmse
sqrt(sum((lm1$fitted - training$eruptions)^2))
# rmse for test set
sqrt(sum((predict(lm1, newdata=testing) - testing$eruptions)^2))

# plot prediction interval
par(mfrow=c(1,1))
pred1 <- predict(lm1, newdata=testing, interval="prediction")
ord <- order(testing$waiting)
plot(testing$waiting, testing$eruptions, pch=19, col="blue")
matlines(testing$waiting[ord], pred1[ord,], type="l",,col=c(1,2,2), lty=c(1,1,1), lwd=3)
