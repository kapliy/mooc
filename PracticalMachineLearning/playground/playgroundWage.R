library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage,select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)

featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")


qplot(age, wage, color=jobclass, data=training)
qplot(age, wage, color=education, data=training)

modFit <- train(wage ~ age + jobclass + education, method='lm', data=training)
finMod <- modFit$finalModel

plot(finMod, 1, pch=19, cex=0.5, col='#00000010')

# note that data is needed because we condition on a variable not included in the original model
qplot(finMod$fitted, finMod$residuals, color=race, data=training)

