library('caret')   # learning
library('rattle')  # plotting of trees

# parallel computing to utilize multiple cores
library(doParallel)
registerDoParallel(cores=4)

bench <- read.csv('pml-testing.csv')
fdata <- read.csv('pml-training.csv')

#
read.csv(..., colClasses=c(...,"character","numeric",...))

# load/save models
#saveRDS(mod, file="mod.v1.rds")
#mod = readRDS("mod.v1.rds")

# split full dataset (fdata) into train/val/test using 60-20-20 fractions
set.seed(1)
p6 <- createDataPartition(fdata$classe, p=0.6, list=F)
train <- fdata[p6, ]  # 60%
tv <- fdata[-p6, ]    # 40%
p5 <- createDataPartition(tv$classe, p=0.5, list=F)
val <- tv[p5, ]       # 0.5*40% = 20%
test <- tv[-p5, ]     # 0.5*40% = 20%

get_model_01 <- function(save_mode = F) {
    # random forest
    name <- 'model.v01.rf.all.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=train, method='rf')
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    mod
}

get_model_02 <- function(save_mode = F) {
    # decision tree
    name <- 'model.v02.rpart.all.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=train, method='rpart')
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    mod
}

get_model_03 <- function(save_mode = F) {
    # boosted decision tree
    name <- 'model.v03.gbm.all.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=train, method='gbm')
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    mod
}

combine_models <- function() {
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
}

plot_tree <- function(mod) {
    fancyRpartPlot(mod$finalModel)
    #qplot(predict(mod,test),wage,data=testing)
    
    #pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
    #qplot(pred1,pred2,colour=wage,data=testing)
}

get_accuracy <- function() {
    library(forecast)
    accuracy(prediction, testing$CompressiveStrength)
    
}

mod1 <- get_model_01()
mod2 <- get_model_02()
mod3 <- get_model_03()


# evaluate

# combining predictors
#predDF <- data.frame(pred1,pred2,wage=testing$wage)
#combModFit <- train(wage ~.,method="gam",data=predDF)
#combPred <- predict(combModFit,predDF)
#pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
#predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
#combPredV <- predict(combModFit,predVDF)
