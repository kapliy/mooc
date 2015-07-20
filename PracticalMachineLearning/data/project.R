library('caret')   # learning
library('rattle')  # plotting of trees

# parallel computing to utilize multiple cores
library(doParallel)
registerDoParallel(cores=4)

# load the data, interpreting certain strings as NaNs:
nans <- c("NA", "", "#DIV/0!")
pml_training <- read.csv('pml-training.csv', header=T, na.strings=nans)
pml_testing <- read.csv('pml-testing.csv', header=T, na.strings=nans)

# 1. filter out the columns where ALL values are NaNs
cols_all <- colnames(pml_training)
cols_allna <- cols_all[colSums(is.na(pml_training))==nrow(pml_training)]
print(cat('Dropped', length(cols_allna), 'columns (only NA values):', cols_allna))
cols <- cols_all[!(cols_all %in% cols_allna)]

# 2. filter out columns with no variable (these provide little separation power between outcomes)
nzv <- nearZeroVar(pml_training[, cols], saveMetrics = T, allowParallel=T)
cols_novar <- cols[nzv[, 'zeroVar']]
print(cat('Dropped', length(cols_novar), 'columns (zero variance):', cols_novar))
cols <- cols[!(cols %in% cols_novar)]

# 3. drop bad columns from both the training and testing (aka benchmark) datasets
ftrain <- pml_training[, colnames(pml_training) %in% cols]
fbench <- pml_testing[, colnames(pml_testing) %in% c(cols, 'problem_id')]

# 4. separate numeric and factor columns (for later use)
cols_numeric_bool <- sapply(cols, function(col){is.numeric(pml_training[,col])})
cols_numeric <- cols[cols_numeric_bool == T]
cols_factor <- cols[-cols_numeric_bool == F]

########### Visualization ##############
# dependent variables
qplot(ftrain$classe, geom="histogram", main='Distribution of dependent variable', xlab='classe', ylab='Counts')

# Use PCA analysis to extract highest-variance predictors so that we can visualize them
df_pca <- preProcess(ftrain[, cols_numeric], method="pca",thresh=0.99)
# TODO - consider applying PCA: reduce down to 75 vars!

# split full dataset (ftrain) into train/val/test using 60-20-20 fractions
set.seed(1)
p6 <- createDataPartition(ftrain$classe, p=0.6, list=F)
train <- ftrain[p6, ]  # 60%
tv <- ftrain[-p6, ]    # 40%
p5 <- createDataPartition(tv$classe, p=0.5, list=F)
val <- tv[p5, ]       # 0.5*40% = 20%
test <- tv[-p5, ]     # 0.5*40% = 20%


# add pre-processing:
# preProcess=c("center", "scale") 

get_model_01 <- function(save_mode = F) {
    # random forest
    name <- 'model.v01.rf.all.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=train, method='rf',
                     trControl = trainControl(method = "oob"))
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
        mod <- train(classe~., data=train, method='gbm',
                     trControl = trainControl(method = "cv", number=5))
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    mod
}

train_all_models <- function() {
    get_model_01(save_mode=T)
    get_model_02(save_mode=T)
    get_model_03(save_mode=T)
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

fancyRpartPlot(mod2$finalModel)

# evaluate

# combining predictors
#predDF <- data.frame(pred1,pred2,wage=testing$wage)
#combModFit <- train(wage ~.,method="gam",data=predDF)
#combPred <- predict(combModFit,predDF)
#pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
#predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
#combPredV <- predict(combModFit,predVDF)
