##############################################
# LIBRARIES
##############################################

library('caret')   # learning
library('rattle')  # plotting of trees
# parallel computing to utilize multiple cores
library(doParallel)
registerDoParallel(cores=2)

##############################################
# DATA LOAD AND PRE-PROCESSING
##############################################

# 0. load the data, interpreting certain values as NaNs:
nans <- c("NA", "", "#DIV/0!")
pml_training <- read.csv('pml-training.csv', header=T, na.strings=nans)
pml_testing <- read.csv('pml-testing.csv', header=T, na.strings=nans)

# 1. A. drop irrelevant columns, such as event metadata, username, and various timestamps
#    B. filter out the columns where ALL values are NaNs
cols_all <- colnames(pml_training)
cols_drop = c('X', 'new_window', 'num_window', 'user_name',
              'raw_timestamp_part_1', 'raw_timestamp_part_2', 'cvtd_timestamp')
print(cat('Dropped', length(cols_drop), 'columns (reason: irrelevant metadata):', cols_drop))
cols_hasna <- cols_all[colSums(is.na(pml_training))>0]
print(cat('Dropped', length(cols_hasna), 'columns (reason: NA values):', cols_hasna))
cols <- cols_all[!(cols_all %in% c(cols_drop,cols_hasna))]

# 2. filter out columns with no variability (these provide little separation power between outcomes)
nzv <- nearZeroVar(pml_training[, cols], saveMetrics = T, allowParallel=T)
cols_novar <- cols[nzv[, 'zeroVar']]
print(cat('Dropped', length(cols_novar), 'columns (reason: zero variance):', cols_novar))
cols <- cols[!(cols %in% cols_novar)]

# 3. drop bad columns from both the training and testing (aka benchmark) datasets
ftrain <- pml_training[, colnames(pml_training) %in% cols]
fbench <- pml_testing[, colnames(pml_testing) %in% c(cols, 'problem_id')]

# 4. separate numeric and factor columns (for later use)
cols_numeric_bool <- sapply(cols, function(col){is.numeric(ftrain[,col])})
cols_numeric <- cols[cols_numeric_bool == T]
cols_factor <- cols[-cols_numeric_bool == F]


##############################################
# DATA VISUALIZATION
##############################################

# 1. Distribution of dependent variables
qplot(ftrain$classe, geom="histogram", main='Distribution of dependent variable', xlab='classe', ylab='Counts')

# Use PCA analysis to extract highest-variance predictors so that we can visualize them
# df_pca <- preProcess(ftrain[, cols_numeric], method="pca",thresh=0.99)
# TODO - consider applying PCA: reduce down to 75 vars!

# split full dataset (ftrain) into train/val/test using 60-20-20 fractions
set.seed(1)
p6 <- createDataPartition(ftrain$classe, p=0.6, list=F)
train <- ftrain[p6, ]  # 60%
tv <- ftrain[-p6, ]    # 40%
p5 <- createDataPartition(tv$classe, p=0.5, list=F)
val <- tv[p5, ]       # 0.5*40% = 20%
test <- tv[-p5, ]     # 0.5*40% = 20%


get_model_01 <- function(save_mode = F) {
    # random forest
    name <- 'model.v01.rf.all.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=train, method='rf', # preProcess=c("center", "scale", "pca"),
                     allowParallel=T, trControl = trainControl(method = "cv", number=10))
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
        mod <- train(classe~., data=train, method='rpart', # preProcess=c("center", "scale", "pca"),
                     allowParallel=T)
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    #fancyRpartPlot(mod$finalModel)
    mod
}

get_model_03 <- function(save_mode = F) {
    # boosted decision tree
    name <- 'model.v03.gbm.all.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=train, method='gbm', # preProcess=c("center", "scale", "pca"),
                     allowParallel=T, trControl = trainControl(method = "cv", number=5))
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

# train_all_models()

mod1 <- get_model_01()


# Out-of-sample error
confusionMatrix(predict(mod1, val), val$classe)

# Prediction
pred <- predict(mod1, newdata=fbench)
print(pred)

# evaluate

# combining predictors
#predDF <- data.frame(pred1,pred2,wage=testing$wage)
#combModFit <- train(wage ~.,method="gam",data=predDF)
#combPred <- predict(combModFit,predDF)
#pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
#predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
#combPredV <- predict(combModFit,predVDF)
