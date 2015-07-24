##############################################
# LIBRARIES
# First, let's load all required libraries
##############################################

library('caret')     # learning
library('rattle')    # plotting of trees
library('knitr')     # HTML reports
# parallel computing to utilize multiple cores
library('doParallel')
registerDoParallel(cores=4)

##############################################
# DATA LOAD AND PRE-PROCESSING
# here, we load the dataset (both training and 20-sample testing)
# from csv files and drop about half of the columns
# (either because they are metadata unrelated to sensor measurements,
#  or because they contain NaNs)
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
df <- pml_training[, colnames(pml_training) %in% cols]
df_bench <- pml_testing[, colnames(pml_testing) %in% c(cols, 'problem_id')]

##############################################
# DATA VISUALIZATION
# we plot the distribution of dependent variable "classe" 
# (to confirm that we can see all 5 cases in the dataset)
##############################################

qplot(df$classe, geom="histogram", main='Distribution of dependent variable', xlab='classe', ylab='Counts')

##############################################
# SPLIT DATASET INTO TRAINING/VALIDATION/TESTING
# We use:
# 60% - for training to train 3 models
# 20% for cross-validation - to choose the best model
# 20% for final accuracy measurement - using our best model chosen via cross-validation
##############################################

# split full dataset (ftrain) into train/val/test using 60-20-20 fractions
set.seed(1)
p6 <- createDataPartition(df$classe, p=0.6, list=F)
df_train <- df[p6, ]  # 60%
tmp <- df[-p6, ]    # 40%
p5 <- createDataPartition(tmp$classe, p=0.5, list=F)
df_val <- tmp[p5, ]       # 0.5*40% = 20%
df_test <- tmp[-p5, ]     # 0.5*40% = 20%

##############################################
# TRAIN 3 MODELS ON TRAINING SET
# NOTE: because training is slow (~30 minutes), we create 3 functions
#       that can cache and quickly retrieve a trained
#       model from a .rds file on disk
# The models we train are:
# 1. random forest ("rf") without pre-processing
# 2. random forest ("rf") with center-scale-PCA preprocessing pipeline
# 3. gradient-boosted decision tree ("gbm")
##############################################

# 1. rf (random forest) without extra pre-processing
get_mod1 <- function(save_mode = F) {
    name <- 'model.v01.rf.rds'
    if (save_mode == T) {
        set.seed(1)
        mod <- train(classe~., data=df_train, method='rf',
                     trControl=trainControl(method="cv", number=10))
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    mod
}

# 2. rf (random forest) with center-scale-PCA preprocessing
get_mod2 <- function(save_mode = F) {
  name <- 'model.v02.rf_pca.rds'
  if (save_mode == T) {
    set.seed(1)
    mod <- train(classe~., data=df_train, method='rf',
                 preProcess=c("center", "scale", "pca"),
                 trControl=trainControl(method="cv", number=10))
    saveRDS(mod, name)
  } else {
    mod <- readRDS(name)
  }
  mod
}

# 3. gbm (boosted decision tree)
get_mod3 <- function(save_mode = F) {
    name <- 'model.v03.gbm.rds'
    if (save_mode == T) {
        set.seed(10)
        mod <- train(classe~., data=df_train, method='gbm', verbose=F)
        saveRDS(mod, name)
    } else {
        mod <- readRDS(name)
    }
    mod
}

# 4. this function can be used to train and save all 3 models to disk
train_all_models <- function() {
    get_mod3(save_mode=T)
    get_mod2(save_mode=T)
    get_mod1(save_mode=T)
}


##############################################
# TRAIN 3 MODELS AND CHOOSE THE BEST MODEL USING VALIDATION DATASET
# Basically, we train the 3 models and apply them to validation dataset
# We then evaluate accuracy on all 3 models and choose the best one
# (the one with the highest accuracy)
##############################################

# the line below can be commented out if the models have already been trained
# (and saved inside rds files)
# WARNING: training the models is slow and can take 30-60 minutes!!!
# train_all_models()

# load models from the .rds files
mod1 <- get_mod1(save_mode=F)
mod2 <- get_mod2(save_mode=F)
mod3 <- get_mod3(save_mode=F)

# computes prediction accuracy on model "mod" using test data "df"
accuracy <- function(mod, df) {
    res <- sum(predict(mod, df) == df$classe) / length(df$classe)
    res
}

# print accuracy for all 3 models using the validation dataset
print(paste("Model 1 validation accuracy", accuracy(mod1, df_val)))
print(paste("Model 2 validation accuracy", accuracy(mod2, df_val)))
print(paste("Model 3 validation accuracy", accuracy(mod3, df_val)))

# We saw that modell "mod1" has the best accuracy on the validation set.
# Therefore, we select it as the "final" model that will be used
# to evaluate accuracy on the test set
mod <- mod1

##############################################
# EVALUATE ACCURACY ON TEST DATASET
# this is the final accuracy that we report for the assignment
##############################################

# note that we are using the best model (chosen via the validation dataset)
print(paste("Final accuracy on test set", accuracy(mod, df_test)))

# Full details from the confusion matrix:
confusionMatrix(predict(mod, df_test), df_test$classe)

# FINAL ACCURACY = 99.39% - not bad!

##############################################
# PREDICT 20 BENCHMARK CASES
# Here, we apply the final model to 20 test cases in the assignment
# and generate text files that can be used to submit them for grading
# on the Coursera website
# All 20 cases passed Coursera's grader with 100% accuracy.
##############################################
pred <- predict(mod, newdata=df_bench)
print(pred)

# create submission files in a subfolder (to upload to Coursera)
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
setwd('results')
pml_write_files(pred)
setwd('../')

