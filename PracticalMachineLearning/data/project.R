##############################################
# LIBRARIES
##############################################

library('caret')   # learning
library('rattle')  # plotting of trees
library('forecast')
# parallel computing to utilize multiple cores
#library(doParallel)
#registerDoParallel(cores=2)

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
df <- pml_training[, colnames(pml_training) %in% cols]
df_bench <- pml_testing[, colnames(pml_testing) %in% c(cols, 'problem_id')]

##############################################
# DATA VISUALIZATION
##############################################

# 1. Distribution of dependent variables
qplot(df$classe, geom="histogram", main='Distribution of dependent variable', xlab='classe', ylab='Counts')

# Use PCA analysis to extract highest-variance predictors so that we can visualize them
# df_pca <- preProcess(ftrain[, cols_numeric], method="pca",thresh=0.99)
# TODO - consider applying PCA: reduce down to 75 vars!

##############################################
# SPLIT DATASET INTO TRAINING/VALIDATION/TESTING
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
# TRAIN SEVERAL MODELS ON TRAINING SET
# NOTE: because training is slow, we create functions
#       that can cache and quickly retrieved a trained
#       model from a .rds file on disk
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

# 5. function to train and save all models to disk
train_all_models <- function() {
    get_mod3(save_mode=T)
    get_mod2(save_mode=T)
    get_mod1(save_mode=T)
}


##############################################
# CHOOSE THE BEST MODEL USING VALIDATION DATASET
##############################################
# train_all_models()

mod1 <- get_mod1()
mod2 <- get_mod2()
mod3 <- get_mod3()


accuracy(predict(mod1, df_val), df_val$classe)

## collect resamples
## results <- resamples(list(LVQ=modelLvq, GBM=modelGbm, SVM=modelSvm))


##############################################
# EVALUATE ACCURACY ON TEST DATASET
##############################################
# train_all_models()

mod1 <- get_model_01()


# Out-of-sample error
confusionMatrix(predict(mod1, val), val$classe)

##############################################
# PREDICT 20 BENCHMARK CASES
##############################################
pred <- predict(mod1, newdata=df_bench)
print(pred)

