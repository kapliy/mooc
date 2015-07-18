library('caret')   # learning
library('rattle')  # plotting of trees

# parallel computing to utilize multiple cores
library(doParallel)
registerDoParallel(cores=4)

bench <- read.csv('pml-testing.csv')
fdata <- read.csv('pml-training.csv')

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

# random forest
set.seed(1)
mod <- train(classe~., data=train, method='rf')
saveRDS(mod, 'model.v01.rf.all.rds')

# decision tree
set.seed(1)
mod <- train(classe~., data=train, method='rpart')
saveRDS(mod, 'model.v02.rpart.all.rds')
# fancyRpartPlot(mod$finalModel)
# qplot(predict(modFit,testing),wage,data=testing)

# boosted decision tree
set.seed(1)
mod <- train(classe~., data=train, method='gbm')
saveRDS(mod, 'model.v03.gbm.all.rds')

# evaluate

# combining predictors
#predDF <- data.frame(pred1,pred2,wage=testing$wage)
#combModFit <- train(wage ~.,method="gam",data=predDF)
#combPred <- predict(combModFit,predDF)
#pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
#predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
#combPredV <- predict(combModFit,predVDF)
