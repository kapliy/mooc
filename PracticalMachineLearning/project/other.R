file_dest_training <- "pml-training.csv"

file_dest_testing <- "pml-testing.csv"

df_training <- read.csv(file_dest_training, na.strings=c("NA",""), header=TRUE)
colnames_train <- colnames(df_training)


df_testing <- read.csv(file_dest_testing, na.strings=c("NA",""), header=TRUE)
colnames_test <- colnames(df_testing)


nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))}

colcnts <- nonNAs(df_training)

drops <- c()

for (cnt in 1:length(colcnts)) {
  if (colcnts[cnt] < nrow(df_training)) {
    drops <- c(drops, colnames_train[cnt])
  }}

df_training <- df_training[,!(names(df_training) %in% drops)]
df_training <- df_training[,8:length(colnames(df_training))]

df_testing <- df_testing[,!(names(df_testing) %in% drops)]
df_testing <- df_testing[,8:length(colnames(df_testing))]



library(caret)
set.seed(12321)

inTest = createDataPartition(y = df_training$classe, p = 0.25, list = FALSE)

Test.Train = df_training[inTest,]

Train.rem = df_training[-inTest,]

set.seed(12321)

inTrain1 = createDataPartition(y = Train.rem$classe, p = 0.33, list = FALSE)

Train1 = Train.rem[inTrain1,]

Train.rem2 = Train.rem[-inTrain1,]

set.seed(12321)
inTrain2 = createDataPartition(y = Train.rem2$classe, p = 0.50, list = FALSE)

Train2 = Train.rem2[inTrain2,]

Train3 = Train.rem2[-inTrain2,]



set.seed(12321)

missClass = function(values, prediction) {
  sum(prediction != values)/length(values)}

modFit1 = train(classe ~ ., method = 'rf', trControl = trainControl(method= 'cv', number = 5), data = Train1, importance = TRUE)



print(modFit1, digits=3)
