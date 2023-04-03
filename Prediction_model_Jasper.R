#import packages
list.of.packages <- c('magrittr', 'ggplot2', 'dplyr', 'caret', 'rms', 'mice')
lapply(list.of.packages, require, character.only=T)

#load data
data <- read.delim("ovarian.txt")

#check for missing variables
mice::md.pattern(data)

summary(data)

#change to appropriate datatype
data$Ascites %<>% as.factor()
data$papflow %<>% as.factor()
data$wallreg %<>% as.factor()
data$Shadows %<>% as.factor()
data$pershistovca %<>% as.factor()
data$famhistovca <- as.factor(ifelse(data$famhistovca == 0, 0, ifelse(data$famhistovca == 1, 1, 2)))
data$hormtherapy %<>% as.factor()
data$pain %<>% as.factor()
data$colscore %<>% as.factor()
data$nrloculescat %<>% as.factor()
data$papnr %<>% as.factor()
data$oncocenter %<>% as.factor()
data$mal %<>% factor(., levels = c(0,1), labels = c("Benign", "Malign"))

#remove CA125
data %<>% select(., -CA125)

#sample test dataset
n <- sample(1:nrow(data), size=1000, replace = T)

data_test <- data[n,]
test_labels <- data_test$mal
test_labels


#set seed
set.seed(23)

#Random Forest
trainControl <- caret::trainControl(
  method = "repeatedcv", #repeated internal cross-validation
  number = 10, #10-fold cross-validation
  repeats = 10, #10 repeats of each fold
  #search = "random",
  summaryFunction = twoClassSummary,
  classProbs=TRUE,
  allowParallel=TRUE,
  savePredictions = T
)

tgControl <- expand.grid(
  mtry = 4 #**HYPERPARAMETER sqrt of 15 variables!
  #num.trees = c(1000, 1500, 2000)
)

tgControl

model_forest <- train(
  mal ~ . , 
  data = data,
  method = "rf",
  trControl = trainControl,
  verbose = T,
  #tuneLength = 5
  metric = "ROC",
  tuneGrid = tgControl,
  num.trees = 10 #**HYPERPARAMETER,
)

model_forest

library(MLeval)
x <- MLeval::evalm(model_forest, plots = T)

x

#Variable Importance
plot(caret::varImp(model_forest))
