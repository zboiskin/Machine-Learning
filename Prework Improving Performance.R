#Improving Performance 
library(tidyverse)
library(caret)
library(DMwR)
library(rpart)
library(xgboost)
library(ROCR)

#1. Collect and prepare data
income <-
  read_csv("https://s3.amazonaws.com/notredame.analytics.data/income.csv", col_types = "nffnfffffnff")

glimpse(income)

income <- income %>%
  mutate(income = as.factor(dplyr::recode(income, "<=50K" = "Below", ">50K" = "Above")))

table(income$income)

#splitting data
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- createDataPartition(y = income$income, p = 0.75, list = FALSE)
income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

#balancing 
RNGkind(sample.kind = "Rounding")
set.seed(1234)
income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#2. Evaluating the model
#creating decision tree training

RNGkind(sample.kind = "Rounding")
set.seed(1234)
tree_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart"
)

tree_mod

#creating model prediction output
tree_pred <- predict(tree_mod, income_test)

#confusion matric
confusionMatrix(tree_pred, income_test$income, positive = "Below")

#viewing results
head(predict(tree_mod, income_test, type = "raw"))
head(predict(tree_mod, income_test, type = "prob"))

#3. Hyperparameter Tuning
RNGkind(sample.kind = "Rounding")
set.seed(1234)

tree_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 3)
)

tree_mod

#tweaking the number of hyperparameters to consider
RNGkind(sample.kind = "Rounding")
set.seed(1234)

tree_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 3),
  tuneLength = 10
)

tree_mod

#grid search process to see which parameters should be specified to use
modelLookup("rpart")
modelLookup("C5.0")

#create C5.0 grid of tuning parameters
grid <-
  expand.grid(
    model = "tree",
    trials = c(1, 5, 10, 15, 20, 25, 30, 35),
    winnow = FALSE
  )
grid

#grid for rpart
grid <- 
  expand.grid(
    cp = seq(from=0.0001, to=0.005, by=0.0001)
  )
grid

#having the training data include these new hyperparameters
RNGkind(sample.kind = "Rounding")
set.seed(1234)
tree_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart",
  trControl = trainControl(method = "cv", number = 3),
  tuneGrid = grid
)
tree_mod

#using Kappa
RNGkind(sample.kind = "Rounding")
set.seed(1234)
tree_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart",
  metric = "Kappa",
  trControl = trainControl(method = "cv", number = 3),
  tuneGrid = grid
)
tree_mod

#4. Bagging Ensamble
#using random forest "rf"
modelLookup("rf")

#creating grid search
grid <- expand.grid(mtry = 11)

#training model
library(randomForest)
RNGkind(sample.kind = "Rounding")
set.seed(1234)
rf_mod <-
  train(
    income ~ .,
    data = income_train,
    method = "rf",
    metric = "Kappa",
    trControl = trainControl(method = "cv", number = 3),
    tuneGrid = grid
  )

rf_mod

#5. Boosting Ensemble
modelLookup("xgbTree")

#training model
grid <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta =  0.3,
  gamma = 0.01,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

RNGkind(sample.kind = "Rounding")
set.seed(1234)

xgb_mod <-
  train(
    income ~ .,
    data = income_train,
    method = "xgbTree",
    metric = "Kappa",
    trControl = trainControl(method = "cv", number = 3),
    tuneGrid = grid
  )


xgb_mod

#6. Comparing Models
#to test the 3 models, we compare their performance against the test dataset 

# I: Tined Decision Tree (CART)
label <- income_test$income
pred <- predict(tree_mod, income_test)
prob <- predict(tree_mod, income_test, type = "prob")[ , "Below"]
matrix <- confusionMatrix(pred, label, positive = "Below")

#getting prediction and performance with ROC curve
roc_pred <- prediction(predictions = prob, labels = label)
roc_perf_tree <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

#getting performance metrics
accuracy <- as.numeric(matrix$overall["Accuracy"])
kappa <- as.numeric(matrix$overall["Kappa"])
sensitivity <- as.numeric(matrix$byClass["Sensitivity"])
specificity <- as.numeric(matrix$byClass["Specificity"])
precision <- as.numeric(matrix$byClass["Precision"])
recall <- as.numeric(matrix$byClass["Recall"])
fmeasure <- as.numeric(matrix$byClass["F1"])
auc <- unlist(slot(performance(roc_pred, measure = "auc"), "y.values"))

#creating table with performance metrics
performance <-
  tibble(
    approach = "Decision Tree (CART)",
    accuracy = accuracy,
    kappa = kappa,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = recall,
    fmeasure = fmeasure,
    auc = auc
  )
performance

#II Random forest ensemble
pred <- predict(rf_mod, income_test)
prob <- predict(rf_mod, income_test, type = "prob")[ , "Below"]
matrix <- confusionMatrix(pred, label, positive = "Below")

#ROC curve
roc_pred <- prediction(predictions = prob, labels = label)
roc_perf_bag <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

#performance metrics
accuracy <- as.numeric(matrix$overall["Accuracy"])
kappa <- as.numeric(matrix$overall["Kappa"])
sensitivity <- as.numeric(matrix$byClass["Sensitivity"])
specificity <- as.numeric(matrix$byClass["Specificity"])
precision <- as.numeric(matrix$byClass["Precision"])
recall <- as.numeric(matrix$byClass["Recall"])
fmeasure <- as.numeric(matrix$byClass["F1"])
auc <- unlist(slot(performance(roc_pred, measure = "auc"), "y.values"))

#ading to performance table
performance <- performance %>%
  add_row(
    approach = "Random Forest",
    accuracy = accuracy,
    kappa = kappa,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = recall,
    fmeasure = fmeasure,
    auc = auc
  )

#III Extreme Gradient Boosting
pred <- predict(xgb_mod, income_test)
prob <- predict(xgb_mod, income_test, type = "prob")[ , "Below"]
matrix <- confusionMatrix(pred, label, positive = "Below")

#ROC Curve
roc_pred <- prediction(predictions = prob, labels = label)
roc_perf_boost <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

#getting performance metrics
accuracy <- as.numeric(matrix$overall["Accuracy"])
kappa <- as.numeric(matrix$overall["Kappa"])
sensitivity <- as.numeric(matrix$byClass["Sensitivity"])
specificity <- as.numeric(matrix$byClass["Specificity"])
precision <- as.numeric(matrix$byClass["Precision"])
recall <- as.numeric(matrix$byClass["Recall"])
fmeasure <- as.numeric(matrix$byClass["F1"])
auc <- unlist(slot(performance(roc_pred, measure = "auc"), "y.values"))

#adding to performance table
performance <- performance %>%
  add_row(
    approach = "Extreme Gradient Boosting",
    accuracy = accuracy,
    kappa = kappa,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    recall = recall,
    fmeasure = fmeasure,
    auc = auc
  )

performance

#plotting ROC curve of all models
plot(roc_perf_tree, main = "ROC Curve for The Income Prediction Problem", col = 2, lwd = 2)
plot(roc_perf_bag, col = 3, lwd = 2, add = TRUE)
plot(roc_perf_boost, col = 4, lwd = 2, add = TRUE)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)
legend(0.6, 0.6, c('Decision Tree (CART)', 'Random Forest', 'Extreme Gradient Boosting'), 2:4)



