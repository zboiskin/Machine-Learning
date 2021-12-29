# Performance Evaluation
library(tidyverse)
library(caret)
library(DMwR)
library(rpart)
library(e1071)
library(ROCR)

#1, Holdout Method
income <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/income.csv", col_types = "nffnfffffnff")

glimpse(income)
summary(income)

#splitting into training and testing data, saving our test data for end of evaluation
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)
income_train <- income[sample_set, ]
income_test <- income[-sample_set, ]

#Balancing the imbalanced data set with SMOTE
RNGkind(sample.kind = "Rounding")
set.seed(1234)
income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

# k-fold cross-validation method demonstration
# 1. The prediction formula
# 2. The training data (data).
# 3. The performance metric that we intend to use (metric)
# 4. The machine learning algorithm to use (method)
# 5. The control object that specifies the resampling technique to use (trControl)

RNGkind(sample.kind = "Rounding")
set.seed(1234)

income_mod <- train(
  income ~ .,
  data = income_train,
  metric = "Accuracy",
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5)
)

#resample attribution allows us to see performance metrics for each iteration of the resampling process
income_mod$resample %>% 
  arrange(Resample)

# k-fold cross-validation estimate of the model's performance is the average
income_mod$resample %>% 
  arrange(Resample) %>% 
  summarise(AvgAccuracy = mean(Accuracy), AvgKappa = mean(Kappa))

#Monte Carlo/Random cross validation training. "LGOCV means leave group out cross validation, training percentage (p) = 0.1 and # iterations "number" = 10 
RNGkind(sample.kind = "Rounding")
set.seed(1234)
income_mod <- train(
  income ~ ., 
  data = income_train, 
  method = "rpart",
  trControl = trainControl(method = "LGOCV", p = .1, number = 10)
)

#model performance
income_mod$resample %>% 
  arrange(Resample) %>% 
  summarise(AvgAccuracy = mean(Accuracy), AvgKappa = mean(Kappa))

#Bootstrapping Method
RNGkind(sample.kind = "Rounding")
set.seed(1234)

income_mod <- train(
  income ~ .,
  data = income_train,
  method = "rpart",
  trControl = trainControl(method = "boot632", number = 3)
)

income_mod$resample %>% 
  arrange(Resample) %>% 
  summarise(AvgAccuracy = mean(Accuracy), AvgKappa = mean(Kappa))

#2. Beyond Predictive Accuracy
download.file("https://s3.amazonaws.com/notredame.analytics.data/spam.RData", "spam.RData")
load("spam.RData")

#alternative way to build confusion matrix
spam_matrix <- confusionMatrix(email_pred, email_test$message_label, positive = "spam")
spam_matrix
spam_matrix$table

#getting accuracy and kappa
spam_accuracy <- as.numeric(spam_matrix$overall["Accuracy"])
spam_accuracy

spam_kappa <- as.numeric(spam_matrix$overall["Kappa"])
spam_kappa

#getting sensitivity, specificity, precision, recall and F-measure
spam_sensitivity <- as.numeric(spam_matrix$byClass["Sensitivity"])
spam_sensitivity

spam_specificity <- as.numeric(spam_matrix$byClass["Specificity"])
spam_specificity

spam_precision <- as.numeric(spam_matrix$byClass["Precision"])
spam_precision

spam_recall <- as.numeric(spam_matrix$byClass["Recall"])
spam_recall

spam_fmeasure <- as.numeric(spam_matrix$byClass["F1"])
spam_fmeasure

#3. Visualizing Model Performance
#creating predition object 

email_pred_prob <- predict(email_mod, email_test,  type = "raw")
head(email_pred_prob)

roc_pred <- prediction(predictions = email_pred_prob[, "spam"], labels = email_test$message_label)

#calculating true positive and false positive rate
roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

#plotting ROC Curve
plot(roc_perf, main = "ROC Curve for Spam Filter Model", col = 2, lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

#Second viz that also shows AUC
mod_auc <- performance(roc_pred, measure = "auc") %>% 
  slot("y.values") %>% 
  unlist()

mod_auc






