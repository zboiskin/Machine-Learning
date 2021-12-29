library(tidyverse)
permits <-
  read_csv("https://s3.amazonaws.com/notredame.analytics.data/permits.csv",
           col_types = "ffffffnnnnfffff")

glimpse(permits)
head(permits)
summary(permits)

#permit category is the variable we are trying to predict
#removing missing permitCategory values
permits <- permits %>% 
  filter(!is.na(permitCategory))

#set values that don't make sense, such as # of stores buildings have being a negative number, to NA
permits <- permits %>% 
  mutate_at(c('valuation', 'floorArea', 'numberUnits', 'stories'), ~ ifelse(. < 1, NA, .))

#correcting tallest building # of stories, since the tallest building in LA is 73 floors, anything taller than 73 floors is set to NA
permits <- permits %>% 
  mutate(stories = ifelse(stories > 73, NA, stories))

#renaming permitCategory to planCheck with values Yes or No
permits <- permits %>% 
  mutate(planCheck = as.factor(ifelse(permitCategory == "Plan Check", "Yes", "No"))) %>% 
  select(-permitCategory)

summary(permits)

#split data into 80% training and 20% testing
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- sample(nrow(permits), round(nrow(permits)* .80), replace = FALSE)
permits_train <- permits[sample_set,]
permits_test <- permits[-sample_set,]

#Train the Model

#Our decision tree model will be based on the CART algorithm. To train our model, we make use of the rpart() function from the rpart package. 
#We pass four arguments to the function - formula, method, data and cp.

#formula: the prediction formula. This specifies which independent variables are to be used to predict the dependent variable. 
#For example, given dependent variable Y, and independent variables X1, X2 and X3, a prediction formula that reads Y ~ . means that we intend to predict Y using all of the independent variables. 
#A prediction formula that reads Y ~ X1 + X3 means that we intend to predict Y using only X1 and X3. This formula could also be written as Y ~ . - X2.
#method: the type of decision tree we intend to build. We set this to "class", indicating that this is a classification tree.
#data: the training set.
#cp: the complexity parameter threshold to use when pruning the tree. We set this to 0.02.

library(rpart)
permits_mod <- 
  rpart(
    planCheck ~ .,
    method = "class",
    data = permits_train,
    cp = 0.02
  )

library(rpart.plot) #decision, % yes, % of data used in the partition
rpart.plot(permits_mod)

# Evaluate the Model
#predicting the class of examples using the test set
permits_pred <- predict(permits_mod, permits_test, type = "class")
head(permits_pred)

permits_pred_prob <- predict(permits_mod, permits_test)
head(permits_pred_prob)

#creating confusion matrix for performance evaluation
permits_pred_table <- table(permits_test$planCheck, permits_pred)
permits_pred_table

#calculating accuracy of model using confusion matrix
permits_pred_accuracy <- sum(diag(permits_pred_table)) / nrow(permits_test)
permits_pred_accuracy

# Improve the Model
permits_pred_table

#Cost Matrix?
costmatrix <- matrix(c(0, 6, 1, 0), ncol = 2)
costmatrix

#
permits_mod_loss <-
  rpart(
    planCheck ~ .,
    method = "class",
    data = permits_train,
    cp = 0.01,
    parms = list(loss = costmatrix)
  )

permits_pred_loss <- predict(permits_mod_loss, permits_test,  type = "class")
permits_pred_loss_table <- table(permits_test$planCheck, permits_pred_loss)
permits_pred_loss_table

#checking accuracy
permits_pred_loss_accuracy <- sum(diag(permits_pred_loss_table)) / nrow(permits_test)
permits_pred_loss_accuracy
