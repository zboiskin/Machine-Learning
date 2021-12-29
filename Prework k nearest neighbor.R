library(tidyverse)

heart <-
  read_csv("https://s3.amazonaws.com/notredame.analytics.data/heart.csv",
           col_types = "nffnnffnfnfnff")

#Explore and Prepare the Data
glimpse(heart)
head(heart)
summary(heart)

#removing NA data
?filter_at

heart <- heart %>% 
  filter_at(c("restingBP",
            "cholesterol",
            "highBloodSugar",
            "restingECG",
            "restingHR",
            "exerciseAngina",
            "STdepression",
            "STslope",
            "coloredVessels",
            "defectType"),
~ !is.na(.)
)

#summary stats of numeric values only
heart %>% 
  select_if(is.numeric) %>% 
  summary()

#nin-max normalization to put all numerics on the same scale
min_max_normalize <- function(x,y,z) {
  return(((x-min(x)) / (max(x) - min(x))) * (y - z) + z)
}

#applying function to the data set
heart1 <- heart %>% 
  mutate_if(is.numeric, ~ min_max_normalize(., 1, 0)) #what does the . mean?

#viewing results
heart1 %>% 
  select_if(is.numeric) %>% 
  summary()

#we must convert the data to a dataframe and add dummy code because k-nearest neighbor only works on numeric code
heart1 <- data.frame(heart1)

#splitting the dependent variable label from the rest of the data
heart_labels <- select(heart1, heartDisease)
heart1 <- select(heart1, -heartDisease)

#creating dummy variable table
library(dummies)
heart1 <- dummy.data.frame(data = heart1, sep = "_")

#reviewing results from dummy application
heart1 %>% 
  select_if(is.numeric) %>% 
  summary()

#splitting data into training and testing set. 75% training, 25% testing
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_index <- sample(nrow(heart1), round(nrow(heart1) * .75), replace = FALSE) #can you walk me through the code for this?
heart_train <- heart1[sample_index,]
heart_test <- heart1[-sample_index,]

#splitting the class labels (dependent variable) as well, and convert to factors
heart_train_labels <- as.factor(heart_labels[sample_index,])
heart_test_labels <- as.factor(heart_labels[-sample_index,])

# Train the Model #
#The class package provides the knn() function that implements k-nearest neighbors. We pass four arguments to the the function - train, test, cl and k.

#train: the independent variables of the training set.
#test: the independent variables of the test set.
#cl: the dependent variable of the training set.
#k: the number of neighbors to be considered.

library(class)
heart_pred <-
  knn(
    train = heart_train, 
    test = heart_test,
    cl = heart_train_labels,
    k = 3
  )

head(heart_pred)

#Evaluate the model
#creating a confusion matrix to evaluate performace
heart_pred_table <- table(heart_test_labels, heart_pred)
heart_pred_table

#computing accuracy
sum(diag(heart_pred_table)) / nrow(heart_test)

# Improving the model
heart_pred2 <-
  knn(
    train = heart_train, 
    test = heart_test,
    cl = heart_train_labels,
    k = 40
  )

heart_pred2_table <- table(heart_test_labels, heart_pred2)
sum(diag(heart_pred2_table)) / nrow(heart_test)

heart_pred3 <-
  knn(
    train = heart_train, 
    test = heart_test,
    cl = heart_train_labels,
    k = 15
  )

heart_pred3_table <- table(heart_test_labels, heart_pred3)
sum(diag(heart_pred3_table)) / nrow(heart_test)






