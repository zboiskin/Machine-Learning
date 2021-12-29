library(tidyverse)

#1 Collect the Data
donors <- read_csv("https://s3.amazonaws.com/notredame.analytics.data/donors.csv",
           col_types = "nnffnnnnnnnnffffffffff")

#Explore and Prepare the Data
glimpse(donors)

donors <- donors %>% 
  mutate(respondedMailing = recode(respondedMailing, "FALSE" = "0", "TRUE" = "1"))

donors %>% 
  keep(is.factor) %>% 
  summary()

#fumction that indicates missing values
useIndicatorVar <- function(x) (x = as.factor(ifelse(is.na(x), "UNK", x)))

donors <- donors %>% 
  mutate_at(
    c(
      'incomeRating',
      'wealthRating',
      'urbanicity',
      'socioEconomicStatus',
      'isHomeowner',
      'gender'
    ),
    ~ useIndicatorVar(.)
  )

donors %>% 
  keep(is.factor) %>% 
  summary()

#Get descriptives for numeric values
donors %>% 
  keep(is.numeric) %>% 
  summary()

#imputing mean for missing numeric values #EXPLAIN
donors <- donors %>% 
  group_by(gender) %>% 
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>% 
  ungroup()

#using median instead of mean for number of children
donors <- donors %>% 
  mutate(numberChildren = ifelse(is.na(numberChildren), median(numberChildren, na.rm = TRUE), numberChildren))

donors %>% 
  keep(is.numeric) %>% 
  summary()

#visualizaing the data
donors %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key), color = 'black') +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

#Rule of thumb for outliers, if value is greater than Q3 + 1.5 x IQR) then remove
#applying this rule to see if what looks like outliers should be removed
isNotOutlier <- function(x) (x <= quantile(x, .75) + (1.5*IQR(x)))

donors <- donors %>% 
  filter_at(
    c(
      'mailOrderPurchases',
      'totalGivingAmount',
      'numberGifts',
      'smallestGiftAmount',
      'largestGiftAmount',
      'averageGiftAmount'
    ),
    ~isNotOutlier(.)
  )

#visualizing
donors %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot() +
  geom_histogram(mapping = aes(x=value, fill=key), color="black") +
  facet_wrap(~ key, scales = "free") +
  theme_minimal()

#limiting our data to only numberic features NOT STANDARD PRACTICE
donors <- donors %>%
  select(
    -incomeRating,
    -wealthRating,
    -inHouseDonor,
    -plannedGivingDonor,
    -sweepstakesDonor,
    -P3Donor,
    -state,
    -urbanicity,
    -socioEconomicStatus,
    -isHomeowner,
    -gender
  )

#splitting into training and testing sets
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- sample(nrow(donors), round(nrow(donors)*.75), replace = FALSE)
donors_train <- donors[sample_set, ] 
donors_test <- donors[-sample_set, ]

#looking at the class distribution for eact dataset 
round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

#bringing back SMOTE to fix class imbalance 
library(DMwR)
RNGkind(sample.kind = "Rounding")
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~ ., data.frame(donors_train), perc.over = 100, perc.under = 200)

#checking the balance now
round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

#3 Train the Model
#To build a logistic regression model, we use the glm() function from the stats package. The stats package is loaded by default in R, 
#so we do not have to load it explicitly. We pass thre arguments to the glm() function - data, family and formula.

#data: is the training data (donors_train).

#family: is the type of regression model we intend to build. We set it to binomial. 
#This tells the glm() function that we intend to build a binomial logistic regression model using the logit link function. 
#Instead of setting family = binomial, we could also write family = binomial(link = "logit").

#formula: is the prediction formula. This is where we specify which features (predictors) to use to predict the class (response). 
#For our model, we specify that the function should use all the features in our training set (.) to build a model that predicts respondedMailing.

donors_mod1 <-
  glm(data = donors_train,
      family = binomial,
      formula = respondedMailing ~ .)

summary(donors_mod1)

#current coeffecients are log-odds, must convert to odds #WHY DID WE CHOOSE THESE PREDICTORS
exp(coef(donors_mod1)["averageGiftAmount"])
exp(coef(donors_mod1)["monthsSinceLastDonation"])
exp(coef(donors_mod1)["totalGivingAmount"])

#4 Evaluate the Model
donors_pred1 <- predict(donors_mod1, donors_test, type = 'response')

head(donors_pred1)

#setting cutoff to 0.5
ideal_cutoff = 0.5

#applying cut off the predictions
donors_pred1 <- ifelse(donors_pred1 >= ideal_cutoff, 1, 0)

head(donors_pred1)

#confusion matrix
donors_pred1_table <- table(donors_test$respondedMailing, donors_pred1)
donors_pred1_table

#computng accuracy 
sum(diag(donors_pred1_table)) / nrow(donors_test)

#5 Improving the Model
#building a model only using significant features from the first model

#significant features have a .,*,**, or ***
summary(donors_mod1)

#Collinearity is a condition in which some of the independent variables are highly correlated. It often results in an inflation in the variance of regression coefficients.
#evaluating which features are collinear
#visual
library(corrplot)
donors %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

#listing correlations, if VIF indicates a a score of 5+ then it collinearity is present and must be dealt with
library(car)
vif(donors_mod1)

#creating second model using only selected sig predictors
donors_mod2 <-
  glm(
    data = donors_train,
    family = binomial,
    formula = respondedMailing ~ numberChildren + totalGivingAmount + largestGiftAmount + monthsSinceLastDonation
  )

summary(donors_mod2)

#making and previewing our predictions
donors_pred2 <- predict(donors_mod2, donors_test, type = 'response')
head(donors_pred2)

#determining ideal cutoff threshold
library(InformationValue)
ideal_cutoff <-
  optimalCutoff(
    actuals = donors_test$respondedMailing,
    predictedScores = donors_pred2,
    optimiseFor = "Both"
  )

ideal_cutoff

#applying cutoff to model
donors_pred2 <- ifelse(donors_pred2 >= ideal_cutoff, 1, 0)
head(donors_pred2)

#testing accuracy 
donors_pred2_table <- table(donors_test$respondedMailing, donors_pred2)
sum(diag(donors_pred2_table)) / nrow(donors_test)


