#Managing Data Example
library(tidyverse)

vehicles <-
  read_csv(file = 'https://s3.amazonaws.com/notredame.analytics.data/vehicles.csv', col_types = "nnnfnfffffnn")

#col_types = "nnnfnffffnn" means to bring the columns in as such data types, n = numeric and f = factor
glimpse(vehicles)

####### Exploration #########

summary(vehicles)
summary(vehicles$drive)

#when selecting multiple columns for summary, df goes in first arguement
summary(select(vehicles, class, cylinders))

#Notice that some categorical features, like the class feature, have more than six values. 
#To see the count for all feature values we need to use the table() function.
#Let's list all the values and counts for the class feature.
table(vehicles$class)

#Now we see all 10 values for the feature. Instead of the count, we can also show the proportional distribution of values for the class feature. 
#We use the prop.table() function for this.
prop.table(table(vehicles$class))

######## Visualization #########

#boxplot of C02 Emissions by vehicle class
vehicles %>% 
  ggplot()+
  geom_boxplot(mapping = aes(x = class, y = co2emissions), fill = "red") +
  labs(title = "Boxplot of C02 Emissions by Vehicle Class", x = "Class", y = "C02 Emissions") +
  theme_minimal()

#scatterplot of emissions vs cylinders
vehicles %>% 
  ggplot() +
  geom_point(mapping = aes(x = citympg, y = co2emissions), color = "blue") +
  labs(title = "Scatterplot of C02 Emissions vs. City MPG",
       x = "City MPG",
       y = "C02 Emissions") +
  theme_minimal()

#distribution of CO2 emissions
vehicles %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = co2emissions), bins = 30, fill = "yellow", color = "black") +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Frequency") +
  theme_minimal()

#how many vehicles per drive type
vehicles %>% 
  ggplot() +
  geom_bar(mapping = aes(x = year, fill = drive), color = "black") +
  labs(title = "stacked Bar Chart of Drive Type Composition by Year", x = "Model Year", y = "Number of Cars") +
  coord_flip() +
  theme_minimal()

########### DATA PREPARATION #########

#finding and identifying missing data
vehicles %>% 
  select(citympg, displacement, highwaympg) %>% 
  summary()

### Imputation ##

#check distribution to see if we should impute mean or median
vehicles %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = citympg), bins = 30, fill = "gray", color = "black") +
  theme_minimal()

#we should use the median because there are a number of outliers that will skew the data
#city mpg
vehicles <- vehicles %>% 
  mutate(citympg = ifelse(is.na(citympg), median(citympg, na.rm = TRUE), citympg))

summary(vehicles$citympg)

#highway mpg
vehicles %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = highwaympg), bins = 30, fill = "gray", color = "black") +
  theme_minimal()

#we should use the median because there are a number of outliers that will skew the data
vehicles <- vehicles %>% 
  mutate(highwaympg = ifelse(is.na(highwaympg), median(highwaympg, na.rm = TRUE), highwaympg))

summary(vehicles$highwaympg)

#displacement
vehicles %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = displacement), bins = 30, fill = "gray", color = "black") +
  theme_minimal()

#we should use the median because there are a number of outliers that will skew the data
vehicles <- vehicles %>% 
  mutate(displacement = ifelse(is.na(displacement), median(displacement, na.rm = TRUE), displacement))

summary(vehicles$displacement)

## Normalizaing Data ##

vehicles %>% 
  select(co2emissions) %>% 
  summary()

#decimal scaling approach
decimal_scale <- function(x, j) {
  return((x) / (10^j))
}

#Let's use the decimal_scale() function to normalize co2emissions (j = 4). 
#In order to compare the results of each approach, we create a new feature to hold the normalized values.
vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_d = decimal_scale(co2emissions, 4)) %>% 
  summary()

#z score normalize score
z_score_normalize <- function(x){
  return((x - mean(x)) / sd(x))
}

vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_z = z_score_normalize(co2emissions)) %>% 
  summary()

## min max ##
min_max_normalize <- function(x, y, z) {
  return(((x - min(x)) / (max(x) - min(x))) * (y - z) + z)
}

vehicles %>% 
  select(co2emissions) %>% 
  mutate(co2emissions_n = min_max_normalize(co2emissions, 1, 0)) %>% 
  summary()

#### Dummy Coding ####
# using drive features

vehicles %>% 
  select(drive) %>% 
  summary()

#To simplify our illustration, let's make a few changes to the data:
  
#We replace "2-Wheel Drive" with "Front-Wheel Drive" and "4-Wheel Drive" with "All-Wheel Drive".
#So as not to destroy our original data, we save our changes as a new dataset called vehicles2.
#For this new dataset, we also create a feature called drive2 which holds the new values.

vehicles2 <- vehicles %>% 
  mutate(drive2 = recode(drive, "2-Wheel Drive" = "Front-Wheel Drive")) %>% 
  mutate(drive2 = recode(drive2, "4-Wheel Drive" = "All-Wheel Drive")) %>%
  select(drive, drive2)

head(vehicles2)
summary(vehicles2)

library(dummies)
vehicles2 <- data.frame(vehicles2)

#converting to adding dummy binary variables
vehicles2 <- dummy.data.frame(data = vehicles2, names = "drive2", sep = "_")

#### Simple random sampling ######
#to make a random sample we use the sample function. first is upper bound then the amount of our sample and finally if we want replacement
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample(100, 20, replace = FALSE)

#with replacement AKA bootstrapping
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample(100, 20, replace = TRUE)

#creating a training and testing set using SRS. 
#First we generate a random set of 27734 (75%) of numbers between 1 - 36979 (number in our vehicle sample)
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- sample(36979, 27734, replace = FALSE)

vehicles_train <- vehicles[sample_set, ] #put our sample amount of rows and keep the columns from vehicles
glimpse(vehicles_train)

vehicles_test <- vehicles[-sample_set, ]

###### Stratified Sampling #######
#step one is taking a look at the population distribution
vehicles %>% 
  select(drive) %>% 
  table() %>% 
  prop.table()

#getting a 1% sample
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- sample(nrow(vehicles), nrow(vehicles) * 0.01, replace = FALSE)
vehicles_sample <- vehicles[sample_set, ]

#getting stratified sample using caTools
library(caTools)
RNGkind(sample.kind = "Rounding")
set.seed(1234)
sample_set <- sample.split(vehicles$drive, SplitRatio = 0.01)
vehicles_stratified <- subset(vehicles, sample_set == TRUE)

#comparing SRS to stratified samples
vehicles_sample %>%
  select(drive) %>%
  table() %>%
  prop.table()


vehicles_stratified %>%
  select(drive) %>%
  table() %>%
  prop.table()

#population for comparison
vehicles %>%
  select(drive) %>%
  table() %>%
  prop.table()


