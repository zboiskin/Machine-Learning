###Cluster Analysis###
library(tidyverse)
library(arules)

#Collect and Explore the Data

#read transactions is for reading data that is very sparse (a lot o missing values)
groceries <-
  read.transactions("https://s3.amazonaws.com/notredame.analytics.data/groceries.csv", sep = ",")

summary(groceries)
#rows = # of transactions, columns = # of unique items, density = completeness
#most frequent items = support
#length distribution size is # of transactions with that amount of items such as there are 2159 transactions with 1 item

#allows us to look at transactions as an index such as looking at what items are in the first 5 transactions
inspect(groceries[1:5])

#lets us look at the frequency (support) for each item in the data set per transaction
itemFrequency(groceries[ ,1:20])

#frequency of individual item per transaction ex sugar is in .033 (or 332) of total 9834 transactions
itemFrequency(groceries[,"sugar"])

#creating a tibble
groceries_frequencies <- 
tibble(Item = names(itemFrequency(groceries)),
  Frequency = itemFrequency(groceries))

head(groceries_frequencies)

#what are the 10 most frequently bought items at this store?
groceries_frequencies %>% 
  arrange(desc(Frequency)) %>% 
  slice(1:10)

#arranging top 10 in a ggplot
groceries_frequencies %>% 
  arrange(desc(Frequency)) %>% 
  slice(1:10) %>% 
  ggplot(mapping = aes(x = reorder(Item,Frequency), y = Frequency)) +
  geom_col(fill = "orange", color = "black") +
  labs(x = "Item") +
  coord_flip() + theme_minimal() + theme(legend.position = "none")

#Create the Rules
#suport # comes from us deciding that we want to see only items that were purchased at least 5 times a day. Our data is for 30 days with 9834 transactions so (5x30)/9834 = 0.015
#confidence specifies how accurate a rule has to be in order to be included in the rule set. Default is .8 but our dataset is small so .25
#minlen is which rules to include for at least x amount of items in a transaction. We want to see these rules applied to transactions with more than 2 items
grocery_rules <- apriori(groceries,
        parameter = list(
          support = 0.015,
          confidence = 0.25,
          minlen = 2
        ))

#we can see that 78 rules were generated
grocery_rules

### Evaluate the Rules ####
summary(grocery_rules)

#preview the rules by index for first 10
inspect(grocery_rules[1:10])

#sorting rules by highest lift for most actionable
grocery_rules %>% 
  sort(by = "lift") %>% 
  head(n = 5) %>% 
  inspect()

#finding rules with a specific item in it
grocery_rules %>% 
  subset(items %in% "tropical fruit") %>% 
  inspect()

#The keyword items, matches an item appearing anywhere in the rule.
#Limit the subset with lhs and rhs instead.
#The operator %in% means that at least one of the items must be found in the list you defined.
#For partial matching (%pin%) and complete matching (%ain%). #matching used in arules package
#We can also filter by support, confidence, or lift.
#We can also combine standard R logical operators such as and (&), or (|), and not (!).

grocery_rules %>% 
  subset(lhs %in% "tropical fruit") %>% 
  sort(by = "lift") %>% 
  inspect()

# Export the Rules #

#converting rules into CSV format to share
write(
  grocery_rules,
  file = "groceryrules.csv",
  sep = ",",
  quote = TRUE,
  row.names = FALSE)

#converting to dataframe
groceryrules_data <- as(grocery_rules, "data.frame")
head(groceryrules_data)

