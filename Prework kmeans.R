####k means####
library(tidyverse)
college <-
  read_csv("https://s3.amazonaws.com/notredame.analytics.data/college.csv",
           col_types = "nccfffffnnnnnnnnn")

glimpse(college)
head(college)
summary(college)

#creating a subset of only Maryland Colleges
maryland_college <- college %>% 
  filter(state == "MD")

head(maryland_college)

#converting the name feature into a row since our analysis only works on numeric data but we want to still see the school it applies to
maryland_college <- maryland_college %>% 
  column_to_rownames(var = "name")

head(maryland_college)

maryland_college %>% 
  select(admission_rate, sat_avg) %>% 
  summary()

#normalizing (putting on the same scale) avg SAT and admission rate, standard is z-score
maryland_college_scaled <- maryland_college %>% 
  select(admission_rate, sat_avg) %>% 
  scale()

summary(maryland_college_scaled)

#Creating Clusters#
#k means inputs is x=the dataset, centers = specifies the number of clusters to create "k", nstart = specifies how many random starting points to try
library(stats)
set.seed(1234)
k_3 <- kmeans(maryland_college_scaled, centers=3, nstart = 25)

#visulizaing clusters created
library(factoextra)
fviz_cluster(k_3,
             data = maryland_college_scaled,
             repel = TRUE,
             ggtheme = theme_minimal())

#Evaluating the quality of clusters
k_3$size

#centroid coordinates
k_3$centers

#listing the colleges in each cluster
k_3$cluster

#bringing the cluster analysis back into a table format
maryland_college <- maryland_college %>% 
  rownames_to_column(var = "name") %>% 
  mutate(cluster = k_3$cluster) %>% 
  select(cluster, name, everything())

head(maryland_college)

#bringing back other attributes along with the new cluster information
maryland_college %>% 
  select(cluster, undergrads, tuition, faculty_salary_avg, loan_default_rate, median_debt) %>% 
  group_by(cluster) %>% 
  summarise_all("mean")

#elbow method
fviz_nbclust(maryland_college_scaled, kmeans, method = "wss")

#average silhouette method
fviz_nbclust(maryland_college_scaled, kmeans, method = "silhouette")

#gap statistic method
fviz_nbclust(maryland_college_scaled, kmeans, method = "gap_stat")
