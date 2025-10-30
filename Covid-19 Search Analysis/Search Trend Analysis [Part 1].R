###More Information about this particular publically available dataset can be found 
###at https://github.com/open-covid-19/data

##The csv file has been queried using BigQuery (SQL)
install.packages("pheatmap")
library(tidyverse)
library(pheatmap)

#Checking out the data. I notice a lot of missing data.
dataset<-read_csv("Covid_19 Search Trends Preliminary.csv")
summary(dataset)
colnames(dataset)
head(dataset)
glimpse(dataset)

#I will isolate columns that start with "search_trends_". Then I will filter for variables where there is more than
#20% of the data available.
search <- dataset %>%
  select(starts_with("search_trends_"))

search_filtered <- search %>%
  select(where(~ mean(!is.na(.)) > 0.2))

#Let's use a heatmap. The syntax below creates a matrix for the heatmap. Pairwise.complete.obs is used to
#use all rows where both variables don't have missing values. This allows us to use as much of the data as possible
#despite the high amount of missing values.

cor_mat <- cor(search_filtered, use = "pairwise.complete.obs")
cor_mat

#To be honest, this one sucks. There are way too many variables and the visualization is a mess.
#Right now, we're visualizing a correlation matrix of Google Search Trends. I will reduce this further. 
pheatmap(cor_mat, 
         scale = "none",
         clustering_distance_rows = "euclidean",
         clustering_distance_cols = "euclidean",
         main = "Correlation Heatmap of Search Trends")

#We'll reduce the heatmap further to the top 15 searches.
top_searches <- search_filtered %>%
  summarise(across(everything(), ~ sd(., na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "symptom", values_to = "sd") %>%
  arrange(desc(sd)) %>%
  slice(1:15) %>%
  pull(symptom)

cor_small <- cor(search_filtered[top_searches], use = "pairwise.complete.obs")

pheatmap(cor_small, main = "Top 15 Variable Symptoms Heatmap")

#We can see that the top searches for 1000 observations in the United States of America during 2020 were
#not always directly to Covid-19. Obesity,Pain,Arthritis,Diabetes,Hypertension,Infection,Inflammation,the common cold
#cough,allergy,itch,skin_rash,alcoholism,acne, and anxiety were the top 15 searches. We see that there is a moderate
#amount of searches between infection and coughing, skin_rash and infections, arthritis and pain, etc (as indicated
#by the yellow boxes in the heatmap.) There is a fairly strong relatinoship (around 0.8) between searching for the
#common_cold and coughing.