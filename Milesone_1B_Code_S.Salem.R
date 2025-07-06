## MILESTONE 1B - Code 
## Name: Sami Salem
## Date: 6/7/2025


# Loading Required Packages
library(tidyverse)
library(ggplot2)
library(patchwork)


## SECTION 2.2.1 - DATA CLEANING FOR IMDB DATASET

IMDB <- read_csv('imdb_movies.csv') # 'imdb_movies.csv' downloaded form citation [1] in report
IMDB <- IMDB %>% select(names, date_x, revenue, budget_x, score, genre, orig_lang, crew) # selecting relevant columns

    # Numerical column Data Cleaning

IMDB <- IMDB %>% mutate(budget = budget_x) # creating new budget column
IMDB <- IMDB %>% select(-budget_x) # removing old budget_x column

    # Numerical Plots, (All boxplost)

p1 <- IMDB %>% ggplot(aes(score)) + geom_boxplot() + labs(title = 'Box Plot of IMDB Scores',x = "imdb_score",y = "Null")

p2 <- IMDB %>% ggplot(aes(revenue)) + geom_boxplot() + labs(title = 'Box Plot of Revenue',x = "revenue",y = "Null")

p3 <- IMDB %>% ggplot(aes(budget)) + geom_boxplot() + labs(title = 'Box Plot of Budget',x = "budget",y = "Null")

p1 + p2 / p3 # Plots using 'patchwork packages

  #  Categorical Data Cleaning


IMDB$genre <- as.factor(IMDB$genre) # convering character variables to factors, useful for later on
IMDB$orig_lang <- as.factor(IMDB$orig_lang)
IMDB$crew <- as.factor(IMDB$crew)
IMDB$names <- as.factor(IMDB$names)
IMDB$date_x <- as.factor(IMDB$date_x)

dates <- str_extract_all(IMDB$date_x,'\\d+') # getting numerical values in date columns

new_dates <- numeric(length(IMDB$date_x))

for (i in 1:length(new_dates)){
  new_dates[i] = dates[[i]][3] # third value is year
}

IMDB <- IMDB %>% mutate(genre = str_extract(IMDB$genre,"[A-Z][a-z]+")) # There are too many genres to use, so we use only the first listed genre
IMDB <- IMDB %>% select(-genre)
IMDB <- IMDB %>% mutate(year = as.factor(new_dates))
IMDB <- IMDB %>% select(-date_x)


write_csv(IMDB, 'IMDB_cleaned.csv') # downloading to current repository, will be used in python code to extract character names


# Section 3.1 Numerical Data Feature plots

ATI <- read_csv('ATI.csv')

ATI <- ATI %>% select(-'...1')

ATI <- ATI %>% filter(orig_lang == 'English')

ATI %>% select(-orig_lang) # now removing orig_lang column as no longer necessary

write_csv(ATI,'ATI.csv')
p1 <- ATI %>% ggplot(aes(twitter_score)) + geom_boxplot() + labs(title = 'Box Plot of Twitter Scores',x = "twitter_score",y = "Null")
p2 <- ATI %>% ggplot(aes(imdb_score)) + geom_boxplot() + labs(title = 'Box Plot of IMDB Scores',x = "imdb_score",y = "Null")
p3 <- ATI %>% ggplot(aes(actors_score)) + geom_boxplot() + labs(title = 'Box Plot of Actor Scores',x = "actors_score",y = "Null")
p4 <- ATI %>% ggplot(aes(revenue)) + geom_boxplot() + labs(title = 'Box Plot of Revenue',x = "revenue",y = "Null")
p5 <- ATI %>% ggplot(aes(budget)) + geom_boxplot() + labs(title = 'Box Plot of Budget',x = "budget",y = "Null")

p1 + p2 / p3
p4 + p5

# Section 3.3 Scatter Plots

p1 <- ATI %>% ggplot(aes(imdb_score, revenue)) + geom_point() + geom_smooth() + labs(title = "Relationship between IMDB scores and Revenue")

p2 <- ATI %>% ggplot(aes(budget, revenue)) + geom_point() + geom_smooth() + labs(title = "Relationship between Budget and Revenue")

p3 <- ATI %>% ggplot(aes(twitter_score, revenue)) + geom_point() + geom_smooth() + labs(title = "Relationship between Twitter scores and Revenue")

p4 <- ATI %>% ggplot(aes(actors_score, revenue)) + geom_point() + geom_smooth() + labs(title = "Relationship between Actors scores and Revenue")

p2 / p4
p1 / p3

ATI_nozeroactor <- ATI %>% filter(actors_score > 0)
cor(ATI_nozeroactor$actors_score, ATI_nozeroactor$revenue)

ATI_nozeroactor <- ATI %>% filter(actors_score > 10000000000)
cor(ATI_nozeroactor$actors_score, ATI_nozeroactor$revenue)

# Section 3.4 - K-Means clustering 


ATI_kmeans_budget <- kmeans(ATI %>% select(budget),centers = 3)
ATI <- ATI %>% mutate(no_cluster = factor(ATI_kmeans_budget$cluster))

p1 <- ATI %>% ggplot(aes(revenue,colour = no_cluster)) + geom_boxplot() + labs(title = "Revenue, seperated into K-Means clusters of Budget")

ATI <- ATI %>% select(-no_cluster) # remove cluster column, as plot ends

ATI_kmeans_twitter <- kmeans(ATI %>% select(twitter_score),centers = 3)
ATI <- ATI %>% mutate(no_cluster = factor(ATI_kmeans_twitter$cluster))

p2 <- ATI %>% ggplot(aes(revenue,colour = no_cluster)) + geom_boxplot() + labs(title = "Revenue, seperated into K-Means clusters of Twitter Scores")

ATI <- ATI %>% select(-no_cluster)

ATI_kmeans_imdb <- kmeans(ATI %>% select(imdb_score),centers = 3)
ATI <- ATI %>% mutate(no_cluster = factor(ATI_kmeans_imdb$cluster))

p3 <- ATI %>% ggplot(aes(revenue,colour = no_cluster)) + geom_boxplot() + labs(title = "Revenue, seperated into K-Means clusters of IMDB Scores")

ATI <- ATI %>% select(-no_cluster) 

ATI_kmeans_actors <- kmeans(ATI %>% select(actors_score),centers = 3)
ATI <- ATI %>% mutate(no_cluster = factor(ATI_kmeans_actors$cluster))

p4 <- ATI %>% ggplot(aes(revenue,colour = no_cluster)) + geom_boxplot() + labs(title = "Revenue, seperated into K-Means clusters of Actor Scores")


p1 / p2 
p3 / p4

ATI_highactors <- ATI %>% filter(no_cluster == 1)

cor(ATI_highactors$revenue, ATI_highactors$actors_score)
