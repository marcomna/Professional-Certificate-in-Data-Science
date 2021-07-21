##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(ggthemes)
library(ggrepel)
library(viridis)
library(RColorBrewer)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

######################################################################################################

dim(edx)

#use of head() to overview the general setup of the dataset

head(edx)

#table() shows all the frequencies for every value "rating" takes

table(edx$rating)

# unique() creates a vector or unique elements in "movieId"; length() displays its size

length(unique(edx$movieId))

length(unique(edx$userId))

#use of ggplot and ggthemes

edx %>%
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "lightgoldenrod3") +
  scale_x_discrete(limits = c(seq(0.5,5,0.5))) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Rating distribution by number of stars") +
  xlab("Stars") +
  ylab("Number of movies") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# the following code shows the frequency of every rating in descending order

edx %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>% 
  top_n(10, count) %>% 
  arrange(desc(count))

# this shows the movies with most ratings in descending order

edx %>% 
  group_by(movieId) %>% 
  summarize(Ratings = n(), Title = first(title)) %>% 
  arrange(desc(Ratings)) %>% 
  top_n(15, Ratings)


# first, creation of an object with the 20 most rated movies

top_rated_movies <- edx %>% 
  group_by(title) %>%  
  summarize(count=n()) %>% 
  top_n(20,count) %>%
  arrange(desc(count))

# then, graph of it reordering the list so the movies are in descending order

top_rated_movies %>% 
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat = "identity", color = "black", fill = "orchid1") + 
  coord_flip(y=c(0, 40000)) +
  xlab("") +
  ylab("Number of ratings") +
  geom_text(aes(label = count), hjust = -0.1, size = 3) +
  ggtitle("Top 20 movies based \n on number of ratings") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -1))

# the following graph shows the distributions of number of ratings per movie

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 40, color = "black", fill = "lightskyblue2") +
  scale_x_log10() +
  ggtitle("Number of ratings per movie") +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# this shows the distribution of number of ratings per user

edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) +
  geom_histogram(bins = 40, color = "black",  fill = "tomato2") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Number of ratings per user") +   
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# this graph is to show how the stars are distributed (it is clearly normal)

edx %>% 
  group_by(userId) %>%
  summarize(avg = mean(rating)) %>% 
  ggplot(aes(avg)) +
  geom_histogram(bins = 40, color = "black", fill = "orchid3") +
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Mean rating per user") +   
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# creation of the pertinent function to use onwards

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# first estimator, just the rating mean

mu_hat <- mean(edx$rating)

mu_hat

# first RMSE, a naive approach

naive_RMSE <- RMSE(edx$rating, mu_hat)

naive_RMSE

# example of higher RMSE using 3.5 (a number very close to mu_hat)

predictions <- rep(3.5, nrow(edx))

RMSE(edx$rating, predictions)

# storing of results in table using kable

RMSE_results <- data.frame(Method = "Average model", RMSE = naive_RMSE)

RMSE_results %>% 
  knitr::kable(label = "RMSEs per method")

# second estimator calculated according to the upper formula

mu <- mean(edx$rating)

movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating-mu))

# distribution of estimates

movie_avgs %>% 
  ggplot(aes(b_i)) +
  geom_histogram(bins = 10, color = "black",  fill = "coral4") +
  xlab("Estimates") +
  ylab("Frecuency") +
  ggtitle("Frecuency of estimates by value") +   
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# calculation of second RMSE and storage of results

predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  .$b_i

model_1_RMSE <- RMSE(predicted_ratings, validation$rating)

RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Movie effect model",
                                 RMSE = model_1_RMSE))

RMSE_results %>% 
  knitr::kable(label = "RMSEs per method")

# distribution of stars (users with more than 100 ratings)

edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n() >= 100) %>% 
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black",  fill = "ivory1") +
  xlab("Estimates") +
  ylab("Frecuency") +
  ggtitle("Frecuency of estimates by value") +   
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# computation of third estimate

user_avgs <- edx %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))

# third RMSE and storage of results

predicted_ratings <- validation %>% 
  left_join(movie_avgs, by = "movieId") %>% 
  left_join(user_avgs, by = "userId") %>% 
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred

model_2_rmse <- RMSE(predicted_ratings, validation$rating)

RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Movie + user effects model",
                                 RMSE = model_2_rmse))

RMSE_results %>% 
  knitr::kable(label = "RMSEs per method")

# definition of function to test 80 different lambdas, then computation of estimates, then computation of RMSE

lambdas <- seq(0, 20, 0.25)

RMSEs <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu)/(n()+l), n_i=n())
  
  b_u <- edx %>%
    left_join(b_i, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating -b_i-mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    mutate(pred= mu + b_i + b_u) %>% 
    pull(pred)
  
  return(RMSE(predicted_ratings, validation$rating))
})

# behavior of lambdas vs. RMSEs

qplot(lambdas, RMSEs, main = "RMSE for different values of lambda", xlab = "Lambdas", ylab = "RMSEs")

# extraction of optimal lambda

lambda_star <- lambdas[which.min(RMSEs)]

lambda_star

# final storage of results

RMSE_results <- bind_rows(RMSE_results,
                          tibble(Method = "Regularized movie + user effects model",
                                 RMSE = min(RMSEs)))

RMSE_results %>% 
  knitr::kable(label = "RMSEs per method")

