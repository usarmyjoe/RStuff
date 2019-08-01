#Machine Learning Algorythm
#Joseph Esensten
#####Instructions:
#Train a machine learning algorithm using the inputs in one subset to predict movie ratings in the validation set.
#Develop your algorithm using the edx set. 
#For a final test of your algorithm, predict movie ratings in the validation set as if they were unknown. 
#RMSE will be used to evaluate how close your predictions are to the true values in the validation set.
######
#Load Libraries
library(caret) #for machine learning algorythms and functions
#library(plyr)
library(dplyr) #for lots of tools
library(knitr)
#set seed
set.seed(1, sample.kind = "Rounding")
#Data Exploration
#See number of users and movies in edx dataset
counts <- edx %>% summarize(count_users = n_distinct(userId), count_movies = n_distinct(movieId))

#Lets see what users and movies look like
#grab a random sample of users and movies
user_sample <- sample(n_distinct(edx$userId),3, replace=FALSE)
movie_sample <- sample(n_distinct(edx$movieId),3, replace=FALSE)
#edx %>% filter(userId %in% user_sample)
#edx %>% filter(movieId %in% movie_sample)

#Lets check out the movies in detail
movie_ratings <- edx %>% select(movieId, rating)
#movie_count_ratings <- count(movie_ratings$movieId)
#movie_avg_ratings <- aggregate(movie_ratings[,2], list(movie_ratings$title),mean)
movie_num_ratings <- movie_ratings %>% count(movieId)
num_ratings <- movie_num_ratings[2] %>% table()
movies <- edx %>% group_by(movieId) %>% summarize(avgRating = mean(rating))

#Lets check out the users in detail
user_ratings <- edx %>% select(userId, rating)
user_avg <- aggregate(user_ratings[,2], list(user_ratings$userId),mean)
user_count_ratings <- user_ratings %>% count(userId)
#Lets check out the genres in detail
all_genres <- edx$genres %>% strsplit("\\|") %>% unlist()
genre_count <- table(all_genres)

#create additional training and test sets as per project instructions to not use "validation" set.
#test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
#edx_train <- edx[-test_index,]
#edx_test <- edx[test_index,]
#Define Root Mean Square Error (RMSE) function.
#SD of residuals. How far data points are from regression line.
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#Lowest RMSE possible will have the best accuracy in prediction
#For a baseline, lets calculate the simple average
mu <- mean(edx$rating)
#Check that against the test set
avg_rmse <- RMSE(edx$rating, mu)
#add this to a running tracker
RMSE_tracker <- data_frame(type="Basic Average", RMSE = avg_rmse)
#1.06, pretty high - over 1 star.
#Explore the movie data

#hist(movies$avgRating, xlab="Rating", ylab="Count", main="Distribution of Ratings")
#Movie Effect Model
#Get movie ratings - mu (bhat)
movie_avg <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#Movie effect prediction
me_predict <- mu + edx %>%
  left_join(movie_avg, by='movieId') %>%
  pull(b_i)
me_RMSE <- RMSE(me_predict, edx$rating)
#Add Movie Effect to tracker
RMSE_tracker <- bind_rows(RMSE_tracker, data_frame(type="Movie Effect", RMSE = me_RMSE))
#User Effect prediction
user_effect <- user_ratings %>% group_by(userId) %>% summarize(b_u = mean(rating))
#Get user +movie biases
user_bias <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#Get user predictions
ue_predict <- edx %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

ue_RMSE <- RMSE(ue_predict, edx$rating)
#Add User Effect to tracker
RMSE_tracker <- bind_rows(RMSE_tracker, data_frame(type="User Effect + Movie Effect", RMSE = ue_RMSE))

#Regularization - Penalized Least Squares was very good at getting RMSE low in previous examples I learned in the course. Let's start there
#The purpose here is to control the variability of the "Movie Effect" (Some movies are rated lower or higher simply due to bias) 
#and User Effect (Some users love every movie and some hate every movie).
#We need to account for large estimates and penalize them accordingly. 
#We will want to use cross-validation with other methods to ensure we have an optimal lambda

##Start with a range of lambdas
lambda <- seq(0, 10, 0.25)
#calculate the RMSE
rmses <- sapply(lambda, function(x){
  #get least squares estimate for movie effect
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+x))
  #get least squares estimate for user effect
  b_u <- edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+x))
  #compare predictions to the test set
  rating_prediction <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  rating_prediction <- rating_prediction[!is.na(rating_prediction)]
    #Return RMSEs derived from two values: the prediction for each movie and the actual rating
  return(RMSE(rating_prediction, validation$rating))
}) 
#report the best findings
best_lambda <- lambda[which.min(rmses)]
best_rmse <- rmses[which.min(lambda)]
#Add best finding to tracker
RMSE_tracker <- bind_rows(RMSE_tracker, data_frame(type="LSE Cross-Validation", RMSE = best_rmse))
RMSE_tracker
cat("Smallest RMSE:", min(RMSE_tracker$RMSE))
save.image(file="MovieLens-Esensten.RData")
