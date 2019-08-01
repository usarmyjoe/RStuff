#Dataset exploration

#grep("Romance",edx$genres, value=TRUE)
#edx %>% group_by(userId) %>% summarize()
#edx %>% select(title,rating) %>% filter(title=="Forrest Gump (1994)") %>% nrow()
#ratings %>% count(rating, sort=TRUE

#)
#Get the average rating from the training set
#mu <- mean(edx_train$rating)
#check a naive RMSE (will be very poor) - just using the avg
#naive_rmse <- RMSE(edx_train$rating, mu)
#1.060
#rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
#include bias mu
#movie_avgs <- edx_train %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
#predict ratings with bias included
#predicted_ratings1 <- mu + edx_train %>% left_join(movie_avgs, by='movieId') %>% .$b_i

#model_1_rmse <- RMSE(predicted_ratings, edx_test$rating)
#rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model", RMSE = model_1_rmse))


#user_avgs <- edx_train %>% left_join(movie_avgs, by='movieId') %>%
#  group_by(userId) %>% summarise(b_u = mean(rating - mu - b_i))

#predicted_ratings2 <- edx_train %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% .$pred

#model_2_rmse <- RMSE(predicted_ratings2, edx_test$rating)

#rmse_results <- bind_rows(rmse_results, data_frame(method="Movie + User Effects Model",RMSE = model_2_rmse))
#all_genres <- edx$genres %>% strsplit("\\|") %>% unlist(all_genres) %>% count()
