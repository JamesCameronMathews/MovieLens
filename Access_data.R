
# Knitr and kable options
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE,
                      fig.align="center", out.width="80%")
options(kableExtra.latex.load_packages = FALSE)

# Open required packages
library(tidyverse)
library(caret)
library(ggplot2)
library(lubridate)
library(kableExtra)
library(stringr)
library(knitr)
library(scales)
library(dplyr)
detach("package:ggpubr", unload = TRUE)

# Create ggplot2 theme
plot_theme <- theme(plot.caption = element_text(size = 11, face = "italic"), axis.title = element_text(size = 11))

# Create edx set and final hold-out test set)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

# Load
load(file="~/Documents/James/EdX/movielens/.RData")

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Plot the distribution of ratings in edx
edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.1, color = I("black")) +
  scale_y_continuous(breaks = c(1000000, 2000000), labels = c("1", "2")) +
  labs(x = "Rating", y = "Count (millions)") + plot_theme

# Plot average rating by movie in the edx dataset
edx %>% group_by(movieId) %>%
  summarise(ave_rating = sum(rating)/n()) %>%
  ggplot(aes(ave_rating)) +
  geom_histogram(bins=25, color = I("black")) +
  labs(x = "Average rating", y = "No. of movies" ) + plot_theme

# Plot number of ratings by movie in the edx dataset
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = I("black")) +
  scale_x_log10() +
  labs(x = "Movies", y = "Number of ratings") + plot_theme

# Plot average rating by user in the edx dataset
edx %>% group_by(userId) %>%
  summarise(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=30, color = I("black")) +
  labs(x = "Mean rating", y = "No. of users") + plot_theme

# Plot number of ratings by user in the edx dataset
edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=30, color = I("black")) +
  scale_x_log10() +
  labs(x = "Users", y = "No. of ratings") + plot_theme

# Individual genres were seperated and ranked
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n(), rating = round(mean(rating), 2)) %>%
  arrange(desc(count)) %>%
  kable(col.names = c("Genre", "No. of Ratings", "Avg. Rating"),
        caption = "Individual genres ranked by number of ratings",
        align = "lrr", booktabs = TRUE, format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot average rating by genre for genre combinations with at least 100,000 ratings
edx %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Genre(s)", y = "Mean Rating") + plot_theme

# Group and list top 10 movie titles based on number of ratings
edx %>% group_by(title) %>%
  summarise(n = n()) %>%
  slice_max(n, n=10) %>%
  kable(caption = "Top 10 Movies by Number of Ratings", align = 'lr', booktabs = T,
        format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center")

# Trim and split title column
edx <- edx %>% mutate(title = str_trim(title)) %>%
  
  # split to title and year columns
  tidyr::extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  
  # Take debut date for series
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  
  # Remove temp column
  select(-title_temp)

# Plot mean rating by year of release
edx %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(rating=mean(rating)) %>%
  dplyr::group_by(year) %>%
  ggplot(aes(y=rating, x=year, group=1)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Release Year", y = "Mean Rating") + 
  plot_theme

# Plot number of ratings by release year
edx %>% dplyr::group_by(year) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::group_by(year) %>%
  ggplot(aes(year, count, group=1)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(breaks = seq(0, 800000, 200000), labels = seq(0, 800, 200)) +
  labs(x = "Release Year", y = "No. of Ratings (Thousands)")+ 
  plot_theme

# Convert timestamp column to date
edx <- edx %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))

edx %>% dplyr::group_by(review_date) %>%
  dplyr::summarize(rating = mean(rating)) %>%
  dplyr::group_by(review_date) %>%
  ggplot(aes(review_date, rating)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Date of Review", y = "Average Rating") +
  plot_theme

# Split edx set into test and train
set.seed(2020, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.5, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]
# Make sure userId and movieId in test set are also in train set
test_set <- temp %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set) 
train_set <- rbind(train_set, removed)
# Remove temporary files
rm(test_index, temp, removed) 

# Create a table and add the target RMSE based on project objective
rmse_objective <- 0.86490
rmse_results <- data.frame(Method = "Project objective", RMSE = "0.8649", Difference = "-")
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Calculate the overall average rating across all movies included in train set
mu_hat <- mean(train_set$rating)

# Calculate RMSE between each rating included in test set and the overall average
simple_rmse <- RMSE(test_set$rating, mu_hat)

# Estimate movie effect (b_i)
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  dplyr::summarise(b_i = mean(rating-mu_hat))

# Predict ratings with adjustment for movie effects
predicted_b_i <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred = mu_hat+b_i) %>%
  pull(pred)

# Calculate RMSE based on movie effects model
movie_rmse <- postResample(predicted_b_i, test_set$rating)[1]

# Estimate of user effect (b_u)
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  dplyr::group_by(userId) %>%
  dplyr::summarise(b_u = mean(rating - b_i - mu_hat)) %>%
  group_by(userId)
# Predict ratings with adjustment for movie and user effects
predicted_b_u <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
# Calculate RMSE based on user effects model
user_rmse <- postResample(predicted_b_u, test_set$rating)[1]

# Estimate the genre effect (b_g)
genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  dplyr::group_by(genres) %>%
  dplyr::summarise(b_g = mean(rating - mu_hat - b_i - b_u)) %>%
  dplyr::group_by(genres)

# Predict ratings with adjustment for movie, user and genre effects
predicted_b_g <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

# Calculate the RMSE with genre effects model
genre_rmse <- postResample(predicted_b_g, test_set$rating)[1]

# Estimate the release year effect
year_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(b_y = mean(rating - mu_hat - b_i - b_u - b_g)) %>%
  dplyr::group_by(year)

# Predict ratings with adjustment for the movie, user, genre and year effects
predicted_b_y <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y) %>%
  pull(pred)

# Calculate the RMSE based on year effect model
year_rmse <- postResample(predicted_b_y, test_set$rating)[1]

# Estimate the review date effect
date_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "year") %>%
  dplyr::group_by(review_date) %>%
  dplyr::summarise(b_r = mean(rating - mu_hat - b_i - b_u - b_g - b_y)) %>%
  dplyr::group_by(review_date)

# Predict ratings with adjustment for movie, user, genre, year and review date effects
predicted_b_r <- test_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "year") %>%
  left_join(date_avgs, by = "review_date") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
  pull(pred)

# Calculate the RMSE based on review date effect model
review_rmse <- postResample(predicted_b_r, test_set$rating)[1]

# Generate a sequence of values for lambda ranging from 4 to 5 with 0.1 increments
inc <- 0.1
lambdas <- seq(2, 5, inc)

# Regularise model, then predict ratings and calculate RMSE for each lambda
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>%
    dplyr::group_by(movieId) %>%
    dplyr::summarise(b_i = sum(rating - mu_hat)/(n()+abs(l)))
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    dplyr::group_by(userId) %>%
    dplyr::summarise(b_u = sum(rating - mu_hat - b_i)/(n()+abs(l)))
  b_g <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    dplyr::group_by(genres) %>%
    dplyr::summarise(b_g = sum(rating -  mu_hat - b_i - b_u)/(n()+abs(l))) 
  b_y <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(b_y = sum(rating - mu_hat - b_i - b_u - b_g)/(n()+abs(l)))
  b_r <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_y, by = "year") %>%
    dplyr::group_by(review_date) %>%
    dplyr::summarise(b_r = sum(rating - mu_hat - b_i - b_u - b_g - b_y)/(n()+abs(l)))
  predicted_ratings <- test_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_y, by="year") %>%
    left_join(b_r, by="review_date") %>%
    mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
    pull(pred)
  return(postResample(predicted_ratings, test_set$rating)[1])
})
# Assign optimal tuning parameter value of lambda
lambda <- lambdas[which.min(rmses)]
# Minimum RMSE
regularised_rmse <- min(rmses) 

# Use mutate function to mutate holdout dataset
final_holdout_test <- final_holdout_test %>% mutate(review_date = round_date(as_datetime(timestamp), unit = "week"))
final_holdout_test <- final_holdout_test %>% mutate(review_date = as_date(review_date))
final_holdout_test <- final_holdout_test %>% mutate(title = str_trim(title)) %>%
  # split title column to title and year
  extract(title, c("title_temp", "year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = F) %>%
  # take debut date for series
  mutate(year = if_else(str_length(year) > 4, as.integer(str_split(year, "-", simplify = T)[1]), as.integer(year))) %>%
  # replace NA titles
  mutate(title = if_else(is.na(title_temp), title, title_temp)) %>%
  # remove title_tmp column
  select(-title_temp)

# Use the edx dataset to model effects regularised with lambda
b_i <- edx %>%
  dplyr::group_by(movieId) %>%
  dplyr::summarise(b_i = sum(rating - mu_hat)/(n()+abs(lambda)))
b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  dplyr::group_by(userId) %>%
  dplyr::summarise(b_u = sum(rating - mu_hat - b_i)/(n()+abs(lambda)))
b_g <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  dplyr::group_by(genres) %>%
  dplyr::summarise(b_g = sum(rating - mu_hat - b_i - b_u)/(n()+abs(lambda))) 
b_y <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(b_y = sum(rating - mu_hat - b_i - b_u - b_g)/(n()+abs(lambda)))
b_r <- edx %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "year") %>%
  dplyr::group_by(review_date) %>%
  dplyr::summarise(b_r = sum(rating - mu_hat - b_i - b_u - b_g - b_y)/(n()+abs(lambda)))

# Predict holdout data ratings using final algorithm
predicted_ratings <- final_holdout_test %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_y, by="year") %>%
  left_join(b_r, by="review_date") %>%
  mutate(pred = mu_hat + b_i + b_u + b_g + b_y + b_r) %>%
  pull(pred)

# Calculate final holdout RMSE
holdout_rmse <- postResample(predicted_ratings, final_holdout_test$rating)[1]

# Add naive RMSE result to table
rmse_results <- rmse_results %>% rbind(c("Simple average", round(simple_rmse,6), round(simple_rmse-rmse_objective,6)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot movie effects distribution
movie_avgs %>%
  ggplot(aes(b_i)) +
  geom_histogram(bins = 25, color = I("black")) +
  labs(x="Movie effects (b_i)") + plot_theme

# Amend results table to include movie effect RMSE
rmse_results <- rmse_results %>% rbind(c("Movie effects (b_i)", round(movie_rmse, 6), round(movie_rmse-rmse_objective, 6)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot user effects distribution
user_avgs %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = I("black")) +
  labs(x="Distribution of user effect (b_u)") + plot_theme

# Amend results table to include user effect RMSE
rmse_results <- rmse_results %>% rbind(c("Movie + User effects (b_u)", round(user_rmse, 6), round(user_rmse-rmse_objective, 6)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot histogram for genre effect distribution
genre_avgs %>%
  ggplot(aes(b_g)) +
  geom_histogram(bins = 20, color = I("black")) +
  labs(x="Genre effects (b_g)") + plot_theme

# Amend RMSE result table to include genre effect
rmse_results <- rmse_results %>% rbind(c("Genre, movie, and user effect (b_g)", round(genre_rmse, 6), round(genre_rmse-rmse_objective, 6)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot histogram for release year effect distribution
year_avgs %>%
  ggplot(aes(b_y)) +
  geom_histogram(bins = 25, color = I("black")) +
  labs(x="Year effects (b_y)") + 
  plot_theme

# Amend RMSE result table with year effect model result
rmse_results <- rmse_results %>% rbind(c("Movie, User, Genre and Year effects (b_y)", round(year_rmse, 6), round(year_rmse-rmse_objective, 6)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot histogram of review date effect distribution
date_avgs %>%
  ggplot(aes(b_r)) +
  geom_histogram(bins = 25, color = I("black")) +
  labs(x="Review date effect (b_r)") + plot_theme

# Add RMSE to table for review date effect
rmse_results <- rmse_results %>% rbind(c("Movie, User, Genre, Year and Review Date effects (b_r)", round(review_rmse, 6), round(review_rmse-rmse_objective, 6)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = "hold_position")

# Plot a histogram of different parameters for lambda
data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  geom_hline(yintercept=min(rmses), linetype='dotted', col = "red") +
  annotate("text", x = lambda, y = min(rmses), label = lambda, vjust = -1, color = "red") +
  labs(x = "Lambda", y = "RMSE") + plot_theme

# Add regularised result to RMSE table
rmse_results <- rmse_results %>% rbind(c("Regularised Movie, User, Genre, Year and Review Date effect", round(regularised_rmse, 6), format(round(regularised_rmse-rmse_objective, 5), scientific = F)))
rmse_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex", linesep = "") %>%
  kable_styling(full_width = FALSE, position = "center")

#A table to compare RMSE results with project objective
final_results <- data.frame(Method = "Project objective", RMSE = "0.86490", Difference = "-") %>% rbind(c("Final model  holdout", round(holdout_rmse, 6), format(round(holdout_rmse-rmse_objective, 6), scientific = F)))
final_results %>%
  kable(align = 'lrr', booktabs = T, format = "latex") %>%
  kable_styling(full_width = FALSE, position = "center")

save.image(file="1.RData")

