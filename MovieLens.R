# Load necessary packages, if not available
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
library(lubridate)
library(knitr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# The zip-file is downloaded from the website, unzipped and data is
# loaded to data frames
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                title = as.character(title),
#                genres = as.character(genres))

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


#################
# Data Cleaning #
#################

# add new column for the rating date for both train and test set
edx <- edx %>% 
  mutate(rating_date=as_datetime(timestamp))  

validation <- validation %>% 
  mutate(rating_date=as_datetime(timestamp)) 


# add new column for the movie release year for both train and test set
# The year is extracted as value between the brackets
edx <- edx %>%
  mutate(movie_year=as.numeric(str_replace_all(str_replace_all(str_extract(title,
  "[/(]\\d{4}[/)]$"),"\\(",""),"\\)",""))) 

validation <- validation %>% 
  mutate(movie_year=as.numeric(str_replace_all(str_replace_all(str_extract(title,    "[/(]\\d{4}[/)]$"),"\\(",""),"\\)",""))) 

# add new column for the movie age for both train and test set
# movie age is the difference between movie release year in column movie_year
# and the year when the rating was done
edx <- edx %>% 
  mutate(movie_age=year(rating_date)-movie_year) 

validation <- validation %>% 
  mutate(movie_age=year(rating_date)-movie_year) 

# add new column for the amount of genre, different genres are separated with a pipe
# we will count the pipes and add 1 to have the correct value
edx <- edx %>% 
  mutate(amount_genres=as.numeric(str_count(genres,fixed("|")))+1)

validation <- validation %>% 
  mutate(amount_genres=as.numeric(str_count(genres,fixed("|")))+1)

mean(edx$amount_genres)

anyNA(edx) # Check for NA's in our table

####################
# Data Exploration #
####################

nrow(edx) # count rows in edx data set

edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

summary(edx)
average_rating <- mean(edx$rating) # calculation of average rating
median_rating <- median(edx$rating) # calculation of median rating

# Put all genres mentioned on the website into a vector
genres <- c("Action","Adventure","Animation","Children's","Comedy","Crime",
            "Documentary","Drama","Fantasy","Film-Noir","Horror","IMAX","Musical",
            "Mystery","Romance","Sci-Fi","Thriller","War","Western")

# Check how many genres there are
length(genres)

# take rating data and count how often one rating was done
analysis_rating <- edx %>%  
  group_by(rating) %>%  
  summarize(count_ratings = n()) %>% 
  arrange(desc(rating)) # sort in descending order

analysis_rating # show rating distribition

# plot the result into a histogram
 edx %>% 
    ggplot(aes(rating)) + 
    geom_histogram(binwidth=0.5, color="black", fill="blue") + 
    ggtitle("Rating Distribution")

# plot in histogram how often a MovieId occurs in the data set,
# this is the amount of available ratings for this movie. 
edx %>% 
  ggplot(aes(movieId)) + 
  geom_histogram(binwidth=0.5, color="black", fill="blue") + 
  ggtitle("Movie Distribution")

# count how often a movie was rated, including the average rating
analysis_movies <- edx %>% 
  group_by(title) %>% 
  summarize(num_ratings = n(), average_rating = mean(rating)) %>% 
  arrange(desc(average_rating))

# Filter for movies, which have only 1 rating done by 1 user
nrow(subset(analysis_movies, num_ratings == 1))

# Group by userId to show the amount of ratings done by this user
# including the average rating the user did for all his rated movies
users <- edx %>% 
  group_by(userId) %>% 
  summarize(num_ratings = n(), average_rating = mean(rating)) %>% 
  arrange(desc(num_ratings)) 

head(users)

users %>% 
  ggplot(aes(userId, num_ratings, color = average_rating)) +
  geom_point() +
  scale_color_gradientn(colours = c("#CC00FF", "#0066FF", 
       "#00FF66", "#CCFF00", "#FF0000")) +
  labs(x = "UserId", 
       y = "Amount of Ratings", 
       title = "Distribution of User Ratings", 
       color = "Rating Average")

# group by genres, which can also be a combination of different genres, 
# then count the amount and calculate the average rating
analysis_genres <- edx %>% 
  group_by(genres) %>% 
  summarize(num_ratings = n(), average_rating = mean(rating)) %>% 
  arrange(desc(average_rating)) 

analysis_genres <- analysis_genres %>% 
  mutate(amount_genres=as.numeric(str_count(genres,fixed("|")))+1)

head(analysis_genres) # display single genres

# Check for all movies, which have only been assigned to 1 specific genre
subset(analysis_genres, amount_genres == 1)

# Check how many movies have been release in the different year (movie year)
analysis_year <- edx %>%
  select(movieId, movie_year) %>% 
  group_by(movie_year) %>% 
  summarise(count = n())  %>% 
  arrange(movie_year)

analysis_year %>%
  ggplot(aes(x = movie_year, y = count)) +
  geom_line(color="blue") +
  xlim(min(edx$movie_year),max(edx$movie_year))

# Group by movie year again and include the average rating to see the
# average rating for every release year of the different movies
analysis_year2 <- edx %>% 
  group_by(movie_year) %>% 
  summarize(num_ratings = n(), average_rating = mean(rating)) %>% 
  arrange(desc(num_ratings)) 

ggplot(analysis_year2) + 
  geom_point(aes(x=movie_year, y=average_rating, colour=average_rating))+
  labs(title="Average rating based on years",
       x = "Year",
       y = "Average Ratings",
       colour = "Average Rating") +
  theme(axis.line.x = element_line(size=2))

# Check the movie age to the time when the rating was done and show the amount
# of rating done for a specific age and the average rating
analysis_movie_age <- edx %>% 
  group_by(movie_age) %>% 
  summarize(num_ratings = n(), average_rating = mean(rating)) %>% 
  arrange(desc(num_ratings))

analysis_movie_age %>% 
  ggplot(aes(movie_age, average_rating)) +
  geom_line() +
  labs(x = "Movie Age", y = "Average Rating", title = "Average rating based on movie age")

###########
# Modelin #
###########
mu <- mean(edx$rating) # calculate mean and save in 'mu'

# create first simple model and compare predicted rating = mu with real ratings
naive_rmse <- RMSE(validation$rating,mu) 

# Create tibble to show all results and add our first RMSE
rmse_results <- tibble(method = "Usage of average rating", RMSE = naive_rmse) 
knitr::kable(rmse_results, caption = "Overview of the RMSE results")

# include movie effect into the model
movie_averages <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_averages %>% 
  ggplot(aes(b_i)) +
  geom_histogram(binwidth=0.5, color="black", fill="blue") 

# predict ratings based on the new model
predicted_ratings <- mu + 
  validation %>% 
  left_join(movie_averages, by='movieId') %>%
  .$b_i

# create model and compare predicted rating with real ratings
model_movie_effects <- RMSE(predicted_ratings, validation$rating)

# add movie effects result to the RMSE table
rmse_results <- bind_rows(rmse_results, 
                          tibble(method = "Adding movie effects",
                          RMSE = model_movie_effects)) 
knitr::kable(rmse_results, caption = "Overview of the RMSE results")

# include user effect into the model
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(binwidth=0.2, color="black", fill="blue")

user_averages <- edx %>% 
  left_join(movie_averages, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# predict ratings based on the new model
predicted_ratings <- validation %>% 
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# create model and compare predicted rating with real ratings
model_user_effects <- RMSE(predicted_ratings, validation$rating)

# add user effects result to the RMSE table
rmse_results <- bind_rows(rmse_results, 
                          tibble(method = "Adding user effects",
                          RMSE = model_user_effects)) 
knitr::kable(rmse_results, caption = "Overview of the RMSE results")

# include movie age effect into the model
movie_age_averages <- edx %>% 
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  group_by(movie_age) %>%
  summarize(b_a = mean(rating - mu - b_i - b_u))

# predict ratings based on the new model
predicted_ratings <- validation %>% 
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  left_join(movie_age_averages, by='movie_age') %>%
  mutate(pred = mu + b_i + b_u + b_a) %>%
  .$pred

# create model and compare predicted rating with real ratings
model_movie_age_effects <- RMSE(predicted_ratings, validation$rating)

# add movie age effects result to the RMSE table
rmse_results <- bind_rows(rmse_results, 
                          tibble(method = "Adding movie age effects",
                          RMSE = model_movie_age_effects)) 
knitr::kable(rmse_results, caption = "Overview of the RMSE results")

# include movie release year effect into the model
movie_year_averages <- edx %>% 
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  left_join(movie_age_averages, by='movie_age') %>%
  group_by(movie_year) %>%
  summarize(b_y = mean(rating - mu - b_i - b_u - b_a))

# predict ratings based on the new model
predicted_ratings <- validation %>% 
  left_join(movie_averages, by='movieId') %>%
  left_join(user_averages, by='userId') %>%
  left_join(movie_age_averages, by='movie_age') %>%
  left_join(movie_year_averages, by='movie_year') %>%
  mutate(pred = mu + b_i + b_u + b_a + b_y) %>%
  .$pred

# create model and compare predicted rating with real ratings
model_movie_year_effects <- RMSE(predicted_ratings, validation$rating)

# add movie release year effects result to the RMSE table
rmse_results <- bind_rows(rmse_results, 
                          tibble(method = "Adding movie release year effects",
                          RMSE = model_movie_year_effects)) 
knitr::kable(rmse_results, caption = "Overview of the RMSE results")


# clear tables to save space
rm(analysis_genres, analysis_movie_age, analysis_movies, analysis_rating, analysis_year, analysis_year2, 
   movie_age_averages, movie_averages, movie_year_averages, user_averages, users)


