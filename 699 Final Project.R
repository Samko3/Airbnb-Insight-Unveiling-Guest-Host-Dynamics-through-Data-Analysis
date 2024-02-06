#Step 1 - Data Preparation & Exploration
library(readr)
buenos <- read_csv("C:/Users/kvksa/Dropbox/PC/Desktop/AD699/buenos.csv")
View(buenos)
library(tidyverse)
monserrat<-filter(buenos, neighbourhood_cleansed=="Monserrat")

#I Missing Values
anyNA(monserrat)
library(naniar)
MissingValuesSummary<-miss_var_summary(monserrat)

#Removing variables with more than 50% missing values
monserrat.1<-subset(monserrat, select = -c(neighbourhood_group_cleansed,
                                           bathrooms, calendar_updated, license))

#II Summary Statistics
#Removing $ and % symbols and changing type to numeric
monserrat.1$price <- gsub("[^0-9.]", "", monserrat.1$price)
monserrat.1$price<-as.numeric(monserrat.1$price)
monserrat.1$host_response_rate <- gsub("[^0-9.]", "", monserrat.1$host_response_rate)
monserrat.1$host_response_rate<-as.numeric(monserrat.1$host_response_rate)
monserrat.1$host_acceptance_rate <- gsub("[^0-9.]", "", monserrat.1$host_acceptance_rate)
monserrat.1$host_acceptance_rate<-as.numeric(monserrat.1$host_acceptance_rate)
str(monserrat.1)
monserrat.nb <- monserrat.1

#Considering only numeric variables
numeric_vars <- monserrat.1 %>% select_if(is.numeric)
numeric_vars <- subset(numeric_vars, select = -c(id, scrape_id, host_id))

#Computing mean, sd, min, max and missing values
summary_stats<-data.frame(mean = sapply(numeric_vars[,c(1,8,10,23,32)], function(x) round(mean(x, na.rm = TRUE), 2)),
                          sd = sapply(numeric_vars[,c(1,8,10,23,32)], function(x) round(sd(x, na.rm = TRUE), 2)),
                          min = sapply(numeric_vars[,c(1,8,10,23,32)], function(x) round(min(x, na.rm = TRUE), 2)),
                          max = sapply(numeric_vars[,c(1,8,10,23,32)], function(x) round(max(x, na.rm = TRUE), 2)),
                          median = sapply(numeric_vars[,c(1,8,10,23,32)], function(x) round(median(x, na.rm = TRUE), 2)))

#III Data Visualization
#Removing unnecessary variables
Monserrat1 <- subset(monserrat, select = -c(id,listing_url,scrape_id,last_scraped,name,description,neighborhood_overview,
                                            picture_url,host_id,host_url,host_name,host_since, host_location,
                                            host_about,host_thumbnail_url,host_picture_url,host_neighbourhood,
                                            neighbourhood,host_neighbourhood,neighbourhood_group_cleansed,
                                            latitude,longitude,amenities,calendar_updated,bathrooms,calendar_updated, 
                                            license,host_verifications,calendar_last_scraped,first_review,last_review,
                                            source))

#Checking for NA Values
anyNA(Monserrat1)

#Dropping NA values
Monserrat2<-Monserrat1 %>% drop_na()

#Changing type to as.factor
Monserrat2$host_response_time<-as.factor(Monserrat2$host_response_time)
Monserrat2$host_is_superhost<-as.factor(Monserrat2$host_is_superhost)
Monserrat2$host_has_profile_pic<-as.factor(Monserrat2$host_has_profile_pic)
Monserrat2$host_identity_verified<-as.factor(Monserrat2$host_identity_verified)
Monserrat2$has_availability<-as.factor(Monserrat2$has_availability)
Monserrat2$property_type<-as.factor(Monserrat2$property_type)
Monserrat2$room_type<-as.factor(Monserrat2$room_type)
Monserrat2$instant_bookable<-as.factor(Monserrat2$instant_bookable)

#Removing $ and % symbols and changing their type to numeric
Monserrat2$price <- gsub("[^0-9.]", "", Monserrat2$price)
Monserrat2$price<-as.numeric(Monserrat2$price)
Monserrat2$host_response_rate <- gsub("[^0-9.]", "", Monserrat2$host_response_rate)
Monserrat2$host_response_rate<-as.numeric(Monserrat2$host_response_rate)
Monserrat2$host_acceptance_rate <- gsub("[^0-9.]", "", Monserrat2$host_acceptance_rate)
Monserrat2$host_acceptance_rate<-as.numeric(Monserrat2$host_acceptance_rate)

#Removing text from bathrooms_text
Monserrat2$bathrooms_text <- str_extract(Monserrat2$bathrooms_text, "\\d+")
Monserrat2$bathrooms_text<-as.numeric(Monserrat2$bathrooms_text)

#Dropping NA from host_response_rate and host_acceptance_rate
Monserrat3<-Monserrat2 %>% drop_na(host_response_rate,host_acceptance_rate)

library(ggplot2)
#Creating a barplot to show the Host Response Time and Count
ggplot(Monserrat3, aes(x = host_response_time)) + 
  geom_bar(fill = "lightgreen", width = 0.5) +
  xlab("Host Response Time") +
  ylab("Count") +
  ggtitle("Host Response Time")

#Creating a barplot to show to the count of different types of rooms
ggplot(data = Monserrat3, aes(x = room_type, fill = room_type)) + 
  geom_bar(width = 0.5, position = "dodge") +
  ggtitle("The Number of Rooms of Each Type") +
  scale_fill_manual(values = c("#3498DB", "#2ECC71", "#F1C40F", "#FF69B4"))

#Creating a histogram to show the distribution of Review Scores Rating
ggplot(Monserrat3, aes(x = review_scores_rating)) + 
  geom_histogram(binwidth = 0.15, fill = "lightblue") +
  xlab("Review Scores Rating") +
  ylab("Count") +
  ggtitle("Distribution of Review Scores Rating")

#Creating a scatterplot to show the relationship  between Minimum Nights and Room Type 
ggplot(data = Monserrat3, aes(x = room_type, y = minimum_nights, color = room_type)) +
  geom_point() +
  xlab("Room Type") +
  ylab("Minimum Nights") +
  ggtitle("Minimum Nights Required for Each Room Type") +
  scale_color_manual(values=c("#FF5733", "#FFC300", "#2ECC71", "#C70039"))

#Creating histograms to show the distribution of Review Scores for Cleanliness by Room Type
ggplot(data = Monserrat3, aes(x = review_scores_cleanliness, fill = room_type)) +
  geom_histogram(binwidth = 1) +
  xlab("Review Scores Cleanliness") +
  ylab("Count") +
  ggtitle("Distribution of Review Scores Cleanliness by Room Type") +
  facet_wrap(~room_type, ncol = 2)

#Creating a boxplot to show the distribution of the number of days an Airbnb is available for rent at different intervals
availability_data <- Monserrat3[, c("availability_30", "availability_60", "availability_90", "availability_365")]
availability_data_melt <- reshape2::melt(availability_data)
ggplot(availability_data_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  xlab("Availability") +
  ylab("Number of days available") +
  ggtitle("Availability of Airbnb listings in Montserrat") +
  scale_fill_manual(values = c("#FFA07A", "#87CEFA", "#7FFF00", "#DA70D6"))

#IV Mapping
#Checking for NA values in latitude and longitude
anyNA(monserrat.1$latitude)
anyNA(monserrat.1$longitude)

library(leaflet)
#Plotting a Map
map <- leaflet() %>% addTiles() %>% 
  addCircles(lng= monserrat.1$longitude , lat= monserrat.1$latitude) %>%
  addProviderTiles(providers$Stamen.Toner)
map

#V Word Cloud
library(tidytext)
library(wordcloud)
library(tidyverse)

#Removing NA values and filtering the data
monserrat<- monserrat %>% filter(!is.na(neighborhood_overview))
neighborhood_overview_data <- monserrat %>% select(neighborhood_overview)
View(neighborhood_overview_data)

#Making individual words
tidy_monserrat <- neighborhood_overview_data %>% 
  unnest_tokens(word, neighborhood_overview)

#Removing stop words and making a Word Cloud
tidy_monserrat %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#Step 2 - Prediction
#Removing the NA values
monserrat.1$review_scores_rating <- ifelse(is.na(monserrat.1$review_scores_rating), mean(monserrat.1$review_scores_rating, na.rm=TRUE), monserrat.1$review_scores_rating)
monserrat.1$review_scores_accuracy <- ifelse(is.na(monserrat.1$review_scores_accuracy), mean(monserrat.1$review_scores_accuracy, na.rm=TRUE), monserrat.1$review_scores_accuracy)
monserrat.1$review_scores_cleanliness <- ifelse(is.na(monserrat.1$review_scores_cleanliness), mean(monserrat.1$review_scores_cleanliness, na.rm=TRUE), monserrat.1$review_scores_cleanliness)
monserrat.1$review_scores_checkin <- ifelse(is.na(monserrat.1$review_scores_checkin), mean(monserrat.1$review_scores_checkin, na.rm=TRUE), monserrat.1$review_scores_checkin)
monserrat.1$review_scores_communication <- ifelse(is.na(monserrat.1$review_scores_communication), mean(monserrat.1$review_scores_communication, na.rm=TRUE),monserrat.1$review_scores_communication)
monserrat.1$review_scores_location <- ifelse(is.na(monserrat.1$review_scores_location), mean(monserrat.1$review_scores_location, na.rm=TRUE),monserrat.1$review_scores_location)
monserrat.1$review_scores_value <- ifelse(is.na(monserrat.1$review_scores_value), mean(monserrat.1$review_scores_value, na.rm=TRUE), monserrat.1$review_scores_value)
monserrat.1$reviews_per_month<- ifelse(is.na(monserrat.1$reviews_per_month), mean(monserrat.1$reviews_per_month, na.rm=TRUE), monserrat.1$reviews_per_month)

view(monserrat.1)
#Checking Missing Value Summary
MissingValuesSummary2<-miss_var_summary(monserrat.1)
MissingValuesSummary2

#Description 
str(monserrat.1)
glimpse(monserrat.1)

#Bathroom_text is not numeric since it has "bath" attached to the numeric value.
monserrat.1$bathrooms_text <- gsub("[^0-9.]+", "", monserrat.1$bathrooms_text)

#Imputing missing values as 1 with the assumption that all airbnb rentals will have 1 bathroom 
monserrat.1$bathrooms_text[is.na(monserrat.1$bathrooms_text)] <- 1
monserrat.1$bathrooms_text <- as.numeric(monserrat.1$bathrooms_text)

#Imputing missing values as 1 with the assumption that all airbnb rentals will have 1 bedroom
monserrat.1$bedrooms[is.na(monserrat.1$bedrooms)] <- 1
monserrat.1$bedrooms <- as.numeric(monserrat.1$bedrooms)

view(monserrat.1)
MissingValuesSummary3<-miss_var_summary(monserrat.1)
MissingValuesSummary3

#Creating a multiple regression model with the outcome variable price
monserrat.2<-subset(monserrat.1, select= -c (id, listing_url, scrape_id, last_scraped, source, name,
                                             description, neighborhood_overview, picture_url, host_id,
                                             host_url, host_name, host_since, host_location, host_about,
                                             host_thumbnail_url,host_neighbourhood, host_verifications,host_picture_url,
                                             host_has_profile_pic, neighbourhood, neighbourhood_cleansed,
                                             latitude, longitude, minimum_minimum_nights, maximum_minimum_nights,
                                             minimum_maximum_nights, maximum_maximum_nights, minimum_nights_avg_ntm,
                                             maximum_nights_avg_ntm, availability_30, availability_90,
                                             availability_365, calendar_last_scraped, first_review, last_review))

colnames(monserrat.2)
length(unique(monserrat.2$minimum_nights))
length(unique(monserrat.2$maximum_nights))
length(unique(monserrat.2$amenities))

#Amenities has 831 unique values so removing it 
monserrat.2<-subset(monserrat.2, select= -c (amenities))
monserrat.2

#Changing type to as.factor
monserrat.2$host_is_superhost <- as.factor(monserrat.2$host_is_superhost)
monserrat.2$host_identity_verified <- as.factor(monserrat.2$host_identity_verified)
monserrat.2$property_type <- as.factor(monserrat.2$property_type)
monserrat.2$room_type <- as.factor(monserrat.2$room_type)
monserrat.2$has_availability <- as.factor(monserrat.2$has_availability)
monserrat.2$instant_bookable <- as.numeric(monserrat.2$instant_bookable)
str(monserrat.2)

#Converting blank spaces to NA and then to 0
monserrat.2$host_response_rate <- gsub("\\D", "", monserrat.2$host_response_rate)
monserrat.2$host_response_rate[monserrat.2$host_response_rate == ""] <- NA
monserrat.2$host_response_rate <- ifelse(is.na(monserrat.2$host_response_rate), 0, monserrat.2$host_response_rate)
monserrat.2$host_response_rate <- as.numeric(monserrat.2$host_response_rate)

monserrat.2$host_acceptance_rate <- gsub("\\D", "", monserrat.2$host_acceptance_rate)
monserrat.2$host_acceptance_rate[monserrat.2$host_acceptance_rate == ""] <- NA
monserrat.2$host_acceptance_rate <- ifelse(is.na(monserrat.2$host_acceptance_rate), 0, monserrat.2$host_acceptance_rate)
monserrat.2$host_acceptance_rate <- as.numeric(monserrat.2$host_acceptance_rate)

view(monserrat.2)

#Changing price to log form 
monserrat.2$log_price <- log(monserrat.2$price)
monserrat.2<-subset(monserrat.2,select = -c(price))

#Splitting the data into Training and Validation
set.seed(150)
train.index<-sample(c(1:nrow(monserrat.2)),nrow(monserrat.2)*0.6)
train.df<- monserrat.2[train.index,]
validation.df<-monserrat.2[-train.index,]

TrainingNumeric <- train.df[, sapply(train.df, is.numeric)]
ValidationNumeric <- validation.df[, sapply(validation.df, is.numeric)]

#Correlation
Correlation <- cor(TrainingNumeric) 
view(Correlation)

train.df<- subset(train.df, select = -c(host_total_listings_count, number_of_reviews_ltm, review_scores_accuracy, host_listings_count, reviews_per_month, beds))
validation.df<-subset(validation.df,select=-c(host_total_listings_count, number_of_reviews_ltm, review_scores_accuracy, host_listings_count, reviews_per_month, beds))

#This includes factors and numeric columns to feed into our MLR which is what we are looking for
train.df1 <- train.df[, sapply(train.df, function(x) is.numeric(x) | is.factor(x))]
validation.df.1 <- validation.df[, sapply(validation.df, function(x) is.numeric(x) | is.factor(x))]
view(train.df1)

str(train.df1)

#This model is the one we will use for our output 
model <- lm(log_price ~ ., data = train.df1)
summary(model)

#Backward Elimination using log_price as the response variable
model_backward <- step(lm(data=train.df1, formula = log_price~ .), direction = 'backward')
summary(model_backward)

#Generating a prediction using Backward Elimination
library(forecast)
TrainingPreds<- predict(model_backward, train.df1)
accuracy(TrainingPreds, train.df1$log_price)
validation.df.1 <- filter(validation.df.1, property_type!="Cave" & property_type!="Room in boutique hotel")
ValidPreds <- predict(model_backward, validation.df.1)
accuracy(ValidPreds, validation.df.1$log_price)

#Step 3 - Classification

#I K-Nearest Neighbors
#Filtering out the amenity 
amenities <- monserrat[grepl("Coffee maker: drip coffee maker", monserrat$amenities), ]
amenities <- data.frame(amenities)

#Removing the unnecessary variables
monserrat_filtered <- subset(monserrat, select = -c(neighborhood_overview, neighbourhood, host_about, 
                                                      calculated_host_listings_count_shared_rooms, calculated_host_listings_count_private_rooms, 
                                                      calculated_host_listings_count_entire_homes, calculated_host_listings_count, instant_bookable,  
                                                      calendar_last_scraped, availability_365, availability_90, availability_60, availability_30, 
                                                      maximum_nights_avg_ntm, minimum_nights_avg_ntm, maximum_maximum_nights, minimum_maximum_nights, 
                                                      maximum_minimum_nights, minimum_minimum_nights, longitude, latitude, neighbourhood_cleansed, 
                                                      host_has_profile_pic, host_total_listings_count, host_listings_count, host_neighbourhood, 
                                                      host_picture_url, host_thumbnail_url, host_since, host_name, host_url, host_id, picture_url, 
                                                      description, name, source, last_scraped, scrape_id, listing_url, id, bathrooms_text, host_location, 
                                                      review_scores_location, review_scores_value, number_of_reviews_l30d, number_of_reviews_ltm, 
                                                      has_availability, maximum_nights, minimum_nights, first_review, last_review, host_verifications, 
                                                      neighbourhood_group_cleansed, bathrooms, calendar_updated, license))

#Convert "true" and "false" to 1 and 0
monserrat_filtered$host_is_superhost[monserrat_filtered$host_is_superhost=="TRUE"] <- 1
monserrat_filtered$host_is_superhost[monserrat_filtered$host_is_superhost=="FALSE"] <- 0
monserrat_filtered$host_is_superhost <- as.factor(monserrat_filtered$host_is_superhost)

#Checking the missing value summary
MissingValues <- miss_var_summary(monserrat_filtered)

#Removing the NA value rows in the dataset
monserrat_filtered <- monserrat_filtered %>% drop_na(review_scores_accuracy)

#Adjusting the NA value rows with mean of the column  
mean_bedrooms <- mean(monserrat_filtered$bedrooms, na.rm = TRUE)
monserrat_filtered$bedrooms <- ifelse(is.na(monserrat_filtered$bedrooms), mean_bedrooms, monserrat_filtered$bedrooms)
mean_beds <- mean(monserrat_filtered$beds, na.rm = TRUE)
monserrat_filtered$beds <- ifelse(is.na(monserrat_filtered$beds), mean_beds, monserrat_filtered$beds)

#Checking the missing value summary
MissingValues1 <- miss_var_summary(monserrat_filtered)

#Removing "N/A" from the dataset
monserrat_filtered <- monserrat_filtered %>% 
  filter(!grepl("N/A", host_response_rate))

monserrat_filtered <- monserrat_filtered %>% 
  filter(!grepl("N/A", host_acceptance_rate))

#Converting the variables to numeric
monserrat_filtered$host_response_rate <- as.numeric(gsub("%", "", monserrat_filtered$host_response_rate))
monserrat_filtered$host_acceptance_rate <- as.numeric(gsub("%", "", monserrat_filtered$host_acceptance_rate))
monserrat_filtered$price <- as.numeric(gsub("[^0-9.]", "", monserrat_filtered$price))

#Setting the seed value and Partitioning the data(60% training, 40% validation)
set.seed(150)
train_index <- sample(nrow(monserrat_filtered), round(0.6 * nrow(monserrat_filtered)))
train <- monserrat_filtered[train_index, ]
validation <- monserrat_filtered[-train_index, ]
test <- (amenities)
test <- data.frame(test)

#Splitting the training data into two subsets based on host_is_superhost
library(tidyverse)
superhost <- train[train$host_is_superhost == 1, ]
not_superhost <- train[train$host_is_superhost == 0, ]

#Performing t-tests on each numeric variable
num_vars <- c("host_response_rate", "host_acceptance_rate", "accommodates", "beds",
              "bedrooms", "price", "number_of_reviews", "review_scores_accuracy", "review_scores_cleanliness", 
              "review_scores_checkin", "review_scores_communication", "review_scores_rating", 
              "reviews_per_month")

for (var in num_vars) {
  ttest <- t.test(superhost[[var]], not_superhost[[var]])
  cat(var, "\n")
  cat("t-statistic:", ttest$statistic, "\n")
  cat("p-value:", ttest$p.value, "\n\n")
}

library(dplyr)
#Variables to drop
vars_to_drop <- c("accommodates", "beds", "bedrooms","reviews_per_month", "price")

#Dropping variables from train set
train <- train %>% select(-vars_to_drop)

#Dropping variables from validation set
validation <- validation %>% select(-vars_to_drop)

#Dropping variables from test set
test <- test %>% select(-vars_to_drop)

library(caret)
#Dropping unnecessary variables in "test"
test <- subset(test, select = -c(neighborhood_overview, neighbourhood, host_about, 
                                 calculated_host_listings_count_shared_rooms, calculated_host_listings_count_private_rooms, 
                                 calculated_host_listings_count_entire_homes, calculated_host_listings_count, instant_bookable,  
                                 calendar_last_scraped, availability_365, availability_90, availability_60, availability_30, 
                                 maximum_nights_avg_ntm, minimum_nights_avg_ntm, maximum_maximum_nights, minimum_maximum_nights, 
                                 maximum_minimum_nights, minimum_minimum_nights, longitude, latitude, neighbourhood_cleansed, 
                                 host_has_profile_pic, host_total_listings_count, host_listings_count, host_neighbourhood, 
                                 host_picture_url, host_thumbnail_url, host_since, host_name, host_url, host_id, picture_url, 
                                 description, name, source, last_scraped, scrape_id, listing_url, id, bathrooms_text, host_location, 
                                 review_scores_location, review_scores_value, number_of_reviews_l30d, number_of_reviews_ltm, 
                                 has_availability, maximum_nights, minimum_nights, first_review, last_review, host_verifications,
                                 neighbourhood_group_cleansed, bathrooms, calendar_updated, license))

#Checking the missing variable summary
MissingValues_test <- miss_var_summary(test)

#Removing "N/A" from the test dataset
test <- test %>% 
  filter(!grepl("N/A", host_response_rate))

#Removing the NA value rows in test dataset
test <- test %>% drop_na(review_scores_rating)

#Converting the variables to numeric 
test$host_response_rate <- as.numeric(gsub("%", "", test$host_response_rate))
test$host_acceptance_rate <- as.numeric(gsub("%", "", test$host_acceptance_rate))

#Convert "true" and "false" to 1 and 0 for test
test$host_is_superhost[test$host_is_superhost=="TRUE"] <- 1
test$host_is_superhost[test$host_is_superhost=="FALSE"] <- 0
test$host_is_superhost <- as.factor(test$host_is_superhost)

#Initializing normalization
train_num <- train
validation_num <- validation
test_num <- test

#Using the preProcess function to normalize the data
preproc <- preProcess(train_num[, c(2, 3, 9, 10, 11, 12, 13, 14)], method = c("center", "scale"))

#Applying the normalization to the training and validation sets
train_num[, c(2, 3, 9, 10, 11, 12, 13, 14)] <- predict(preproc, train[, c(2, 3, 9, 10, 11, 12, 13, 14)])
validation_num[, c(2, 3, 9, 10, 11, 12, 13, 14)] <- predict(preproc, validation[, c(2, 3, 9, 10, 11, 12, 13, 14)])
test_num[, c(2, 3, 9, 10, 11, 12, 13, 14)] <- predict(preproc, test[, c(2, 3, 9, 10, 11, 12, 13, 14)])

#Select the k-nn package
library(FNN)
set.seed(150)

#Use the knn() function to generate a predicted classification for host_is_superhost
nn <- knn(train = train_num[, c(2, 3, 9, 10, 11, 12, 13, 14)], 
          test = test_num[, c(2, 3, 9, 10, 11, 12, 13, 14)],
          cl = train_num$host_is_superhost, k = 8)
nn_near <- row.names(train_num)[attr(nn, "nn.index")]
neighbour <- monserrat_filtered[c(144, 124, 219, 173, 165, 126, 232, 192),]

#II Naive Bayes
nb.df<-subset(monserrat.nb, select=c(host_is_superhost, host_identity_verified,
                                    property_type, room_type, accommodates,
                                    price, has_availability, number_of_reviews,
                                    number_of_reviews_ltm, number_of_reviews_l30d,
                                    review_scores_rating, review_scores_accuracy, review_scores_cleanliness,
                                    review_scores_checkin, review_scores_communication,
                                    review_scores_location, review_scores_value, instant_bookable,
                                    reviews_per_month))

#Checking  for and dealing with missing values
mvs.nb.df<-miss_var_summary(nb.df)
nb.df.1<-nb.df %>% drop_na(review_scores_value)
mvs.nb.df.1<-miss_var_summary(nb.df.1)

#Changing character type variable to factor
nb.df.1$property_type<-as.factor(nb.df.1$property_type)
nb.df.1$room_type<-as.factor(nb.df.1$room_type)

#Binning numeric variables
library(arules)
nb.df.2<-nb.df.1

table(nb.df.1$accommodates)
nb.df.2$accommodates<-discretize(nb.df.2$accommodates, method="frequency", breaks = 3,
                                 labels=c("1", "2", "3 or more"))
table(nb.df.2$accommodates)

table(nb.df.1$price)
nb.df.2$price<-discretize(nb.df.2$price, method="frequency", breaks = 3,
                          labels=c("cheap","appropriate","costly"))
table(nb.df.2$price)

table(nb.df.1$number_of_reviews)
nb.df.2$number_of_reviews<-discretize(nb.df.2$number_of_reviews, method="frequency", breaks = 3,
                                      labels=c("Very few",  "few", "good number"))
table(nb.df.2$number_of_reviews)

table(nb.df.1$number_of_reviews_ltm)
nb.df.2$number_of_reviews_ltm<-discretize(nb.df.2$number_of_reviews_ltm, method="frequency", breaks = 3,
                                          labels=c("very few","few", "a lot"))
table(nb.df.2$number_of_reviews_ltm)

table(nb.df.1$number_of_reviews_l30d)
nb.df.2$number_of_reviews_l30d<-discretize(nb.df.2$number_of_reviews_l30d, method="frequency", breaks = 2,
                                           labels=c("0","1 or more"))
table(nb.df.2$number_of_reviews_l30d)

table(nb.df.1$review_scores_rating)
nb.df.2$review_scores_rating<-discretize(nb.df.2$review_scores_rating, method="frequency", breaks = 3,
                                         labels=c("bad","good", "v good"))
table(nb.df.2$review_scores_rating)

table(nb.df.1$review_scores_accuracy)
nb.df.2$review_scores_accuracy<-discretize(nb.df.2$review_scores_accuracy, method="frequency", breaks = 3,
                                           labels=c("bad","good", "v good"))
table(nb.df.2$review_scores_accuracy)

table(nb.df.1$review_scores_cleanliness)
nb.df.2$review_scores_cleanliness<-discretize(nb.df.2$review_scores_cleanliness, method="frequency", breaks = 3,
                                              labels=c("bad","good", "v good"))
table(nb.df.2$review_scores_cleanliness)

table(nb.df.1$review_scores_checkin)
nb.df.2$review_scores_checkin<-discretize(nb.df.2$review_scores_checkin, method="frequency", breaks = 2,
                                          labels=c("bad","good"))
table(nb.df.2$review_scores_checkin)

table(nb.df.1$review_scores_communication)
nb.df.2$review_scores_communication<-discretize(nb.df.2$review_scores_communication, method="frequency", breaks = 2,
                                                labels=c("bad","good"))
table(nb.df.2$review_scores_communication)

table(nb.df.1$review_scores_location)
nb.df.2$review_scores_location<-discretize(nb.df.2$review_scores_location, method="frequency", breaks = 2,
                                           labels=c("bad","good"))
table(nb.df.2$review_scores_location)

table(nb.df.1$review_scores_value)
nb.df.2$review_scores_value<-discretize(nb.df.2$review_scores_value, method="frequency", breaks = 2,
                                        labels=c("bad","good"))
table(nb.df.2$review_scores_value)

table(nb.df.1$reviews_per_month)
nb.df.2$reviews_per_month<-discretize(nb.df.2$reviews_per_month, method="frequency", breaks = 2,
                                      labels=c("less","more"))
table(nb.df.2$reviews_per_month)

#Changing values of instant_bookable column (to avoid confusion)
nb.df.2$instant_bookable[nb.df.2$instant_bookable == "TRUE"] <- "YES"
nb.df.2$instant_bookable[nb.df.2$instant_bookable == "FALSE"] <- "NO"

#Changing variable type of instant_bookable to numeric
nb.df.2$instant_bookable<-as.factor(nb.df.2$instant_bookable)

#Creating training and validating set
set.seed(150)
train.index<-sample(c(1:nrow(nb.df.2)), nrow(nb.df.2)*0.6) #index for data partition
trainingset<-nb.df.2[train.index,] #60% data in training set
table(trainingset$instant_bookable)
validatingset<-nb.df.2[-train.index,] #40% data in validating set
table(validatingset$instant_bookable)

#Checking for distribution
library(dplyr)
prop.table(table(trainingset$instant_bookable,trainingset$host_is_superhost))
prop.table(table(trainingset$instant_bookable,trainingset$host_identity_verified))
prop.table(table(trainingset$instant_bookable,trainingset$property_type))
prop.table(table(trainingset$instant_bookable,trainingset$room_type))
prop.table(table(trainingset$instant_bookable,trainingset$accommodates))
prop.table(table(trainingset$instant_bookable,trainingset$price))
prop.table(table(trainingset$instant_bookable,trainingset$has_availability))
prop.table(table(trainingset$instant_bookable,trainingset$number_of_reviews))
prop.table(table(trainingset$instant_bookable,trainingset$number_of_reviews_ltm))
prop.table(table(trainingset$instant_bookable,trainingset$number_of_reviews_l30d))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_rating))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_accuracy))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_cleanliness))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_checkin))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_communication))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_location))
prop.table(table(trainingset$instant_bookable,trainingset$review_scores_value))
prop.table(table(trainingset$instant_bookable,trainingset$reviews_per_month))

#Verifying proportional bar plots for some variables with instant_bookable
library(ggplot2)
ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_rating)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Rating Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Rating Review Scores")

ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_accuracy)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Accuracy Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Accuracy Review Scores")

ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_cleanliness)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Cleanliness Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Cleanliness Review Scores")

ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_checkin)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Checkin Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Checkin Review Scores")

ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_communication)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Communication Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Communication Review Scores")

ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_location)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Location Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Location Review Scores")

ggplot(trainingset, aes(x=instant_bookable, fill=review_scores_value)) + geom_bar(position = "fill")+
  labs(x = "Instant Bookable", y = "Proportion", fill = "Value Review Scores") + 
  ggtitle("Proportional Bar Plot of Instant Bookable over Value Review Scores")

#Removing variables with similar proportions
trainingset <- subset(trainingset, select=-c(review_scores_checkin,review_scores_location))
validatingset <- subset(validatingset, select=-c(review_scores_checkin,review_scores_location))

#Creating naive bayes model
library(e1071)
library(caret)
instantbookable.nb<-naiveBayes(instant_bookable~., data=trainingset)
instantbookable.nb

#Checking for training set and validating set accuracy
training_pred<-predict(instantbookable.nb, newdata = trainingset)
confusionMatrix(training_pred, trainingset$instant_bookable)

validating_pred<-predict(instantbookable.nb, newdata = validatingset)
confusionMatrix(validating_pred, validatingset$instant_bookable)

#Finding probabilities for each record in the validating set
pred.prob <- data.frame(prob=predict(instantbookable.nb, newdata = validatingset, type = "raw")) ## predict probabilities
pred.class <- data.frame(predictions=predict(instantbookable.nb, newdata = validatingset))

validatingset_with_preds <- data.frame(validatingset, pred.class, pred.prob)
yes <- validatingset_with_preds[validatingset_with_preds$predictions == "YES", ]
yes <- yes[order(yes$prob.YES, decreasing = TRUE),]
yes <- yes[1:100,]
accuracy.df <- data.frame(actual = yes$instant_bookable, predicted = yes$predictions, yes$prob.YES)
actual_Y_count<-table(accuracy.df$actual)["YES"]
actual_Y_count

#Fictional record
df <- tibble(
  host_is_superhost = TRUE,
  host_identity_verified = TRUE,
  accommodates = factor("2", levels= c("1","2","3 or more")),
  price = factor("costly", levels = c("cheap", "appropriate", "costly")),
  has_availability = TRUE,
  number_of_reviews = factor("good number", levels = c("Very few", "few", "good number")),
  number_of_reviews_l30d = factor("1 or more", levels = c("0", "1 or more")),
  review_scores_rating = factor("v good", levels = c("bad", "good", "v good")),
  review_scores_value = factor("good", levels = c("bad", "good")),
  reviews_per_month = factor("more", levels = c("less", "more"))
)

#Predicting the outcome for fictional record
pred<-predict(instantbookable.nb, newdata = df)
pred

#III Classification Tree
#Binning
monserrat_filtered$review_scores_rating <- cut(monserrat_filtered$review_scores_rating, breaks = 5)

#Create a new data frame with predictor variables
tree_data <- monserrat_filtered[, c("review_scores_rating", "host_response_rate", "host_identity_verified", "room_type")]

library(rpart)
library(rpart.plot)
#Split the data into a training set and a testing set
set.seed(150)
train_index <- sample(nrow(tree_data), 0.6 * nrow(tree_data))
train <- tree_data[train_index, ]
test <- tree_data[-train_index, ]

#Classification tree model with train set
tree_model <- rpart(review_scores_rating ~ host_response_rate + host_identity_verified + room_type, data = train)
rpart.plot(tree_model, box.palette = "turquoise")

#Classification tree model with tree_data set
tree_model1 <- rpart(review_scores_rating ~ host_response_rate + host_identity_verified + room_type, data = tree_data, method = "class", cp = 0, minsplit = 2)
rpart.plot(tree_model1, fallen.leaves = TRUE, main = "Decision Tree", cex=.65)

#Finding the CP values of the model
cp_val <- rpart(review_scores_rating ~ host_response_rate + host_identity_verified + room_type, data = train, method = "class", cp = 0.00001, minsplit = 2, xval = 5)
printcp(cp_val)

#Making a Classification tree with the chosen CP Value
tree_model2 <- rpart(review_scores_rating ~ host_response_rate + host_identity_verified + room_type, data = tree_data, method = "class", cp = 0.011278)
rpart.plot(tree_model2, fallen.leaves = FALSE, main = "Decision Tree", cex=.65, type = 1, extra = 3, box.palette = "Blues")

library(caret)
#Using the tree to predict on train & test set to evaluate performance of Model 1
conf_mat <- predict(tree_model1, train, type = "class")
confusionMatrix(conf_mat, train$review_scores_rating)
conf_mat1 <- predict(tree_model1, test, type = "class")
confusionMatrix(conf_mat1, test$review_scores_rating)

#Using the tree to predict on train & test set to evaluate performance of Model 2
conf_mat2<-predict(tree_model2,train,type="class")
confusionMatrix(conf_mat2,train$review_scores_rating)
conf_mat3<-predict(tree_model2,test,type="class")
confusionMatrix(conf_mat3,test$review_scores_rating)

#Step 4 - Clustering
#Scaling the numeric variables
cols_scale<-which(sapply(Monserrat3,is.numeric))
Monserrat4<-scale(Monserrat3[,cols_scale])%>%as.data.frame()
view(Monserrat4)

#Performing K-Means Clustering
set.seed(150)
k.max<-15
.max <- 15
wss <- sapply(1:k.max, function(k) {kmeans(Monserrat4, k, nstart = 50, iter.max = 15)$tot.withinss})

plot(1:k.max, wss, type="b",pch=20,frame=FALSE, xlab = "Number of Clusters K", ylab = "Total Within-Cluster Sum of Squares")

km<-kmeans(Monserrat4,5,nstart=25)
print(km)

Monserrat5<- cbind(Monserrat3, cluster = km$cluster)%>%group_by(cluster)
view(Monserrat5)

#Creating a Scatter plot 
ggplot(Monserrat5, aes(x = `cluster`, y = `reviews_per_month`, color = as.factor(cluster))) +
  geom_point() +
  labs(title = "Monthly Reviews of each Cluster", x = "cluster", y = "reviews_per_month")

#Creating a Bar Plot
ggplot(Monserrat5, aes(x=cluster, fill=room_type)) +
  geom_bar(position="stack") +
  xlab("Cluster") +
  ylab("Count") +
  ggtitle("Number of Listings by Room Type and Cluster") +
  scale_fill_discrete(name = "Room Type")

#Creating a Violin Plot
ggplot(Monserrat5, aes(x = factor(cluster), y = review_scores_cleanliness, fill = factor(cluster))) +
  geom_violin(scale = "width", trim = TRUE) +
  geom_boxplot(width = 0.2, fill = "white", outlier.shape = NA) +
  labs(x = "Cluster", y = "Review Scores Cleanliness") +
  ggtitle("Review Scores Cleanliness by Cluster") +
  scale_fill_discrete(name = "Cluster")
