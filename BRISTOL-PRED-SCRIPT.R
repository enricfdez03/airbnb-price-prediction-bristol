library(readr)
library(tidyverse)
library(dplyr)
library(GGally)
library(ggplot2)
library(DescTools)
library(jsonlite)
library(fastDummies)
library(glmnet)
library(caret)
library(tidyverse)

# Load dataset
listings_bristol <- read_csv("C:/Users/Usuario/Downloads/listings (4).csv/listings.bristol.csv")

# Remove duplicate rows
raw_df <- unique(listings_bristol)

# Select relevant variables
df_shortened <- raw_df %>%
  select(
    # Property characteristics
    property_type, room_type, accommodates, bathrooms_text, bedrooms, beds, amenities, latitude, longitude,
    
    # Host information
    host_is_superhost, host_listings_count, host_total_listings_count, host_response_rate, host_acceptance_rate, host_since, host_response_time,
    
    # Availability & minimum stay
    minimum_nights, availability_30, availability_60, availability_90, availability_365,
    
    # Reviews & ratings
    number_of_reviews, review_scores_rating, review_scores_cleanliness, review_scores_location, review_scores_value,
    
    # Booking & pricing factors
    instant_bookable, calculated_host_listings_count,
    
    # Target variable
    price
  )

# Convert categorical variables to factors
df_shortened <- df_shortened %>%
  mutate(across(c(property_type, room_type, host_is_superhost, instant_bookable), as.factor))

# Convert percentage values to numeric
df_shortened<- df_shortened %>%
  mutate(
    host_response_rate = as.numeric(gsub("%", "", host_response_rate)) / 100,
    host_acceptance_rate = as.numeric(gsub("%", "", host_acceptance_rate)) / 100
  )

# Define numeric variables
numeric_cols <- c("accommodates", "bedrooms", "beds", "latitude", "longitude",
                  "host_listings_count", "host_total_listings_count", "host_response_rate", 
                  "host_acceptance_rate", "minimum_nights", "availability_30", "availability_60",
                  "availability_90", "availability_365", "number_of_reviews", "review_scores_rating",
                  "review_scores_cleanliness", "review_scores_location", "review_scores_value",
                  "calculated_host_listings_count")

# Convert selected columns to numeric
df_shortened <- df_shortened %>%
  mutate(across(all_of(numeric_cols), ~ as.numeric(.)))


# Convert price by removing "$" and commas
df_shortened <- df_shortened %>%
  mutate(price = as.numeric(gsub("[$,]", "", price)))


# Convert categorical variables to factors
df_shortened <- df_shortened %>%
  mutate(across(c(property_type, room_type, host_is_superhost, instant_bookable), as.factor))

# Replace "N/A" with NA in host response rate and acceptance rate
na_cols <- c("host_response_rate", "host_acceptance_rate", "host_response_time")
df_shortened[na_cols] <- lapply(df_shortened[na_cols], function(x) ifelse(x == "N/A", NA, x))

# Count missing values for each column
na_counts <- colSums(is.na(df_shortened))
na_counts <- na_counts[na_counts > 0]  # Filter only columns with missing values
print(na_counts)  # Display columns with missing values

# Separate data into those with and without a price
df_price <- df_shortened %>% filter(!is.na(price))
df_noprice <- df_shortened %>% filter(is.na(price))

library(DescTools)

# Fill missing values with mode for categorical variables
df_price$bathrooms_text[is.na(df_price$bathrooms_text)] <- "1 bath"
df_price$host_is_superhost[is.na(df_price$host_is_superhost)] <- "FALSE"
# Fill missing values with mode for some numerical variables
df_price$bedrooms[is.na(df_price$bedrooms)] <- 1
df_price$beds[is.na(df_price$beds)] <- 1


Mode(df_price$bathrooms_text)
Mode(df_price$host_is_superhost)
Mode(df_price$bedrooms)
Mode(df_price$beds)

# Plot histograms to assess distributions before imputing missing values
hist(df_price$host_acceptance_rate, main = "Host Acceptance Rate Distribution", xlab = "Host Acceptance Rate")
hist(df_price$host_response_rate, main = "Host Response Rate Distribution", xlab = "Host Response Rate")
hist(df_price$review_scores_rating, main = "Review Scores Rating Distribution", xlab = "Review Scores Rating") #Quite epresentative of the others due to correlation 

# Compute correlation between host response rate and acceptance rate
cor(df_price$host_response_rate, df_price$host_acceptance_rate, use = "complete.obs")

# Function to fill missing values with the median
fill_with_median <- function(column) {
  column[is.na(column)] <- median(column, na.rm = TRUE)
  return(column)
}

# Apply median imputation to numeric columns with missing values
df_price <- df_price %>%
  mutate(
    host_response_rate = fill_with_median(host_response_rate),
    host_acceptance_rate = fill_with_median(host_acceptance_rate),
    review_scores_rating = fill_with_median(review_scores_rating),
    review_scores_cleanliness = fill_with_median(review_scores_cleanliness),
    review_scores_location = fill_with_median(review_scores_location),
    review_scores_value = fill_with_median(review_scores_value)
  )

#Create Unknown column for host response time (significant number of missing values)
df_price$host_response_time[is.na(df_price$host_response_time)] <- "Unknown"
df_price$host_response_time <- as.factor(df_price$host_response_time)

#Check for possible NAs left
na_countsp <- colSums(is.na(df_price))
na_countsp <- na_countsp[na_countsp > 0]  # Filter only columns with missing values
print(na_countsp)

# Create price bins (intervals)
df_price <- df_price %>%
  mutate(price_category = cut(price, breaks = seq(0, max(price, na.rm = TRUE), by = 50), include.lowest = TRUE))
# Count the number of listings per price category
price_counts <- df_price %>%
  group_by(price_category = cut(price, breaks = seq(0, max(price, na.rm = TRUE) + 50, by = 50), include.lowest = TRUE)) %>%
  summarise(count = n())

# Plot price distribution using ggplot2
ggplot(price_counts, aes(x = price_category, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate labels for better readability
  labs(title = "Price Distribution of Airbnb Listings in Bristol",
       x = "Price Range ($)",
       y = "Number of Listings")

# PRICE OUTLIERS
# Calculate the 95th percentile price threshold
price_threshold <- quantile(df_price$price, 0.95, na.rm = TRUE)

# Filter out listings with prices above the threshold
df_price <- df_price %>% filter(price <= price_threshold)

# Recalculate the price counts based on the filtered data
price_counts <- df_price %>%
  group_by(price_category = cut(price, breaks = seq(0, max(price, na.rm = TRUE), by = 50), include.lowest = TRUE)) %>%
  summarise(count = n())

# Plot price distribution using ggplot2
ggplot(price_counts, aes(x = price_category, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotate labels for better readability
  labs(title = "Price Distribution of Airbnb Listings in Bristol (Outliers Removed)",
       x = "Price Range ($)",
       y = "Number of Listings")


# Look for numeric columns
df_price %>%
  select(where(is.numeric)) %>%
  select(where(~n_distinct(.) > 2)) %>%  # Exclude dummies
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Continuous Variables")


# Function to adjust the highest and lowest values to defined percentiles
adjust_outliers_percentiles <- function(df_price, columns, upper_percentile = 0.99, lower_percentile = 0.01) {
  for (col in columns) {
    # Calculate the upper (99%) and lower (1%) percentiles
    upper_limit <- quantile(df_price[[col]], upper_percentile, na.rm = TRUE)
    lower_limit <- quantile(df_price[[col]], lower_percentile, na.rm = TRUE)
    
    # Replace values above the upper percentile with the 99th percentile value
    df_price[[col]] <- ifelse(df_price[[col]] > upper_limit, upper_limit,
                              ifelse(df_price[[col]] < lower_limit, lower_limit, df_price[[col]]))
  }
  return(df_price)
}

# Select numeric columns, excluding 'price' and binary variables (only 2 unique values)
numeric_columns <- df_price %>%
  select(where(is.numeric)) %>%
  select(-price) %>%  # Exclude 'price'
  select(where(~n_distinct(.) > 2)) %>%  # Exclude columns with only 2 unique values (binary)
  colnames()

# Apply the function to adjust outliers to the percentiles
df_price <- adjust_outliers_percentiles(df_price, numeric_columns)

# Visualize the results with boxplots
df_price %>%
  select(where(is.numeric) & !starts_with("price")) %>%
  select(where(~n_distinct(.) > 2)) %>%  # Exclude binary numeric variables
  gather(key = "Variable", value = "Value") %>%
  ggplot(aes(x = Variable, y = Value)) + 
  geom_boxplot(outlier.shape = NA) +  # Hide the outliers
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplot of Numeric Variables (After Removing Outliers)")


# CONVERTING BINARY VARIABLES TO NUMERICAL FORM
df_price$host_is_superhost <- ifelse(df_price$host_is_superhost == "TRUE", 1, 0)
df_price$instant_bookable <- ifelse(df_price$instant_bookable == "TRUE", 1, 0)

reference_date <- as.Date("2025-03-02")
# Compute difference between dates to get host experience
df_price$host_experience <- as.numeric(difftime(reference_date, df_price$host_since, units = "days")) / 365.25
# See new variable
summary(df_price$host_experience)

# Create a binary variable indicating if a listing has reviews
df_price <- df_price %>%
  mutate(has_reviews = ifelse(number_of_reviews > 0, 1, 0))

# Create two variables from the bathroom column
df_price <- df_price %>%
  mutate(
    bathrooms = case_when(
      str_detect(str_to_lower(bathrooms_text), "half") ~ 0.5,  # Assign 0.5 for half-baths
      TRUE ~ as.numeric(str_extract(bathrooms_text, "\\d*\\.?\\d+"))  # Extract numeric values
    ),
    shared_bathroom = ifelse(str_detect(str_to_lower(bathrooms_text), "shared"), 1, 0)  # Detect shared
  )

#ORDINAL ENCODERS?
catg_vars <- c("property_type", "room_type", "host_response_time")
# Print the unique categories/levels for each variable
for (var in catg_vars) {
  cat("\nUnique values for", var, ":\n")
  if (is.factor(df_price[[var]])) {
    print(levels(df_price[[var]]))  # For factor variables, show the levels
  } else if (is.character(df_price[[var]])) {
    print(unique(df_price[[var]]))  # For character variables, show unique values
  } else {
    print(sort(unique(df_price[[var]])))  # For numeric variables, show sorted unique values
  }
}

#Ordinality for host response time
df_price$host_response_time <- factor(df_price$host_response_time, 
                                      levels = c("a few days or more", 
                                                 "within a day", 
                                                 "within a few hours", 
                                                 "within an hour", 
                                                 "Unknown"), 
                                      ordered = TRUE)


# ONE HOT ENCODING
categorical_columns <- c("property_type", "room_type")
#Calculate the frequency of each property type
property_type_freq <- table(df_price$property_type)
# Filter property types that are > 2.5% of total observations
property_types_over_2.5_percent <- names(property_type_freq[property_type_freq / nrow(df_price) > 0.025])
# Create binary columns only for property types over 2.5% of observations
for (property_type in property_types_over_2.5_percent) {
  df_price[[make.names(property_type)]] <- ifelse(df_price$property_type == property_type,1,0)
}

# Binary cols for room type
df_price <- dummy_cols(df_price, select_columns = "room_type", remove_first_dummy = FALSE)

# AMENITIES
# Paso 1: Convertir amenities JSON a listas
df_price$amenities <- lapply(df_price$amenities, fromJSON)
# Paso 2: Crear una tabla de frecuencia de todas las amenities
all_amenities <- unlist(df_price$amenities)
amenity_freq <- table(all_amenities)
# Paso 3: Seleccionar amenities que aparecen en más del 10% de los anuncios
cutoff <- 0.10
popular_amenities <- names(amenity_freq[amenity_freq / nrow(df_price) > cutoff])
# Paso 4: Crear variables binarias para las amenities seleccionadas
for (amenity in popular_amenities) {
  df_price[[make.names(amenity)]] <- sapply(df_price$amenities, function(x) ifelse(amenity %in% x, 1, 0))
}
# Paso 5: Mantener solo las columnas de las amenities seleccionadas (sin calcular correlaciones)
df_price <- df_price %>% select(all_of(make.names(popular_amenities)), everything())
# Ver las amenities finales seleccionadas
print(popular_amenities)

# CLUSTERING: ELBOW METHOD
set.seed(123)  # To ensure reproducibility of the results
wss <- numeric(10)  # Create an empty vector to store the WSS values
# Perform K-means for different numbers of clusters (from 1 to 10)
for (k in 1:10) {
  kmeans_result <- kmeans(df_price[, c("latitude", "longitude")], centers = k)
  wss[k] <- kmeans_result$tot.withinss  # Store the WSS for each k
}
# Plot the Elbow Method
plot(1:10, wss, type = "b", pch = 19, xlab = "Number of Clusters", ylab = "Within-cluster Sum of Squares",
     main = "Elbow Method: Optimal K Selection")

# Perform K-means clustering on latitude and longitude
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(df_price[, c("latitude", "longitude")], centers = 5)  # Use 5 clusters, adjust as necessary
df_price$geo_cluster <- kmeans_result$cluster

# Plot the clusters on a scatter map of latitude and longitude
ggplot(df_price, aes(x = latitude, y = longitude, color = as.factor(geo_cluster))) +
  geom_point(alpha = 0.6) + 
  labs(title = "Geographic Cluster Distribution",
       x = "Latitude", 
       y = "Longitude", 
       color = "Cluster") +
  theme_minimal() +
  scale_color_discrete(name = "Cluster")

# Create binary columns for each cluster
df_price$cluster_1 <- ifelse(df_price$geo_cluster == 1, 1, 0)
df_price$cluster_2 <- ifelse(df_price$geo_cluster == 2, 1, 0)
df_price$cluster_3 <- ifelse(df_price$geo_cluster == 3, 1, 0)
df_price$cluster_4 <- ifelse(df_price$geo_cluster == 4, 1, 0)
df_price$cluster_5 <- ifelse(df_price$geo_cluster == 5, 1, 0)


# Use dplyr to erase columns
df_price_final <- df_price %>%
  select(-c(property_type, room_type, bathrooms_text, amenities, host_since, price_category, geo_cluster))

# Obtain type of column
column_types <- sapply(df_price_final, function(x) class(x)[1])
# Count how many columns of each type
table(column_types)

# Use dplyr to erase columns
df_price_final <- df_price %>%
  select(-c(property_type, room_type, bathrooms_text, amenities, host_since, price_category, geo_cluster))

# Obtain type of column
column_types <- sapply(df_price_final, function(x) class(x)[1])
# Count how many columns of each type
table(column_types)

#NORMALISATION/STANDARIZTION
# Identify numeric columns (nonbinary)
non_binary_cols <- sapply(df_price_final, function(x) is.numeric(x) && max(x, na.rm = TRUE) != 1)
non_binary_cols["price"] <- FALSE
# See selected columns
names(df_price_final)[non_binary_cols]

#STANDARISATION (ZSCORE FOR LINEAR REGRESSION AND TREES

# Apply standarisation
df_price_stdt <- df_price_final %>%
  mutate(across(
    .cols = which(non_binary_cols),   # Select non binary columns
    .fns = ~ scale(.)
  )) %>%
  mutate(across(
    .cols = which(non_binary_cols), 
    .fns = as.numeric                 # Transform from matrix to numerical  
  ))

# Remove One Category to Avoid Dummy Trap
df_LASSO <- df_price_stdt %>%
  select(-c(`room_type_Private room`, cluster_1 , longitude, latitude)) #Coordinates already captures in clusters


#INTERACTIONS

df_LASSO$Rating_Interaction <- df_LASSO$has_reviews*df_LASSO$review_scores_rating
df_LASSO$Cleanliness_Interaction <- df_LASSO$has_reviews*df_LASSO$review_scores_cleanliness
df_LASSO$Location_Interaction <- df_LASSO$has_reviews*df_LASSO$review_scores_location
df_LASSO$Value_Interaction <- df_LASSO$has_reviews*df_LASSO$review_scores_value

# Only mantain Interaction Columns
df_LASSO <- df_LASSO %>%
  select(-has_reviews, 
         -review_scores_rating, 
         -review_scores_cleanliness, 
         -review_scores_location, 
         -review_scores_value)

df_LASSO$host_response_time <- as.numeric(as.factor(df_LASSO$host_response_time))  

# 1. Split the data into training and testing sets
set.seed(321)  # For reproducibility
trainIndex <- createDataPartition(df_LASSO$price, p = 0.8, list = FALSE)  # Use 80% for training
train_data <- df_LASSO[trainIndex, ]
test_data <- df_LASSO[-trainIndex, ]

# 2. Fit the Lasso model with cross-validation (cv.glmnet optimizes lambda)
# In glmnet, "alpha = 1" specifies that we are using Lasso (L1 regularization)
lasso_model <- cv.glmnet(
  x = as.matrix(train_data %>% select(-price)),  # Exclude the dependent variable 'price'
  y = train_data$price,
  alpha = 1,  # Lasso regularization (L1)
  standardize = TRUE,  # Standardizes the variables (important for Lasso)
  nfolds = 10,  # Number of folds in cross-validation
  lambda.min.ratio = 0.0001  # Minimum lambda ratio
)

# 3. Display the optimal lambda value
# Plot the cross-validation results for lambda
plot(lasso_model)
cat("Optimal lambda value:", lasso_model$lambda.min, "\n")

# 4. Train the model with the best lambda
best_lambda <- lasso_model$lambda.min
lasso_final_model <- glmnet(
  x = as.matrix(train_data %>% select(-price)),
  y = train_data$price,
  alpha = 1,
  lambda = best_lambda,
  standardize = TRUE
)

# 5. PREDICTIONS

# Make predictions on the test set
test_predictions <- predict(lasso_final_model, s = best_lambda, newx = as.matrix(test_data %>% select(-price)))
test_predictions <- as.vector(test_predictions)

# Make predictions on the training set
train_predictions <- predict(lasso_final_model, s = best_lambda, newx = as.matrix(train_data %>% select(-price)))
train_predictions <- as.vector(train_predictions)

# 6. EVALUATE MODEL PERFORMANCE

# Function to calculate performance metrics
calculate_metrics <- function(actual, predicted) {
  mae <- mean(abs(predicted - actual))  # Mean Absolute Error
  mse <- mean((predicted - actual)^2)   # Mean Squared Error
  rmse <- sqrt(mse)                     # Root Mean Squared Error
  
  # R-squared Calculation
  ss_total <- sum((actual - mean(actual))^2)  # Total sum of squares
  ss_residual <- sum((predicted - actual)^2)  # Residual sum of squares
  r_squared <- 1 - (ss_residual / ss_total)
  
  # Return a named list
  return(list(MAE = mae, MSE = mse, RMSE = rmse, R_squared = r_squared))
}

# Calculate metrics for the test set
test_metrics <- calculate_metrics(test_data$price, test_predictions)

# Calculate metrics for the training set
train_metrics <- calculate_metrics(train_data$price, train_predictions)

# Create a comparison table for Train and Test metrics
metrics_table <- data.frame(
  Data = c("TRAINING", "TEST"),
  MAE = c(train_metrics$MAE, test_metrics$MAE),
  MSE = c(train_metrics$MSE, test_metrics$MSE),
  RMSE = c(train_metrics$RMSE, test_metrics$RMSE),
  R_squared = c(train_metrics$R_squared, test_metrics$R_squared)
)

# Print the metrics table
print(metrics_table)

print("Price Summary")
summary(df_LASSO$price) #For comparison

# Extraer y convertir coeficientes a dataframe
coeff_df <- as.data.frame(as.matrix(coef(lasso_final_model, s = best_lambda)))

# Agregar nombres de las variables
coeff_df$Variable <- rownames(coeff_df)

# Renombrar la columna de coeficientes
colnames(coeff_df)[1] <- "Coefficient"

# Filtrar solo coeficientes distintos de 0 y ordenar por importancia
coeff_df <- coeff_df %>%
  filter(Coefficient != 0) %>%
  arrange(desc(abs(Coefficient)))

# Imprimir en formato de tabla legible
print(coeff_df, row.names = FALSE)

# Plot residualsDistribution
residuals <- test_predictions - test_data$price
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = 'skyblue', color = 'black') +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency")

#Plot Predicted vs Actual Values
# Convert predictions to a vector
test_predictions <- as.vector(test_predictions)
# Create a dataframe with actual vs predicted values
LASSO_testresults_df <- data.frame(
  Actual = test_data$price,
  Predicted = test_predictions
)
# Plot Predicted vs Actual values
ggplot(LASSO_testresults_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +   # Blue points
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +  # Red reference line (ideal)
  labs(title = "Predicted vs Actual Values (Test Set)",
       x = "Actual Value",
       y = "Model Prediction") +
  theme_minimal()  # Minimalist style for the plot


y = train_data$price
y = log(train_data$price)
df_LASSO$host_response_time <- as.numeric(as.factor(df_LASSO$host_response_time))  

# 1. Split the data into training and testing sets
set.seed(321)  # For reproducibility
trainIndex <- createDataPartition(df_LASSO$price, p = 0.8, list = FALSE)  # Use 80% for training
train_data <- df_LASSO[trainIndex, ]
test_data <- df_LASSO[-trainIndex, ]

# 2. Fit the Lasso model with cross-validation (cv.glmnet optimizes lambda)
lasso_model <- cv.glmnet(
  x = as.matrix(train_data %>% select(-price)),  # Exclude 'price'
  y = log(train_data$price),  # Use log(price)
  alpha = 1,  
  standardize = TRUE,  
  nfolds = 10,  
  lambda.min.ratio = 0.0001  
)

# 3. Predictions (exponentiate to return to original scale)
predictions <- predict(lasso_model, newx = as.matrix(test_data %>% select(-price)), s = "lambda.min")
predicted_prices <- exp(predictions)  # Convert back to original price scale

# 4. Compute error metrics
actual_prices <- test_data$price
rmse <- sqrt(mean((actual_prices - predicted_prices)^2))
cat("RMSE:", rmse, "\n")

# Standarized data only for decision tree methods 
df_DT <- df_price_stdt
# Load necessary libraries
library(xgboost)

# GB DATASET
df_DT$Rating_Interaction <- df_DT$has_reviews * df_DT$review_scores_rating
df_DT$Cleanliness_Interaction <- df_DT$has_reviews * df_DT$review_scores_cleanliness
df_DT$Location_Interaction <- df_DT$has_reviews * df_DT$review_scores_location
df_DT$Value_Interaction <- df_DT$has_reviews * df_DT$review_scores_value

# Keep only the necessary columns (the interactions)
df_DT <- df_DT %>%
  select(-has_reviews, 
         -review_scores_rating, 
         -review_scores_cleanliness, 
         -review_scores_location, 
         -review_scores_value)


# Cross-Validation Setup (4 folds)
train_control <- trainControl(
  method = "cv",        # Cross-validation
  number = 5,           # 5 folds for faster execution
  verboseIter = FALSE,   # To see the progress
  search = "grid"       # Grid search to find the best hyperparameters
)

# Hyperparameter grid for Grid Search with fewer combinations
tune_grid <- expand.grid(
  nrounds = c(25, 50),              # Reduced number of rounds (iterations)
  max_depth = c(3, 5),              # Tree depth (reduced range)
  eta = c(0.05, 0.1),               # Learning rate
  gamma = c(0, 0.1),                # Regularization (minimum loss value)
  colsample_bytree = c(0.8),        # Feature subsample (fixed value)
  min_child_weight = c(1, 5),       # Minimum child weight
  subsample = c(0.8)                # Row subsample (fixed value)
)

# Split the data into training (80%) and testing (20%)
set.seed(123)
trainIndex <- createDataPartition(df_DT$price, p = 0.8, list = FALSE)
df_train <- df_DT[trainIndex, ]
df_test <- df_DT[-trainIndex, ]

# Adjust the model using a smaller sample with cross-validation
set.seed(123)  # For reproducibility
df_train_sample <- df_train[sample(nrow(df_train), size = 1000), ]  # Use a smaller sample

# Fit the model using cross-validation
xgb_model <- train(
  price ~ .,               # Formula to predict 'price'
  data = df_train_sample,  # Use the reduced training sample
  method = "xgbTree",      # XGBoost model
  trControl = train_control, # Cross-validation
  tuneGrid = tune_grid,    # Hyperparameter grid
  verbose = FALSE           # Show progress
)

# Show the results of the hyperparameter search
print(xgb_model)
print(xgb_model$bestTune)  # Optimal hyperparameters

# Predict on test and train data with the final model
train_preds <- predict(xgb_model, newdata = df_train)
test_preds <- predict(xgb_model, newdata = df_test)



# Calculate metrics for Train and Test
calc_metrics <- function(actual, predicted) {
  mae <- mean(abs(predicted - actual))
  mse <- mean((predicted - actual)^2)
  rmse <- sqrt(mse)
  rsq <- cor(predicted, actual)^2  # R²
  return(c(MAE = mae, MSE = mse, RMSE = rmse, Rsquared = rsq))
}

train_metrics <- calc_metrics(df_train$price, train_preds)
test_metrics <- calc_metrics(df_test$price, test_preds)

# Create a table with metrics
metrics_table <- data.frame(
  Dataset = c("Train", "Test"),
  MAE = c(train_metrics["MAE"], test_metrics["MAE"]),
  MSE = c(train_metrics["MSE"], test_metrics["MSE"]),
  RMSE = c(train_metrics["RMSE"], test_metrics["RMSE"]),
  Rsquared = c(train_metrics["Rsquared"], test_metrics["Rsquared"])
)

print(metrics_table)  # Display metrics

# Predicted vs Actual plot (Test set)
ggplot(data = data.frame(Actual = df_test$price, Predicted = test_preds), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Ideal line
  theme_minimal() +
  labs(title = "Predicted vs Actual (Test Set)", x = "Actual Values", y = "Predicted Values")

# Plot variable importance (Top 10)
importance <- xgb.importance(model = xgb_model$finalModel)
top_10_importance <- importance[1:10, ]
xgb.plot.importance(importance_matrix = top_10_importance)

