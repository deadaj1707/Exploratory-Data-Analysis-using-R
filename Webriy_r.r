# Load relevant libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("Intern.csv")

# Convert the 'dteday' variable to Date type
data$dteday <- as.Date(data$dteday)

# Printing first 5 rows of data
head(data)

# Basic summary statistics
summary(data)



# Visualize seasonal patterns
ggplot(data, aes(x = mnth, y = cnt, fill = as.factor(season))) +
  geom_boxplot() +
  labs(title = "Seasonal Distribution of Bike Rentals",
       x = "Month",
       y = "Count",
       fill = "Season") +
  theme_minimal()




# Visualize temperature distribution
ggplot(data, aes(x = temp)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Temperature",
       x = "Temperature (Celsius)",
       y = "Frequency") +
  theme_minimal()



# Visualize weather situation distribution
ggplot(data, aes(x = weathersit, fill = as.factor(season))) +
  geom_bar(position = "dodge") +
  labs(title = "Weather Situation Distribution by Season",
       x = "Weather Situation",
       y = "Count",
       fill = "Season") +
  theme_minimal()



# Visualize bike rentals over time
ggplot(data, aes(x = dteday, y = cnt, group = 1)) +
  geom_line(color = "steelblue") +
  labs(title = "Bike Rentals Over Time",
       x = "Date",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatter plot for temperature vs. count
ggplot(data, aes(x = temp, y = cnt)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(title = "Scatter Plot: Temperature vs. Bike Rentals",
       x = "Temperature (Celsius)",
       y = "Count") +
  theme_minimal()



# Scatter plot for humidity vs. count
ggplot(data, aes(x = hum, y = cnt)) +
  geom_point(alpha = 0.6, color = "coral") +
  labs(title = "Scatter Plot: Humidity vs. Bike Rentals",
       x = "Humidity",
       y = "Count") +
  theme_minimal()

# Calculate the correlation matrix for all numeric variables
correlation_matrix <- cor(select_if(data, is.numeric))

# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "number", tl.col = "black")

# Assuming data is your dataset
correlation_values <- cor(data[c("cnt", "season", "yr", "mnth", "holiday", "weekday",
                                      "workingday", "weathersit", "temp", "atemp", "hum", "windspeed")])

# Display correlation values
print("Correlation values:")
print(correlation_values)

install.packages("corrplot")
library(corrplot)

# Perform missing value analysis
missing_values <- data.frame(
  Variable = names(data),
  Missing_Values = sapply(data, function(x) sum(is.na(x)))
)

# Display missing values summary
print("Missing Values Summary:")
print(missing_values)

# Plot monthly distribution of the total number of bikes rented
monthly_plot <- aggregate(data$cnt, by = list(month = data$mnth), FUN = sum)

ggplot(monthly_plot, aes(x = factor(month), y = x)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Monthly Distribution of Total Bikes Rented",
       x = "Month",
       y = "Total Bikes Rented")

# Plot yearly distribution of the total number of bikes rented
yearly_plot <- aggregate(data$cnt, by = list(year = data$yr), FUN = sum)

ggplot(yearly_plot, aes(x = factor(year), y = x)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Yearly Distribution of Total Bikes Rented",
       x = "Year",
       y = "Total Bikes Rented")

# Plot boxplot for outliers' analysis
outliers_plot <- data.frame(cnt = data$cnt)

ggplot(outliers_plot, aes(y = cnt)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Boxplot for Outliers' Analysis",
       y = "Total Bikes Rented")

# Identify outliers using the InterQuartileRange method
Q1 <- quantile(data$cnt, 0.25)
Q3 <- quantile(data$cnt, 0.75)
IQR_value <- Q3 - Q1

# Define upper and lower bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify and remove outliers
outliers <- data[data$cnt < lower_bound | data$cnt > upper_bound, ]
cleaned_data <- data[data$cnt >= lower_bound & data$cnt <= upper_bound, ]

# Print the summary of removed outliers
cat("Number of outliers removed:", nrow(outliers), "\n")

# Additional information about the bounds
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

install.packages("randomForest")

# Create a new dataset 'data_new' by copying 'data'
data_new <- data

library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Generate random indices for train and test split
train_indices <- sample(1:nrow(data_new), 0.8 * nrow(data_new))
train_data <- data_new[train_indices, ]
test_data <- data_new[-train_indices, ]

# Create a random forest model
rf_model <- randomForest(cnt ~ season + yr + mnth  + weekday + holiday+
                         weathersit + temp + atemp+ hum + windspeed,
                         data = train_data)

# Predict the performance of the model on the test dataset
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance (optional)
# For regression problems, you can use metrics like RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - test_data$cnt)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Create a new dataset 'data_new' by copying 'data'
data_new <- data

library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Generate random indices for train and test split
train_indices <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_data <- data_new[train_indices, ]
test_data <- data_new[-train_indices, ]

# Create a random forest model
rf_model <- randomForest(cnt ~ season + yr + mnth  + weekday + holiday+
                         weathersit + temp + atemp+ hum + windspeed,
                         data = train_data)

# Predict the performance of the model on the test dataset
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance (optional)
# For regression problems, you can use metrics like RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - test_data$cnt)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Create a new dataset 'data_new' by copying 'data'
data_new <- data

library(randomForest)

# Set a seed for reproducibility
set.seed(123)

# Generate random indices for train and test split
train_indices <- sample(1:nrow(data_new), 0.7 * nrow(data_new))
train_data <- data_new[train_indices, ]
test_data <- data_new[-train_indices, ]

# Create a random forest model
rf_model <- randomForest(cnt ~ season + yr + mnth  + weekday +
                         weathersit + temp + atemp+ hum + windspeed,
                         data = train_data)

# Predict the performance of the model on the test dataset
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance (optional)
# For regression problems, you can use metrics like RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - test_data$cnt)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Predict the performance of the model on the test dataset
predictions_test <- predict(rf_model, newdata = test_data)

# Evaluate the model performance on test data
rmse_test <- sqrt(mean((predictions_test - test_data$cnt)^2))
print(paste("Root Mean Squared Error (RMSE) on Test Data:", rmse_test))

# Predict the performance of the model on the training dataset
predictions_train <- predict(rf_model, newdata = train_data)

# Evaluate the model performance on training data
rmse_train <- sqrt(mean((predictions_train - train_data$cnt)^2))
print(paste("Root Mean Squared Error (RMSE) on Training Data:", rmse_train))

# Plot for training data
ggplot(data = data.frame(Actual = train_data$cnt, Predicted = predictions_train)) +
  geom_point(aes(x = Actual, y = Predicted),
             color = "blue", alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values on Training Data",
       x = "Actual Count", y = "Predicted Count") +
  theme_minimal()

# Plot for testing data
ggplot(data = data.frame(Actual = test_data$cnt, Predicted = predictions_test)) +
  geom_point(aes(x = Actual, y = Predicted),
             color = "green", alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values on Test Data",
       x = "Actual Count", y = "Predicted Count") +
  theme_minimal()

# Predict the performance of the model on the test dataset
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model performance
rmse <- sqrt(mean((predictions - test_data$cnt)^2))
mae <- mean(abs(predictions - test_data$cnt))
msle <- mean((log1p(test_data$cnt) - log1p(predictions))^2)
r_squared <- 1 - sum((test_data$cnt - predictions)^2) / sum((test_data$cnt - mean(test_data$cnt))^2)

# Print the metrics
print(paste("Root Mean Squared Error (RMSE):", rmse))
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Squared Logarithmic Error (MSLE):", msle))
print(paste("R-Squared (R2) Score:", r_squared))

library(dplyr)

# Grid search for mtry
mtry_values <- c(2, 4, 6, 8, 10)  # Add more values as needed
rmse_results <- numeric(length(mtry_values))

# Perform grid search
for (i in seq_along(mtry_values)) {
  # Create a random forest model with current mtry value
  rf_model <- randomForest(cnt ~ season + yr + mnth + weekday +
                            weathersit + temp + atemp + hum + windspeed,
                            data = train_data, mtry = mtry_values[i])

  # Predict the performance of the model on the test dataset
  predictions <- predict(rf_model, newdata = test_data)

  # Calculate RMSE
  rmse_results[i] <- sqrt(mean((predictions - test_data$cnt)^2))
}

# Identify the optimal mtry value with the lowest RMSE
optimal_mtry <- mtry_values[which.min(rmse_results)]
print(paste("Optimal mtry value:", optimal_mtry))

# Train the final model with the optimal mtry value
final_rf_model <- randomForest(cnt ~ season + yr + mnth + weekday +
                               weathersit + temp + atemp + hum + windspeed,
                               data = train_data, mtry = optimal_mtry)

# Predict the performance of the final model on the test dataset
final_predictions <- predict(final_rf_model, newdata = test_data)

# Evaluate the final model performance on test data
final_rmse <- sqrt(mean((final_predictions - test_data$cnt)^2))
print(paste("Final Root Mean Squared Error (RMSE) on Test Data:", final_rmse))


# Plot for testing data
ggplot(data = data.frame(Actual = test_data$cnt, Predicted = final_predictions)) +
  geom_point(aes(x = Actual, y = Predicted),
             color = "green", alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs. Predicted Values on Test Data",
       x = "Actual Count", y = "Predicted Count") +
  theme_minimal()

# interaction term
train_data$interaction_temp_hum <- train_data$temp * train_data$atemp * train_data$yr
test_data$interaction_temp_hum <- test_data$temp * test_data$atemp * test_data$yr

# Update the model with the new interaction term
rf_model_interaction <- randomForest(cnt ~ season + yr + mnth + weekday +
                                     weathersit + temp + atemp + hum + windspeed +
                                     interaction_temp_hum,
                                     data = train_data)

# Evaluate the updated model
predictions_test_interaction <- predict(rf_model_interaction, newdata = test_data)
rmse_test_interaction <- sqrt(mean((predictions_test_interaction - test_data$cnt)^2))
print(paste("RMSE on Test Data with Interaction Term:", rmse_test_interaction))

# polynomial feature
train_data$temp_squared <- train_data$temp^2* train_data$atemp^2
test_data$temp_squared <- test_data$temp^2* test_data$atemp^2

# Update the model with the new polynomial feature
rf_model_poly <- randomForest(cnt ~ season + yr + mnth + weekday +
                              weathersit + temp + atemp + hum + windspeed +
                              temp_squared,
                              data = train_data)

# Evaluate the updated model
predictions_test_poly <- predict(rf_model_poly, newdata = test_data)
rmse_test_poly <- sqrt(mean((predictions_test_poly - test_data$cnt)^2))
print(paste("RMSE on Test Data with Polynomial Feature:", rmse_test_poly))
