rm(list=ls())

#Libraries installation
library(dplyr)
library(lubridate)
library("ggplot2")
library(tidyverse)
library(car) # vif
library(glmnet) # LASSO
library(moments) # jarque.test
library(caret) # validazione esterna
library(DataExplorer)

print("Libraries loaded")


#Dataset import

library(readr)
Agrimonia_Dataset <- read_csv("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/Agrimonia_Dataset_v_3_0_0.csv") #From .csv
Metadata_stations <- read_csv("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/Metadata_monitoring_network_registry_v_2_0_1.csv")

# load(file = "Agrimonia_Dataset_v_3_0_0.Rdata") #From R
# Agrimonia_Dataset <- AgrImOnIA_Dataset_v_3_0_0
# rm(list="AgrImOnIA_Dataset_v_3_0_0")

Stations_name <- Metadata_stations %>%
  select(IDStation, NameStation, Province) %>%
  distinct(IDStation, .keep_all = TRUE)

Agrimonia_Dataset <- Agrimonia_Dataset %>%
  left_join(Stations_name, 
            by = c("IDStations" = "IDStation")) %>%
  select(IDStations, NameStation, Province, everything())

Agrimonia_Dataset$Month <- lubridate::month(Agrimonia_Dataset$Time, label = TRUE)
Agrimonia_Dataset <- Agrimonia_Dataset %>% select(IDStations:Time, Month, everything())
mesi_italiani <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#mesi_italiani <- c("gen", "feb", "mar", "apr", "mag", "giu",
#"lug", "ago", "set", "ott", "nov", "dic")

Agrimonia_Dataset <- Agrimonia_Dataset %>%
  mutate(
    Month_num = match(Month, mesi_italiani),  
    Season = case_when(
      Month_num %in% c(12, 1, 2)  ~ "Winter",
      Month_num %in% c(3, 4, 5)   ~ "Spring",
      Month_num %in% c(6, 7, 8)   ~ "Summer",
      Month_num %in% c(9, 10, 11)   ~ "Autumn",
    ))

# Create a column indicating day of the week
Agrimonia_Dataset$Day_of_week <- weekdays(Agrimonia_Dataset$Time)

# Create a vector with station IDs selected
stations_id   <- c(504, 583, 697)
# stations_name <- c("Sesto San Giovanni -> MI", "Bergamo -> BG", "Borgofranco sul Po -> MN")

DB <- Agrimonia_Dataset %>% 
  filter(IDStations %in% stations_id)
MI_DB <- DB %>% 
  filter(IDStations==504)
BG_DB <- DB %>% 
  filter(IDStations==583)
MN_DB <- DB %>% 
  filter(IDStations==697)


#Preliminary data analysis

#Global analysis
plot(DB$Time,DB$AQ_nox)
plot(DB$Month,DB$AQ_nox)
ggplot(DB, aes(x = Season, y = AQ_nox)) +
  geom_boxplot()+ theme_minimal() +
  theme(legend.position = "bottom")
ggplot(DB, aes(x = Time, y = AQ_nox, color=Province)) +
  geom_point()+ theme_minimal() +
  theme(legend.position = "bottom")
ggplot(DB, aes(x = Season, y = AQ_nox, color=Province)) +
  geom_boxplot()+ theme_minimal() +
  theme(legend.position = "bottom")
ggplot(DB, aes(x = Time, y = AQ_nox, color=Province)) +
  geom_smooth()+ theme_minimal() +
  theme(legend.position = "bottom")
ggplot(DB, aes(x  = AQ_nox, color=Province)) +
  geom_density(size=1.5)+ theme_minimal() +
  theme(legend.position = "bottom")
# Create full HTML report
#create_report(DB,
#              y = "Province",
#              config = configure_report(add_plot_prcomp = FALSE),
#              output_file = "EDA_Prov_Report.html")

#From here new changes applied by nick99silver to be approved from group
ggplot(DB, aes(x = Time, y = AQ_nox)) +
  geom_point(aes(color = Season)) +  # colore solo per i punti
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # una sola linea per Province
  facet_wrap(~ Province) +
  labs(title = "AQ_nox over time by Province") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Create a scatter plot with a linear regression line
ggplot(DB, aes(x = Time, y = AQ_nox)) +
  geom_point(aes(color=Season)) + 
  geom_smooth(method = "lm", se = FALSE, color="black") +
  labs(title = "AQ_nox over time") +
  theme_minimal() +
  theme(legend.position = "bottom")



# Create a plot showing the average AQ_nox by day of the week
ggplot(DB, aes(x = Day_of_week, y = AQ_nox)) +
  geom_boxplot(outlier.alpha = 0.2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
  labs(title = "AQ_nox by Day of the Week (Mean Highlighted)",
       y = "NOₓ Concentration (µg/m³)") +
  theme_minimal() +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create a plot showing the average AQ_nox by day of the week In Milano
ggplot(MI_DB, aes(x = Day_of_week, y = AQ_nox)) +
  geom_boxplot(outlier.alpha = 0.2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 2) +
  labs(title = "AQ_nox by Day of the Week in Milano (Mean Highlighted)",
       y = "NOₓ Concentration (µg/m³)") +
  theme_minimal() +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

library(zoo)

# Interpola direttamente nella colonna AQ_nox
# Rivedere interpolazione chiedendo a Otto, possible uso ARIMA per stimare i valori mancanti
DB$AQ_nox <- na.approx(DB$AQ_nox)

sum(is.na(DB$AQ_nox))

# Define different split ratios to test
split_ratios <- seq(0.6, 0.9, by = 0.05)  # Testing splits from 60% to 90% training data

# Initialize storage for results
split_results <- data.frame(
  train_ratio = numeric(),
  rmse = numeric(),
  rmse_sd = numeric()  # Standard deviation of RMSE across folds
)

# Number of cross-validation folds
n_folds <- 5

# Function to evaluate split ratio
evaluate_split <- function(train_ratio, data) {
  # Initialize vector to store RMSE values for each fold
  fold_rmses <- numeric(n_folds)
  
  # Create folds
  set.seed(123)  # For reproducibility
  folds <- cut(seq(1, nrow(data)), breaks = n_folds, labels = FALSE)
  
  for(fold in 1:n_folds) {
    # Create train and test indices for this fold
    test_indices <- which(folds == fold)
    train_indices <- which(folds != fold)
    
    # Further split the training data according to the ratio
    n_train <- floor(length(train_indices) * train_ratio)
    train_indices <- sample(train_indices, n_train)
    
    # Split the data
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]
    
    # Fit model
    model <- lm(AQ_nox ~ Time + Month_num + Day_of_week + Province, data = train_data)
    
  
    # Make predictions
    predictions <- predict(model, newdata = test_data)
    
    # Calculate RMSE for this fold
    fold_rmses[fold] <- sqrt(mean((test_data$AQ_nox - predictions)^2, na.rm = TRUE))
  }
  
  # Return mean and standard deviation of RMSE
  return(list(
    mean_rmse = mean(fold_rmses),
    sd_rmse = sd(fold_rmses)
  ))
}

# Perform grid search
cat("Starting split ratio grid search...\n")
for(ratio in split_ratios) {
  cat(sprintf("Testing train ratio: %.2f\n", ratio))
  results <- evaluate_split(ratio, DB)
  
  split_results <- rbind(split_results, data.frame(
    train_ratio = ratio,
    rmse = results$mean_rmse,
    rmse_sd = results$sd_rmse
  ))
}

# Find best split ratio (minimum RMSE)
best_ratio_idx <- which.min(split_results$rmse)
best_ratio <- split_results$train_ratio[best_ratio_idx]

cat("\nResults of split ratio grid search:\n")
print(split_results)

cat("\nBest train ratio:", best_ratio, "\n")
cat("Best RMSE:", split_results$rmse[best_ratio_idx], "\n")
cat("RMSE standard deviation:", split_results$rmse_sd[best_ratio_idx], "\n")

# Plot results
ggplot(split_results, aes(x = train_ratio)) +
  geom_line(aes(y = rmse, color = "Mean RMSE")) +
  geom_ribbon(aes(ymin = rmse - rmse_sd, ymax = rmse + rmse_sd, fill = "RMSE SD"), alpha = 0.2) +
  geom_point(aes(y = rmse, color = "Mean RMSE")) +
  labs(title = "Model Performance vs Train/Test Split Ratio",
       x = "Training Data Ratio",
       y = "RMSE",
       color = "Metric",
       fill = "Metric") +
  scale_color_manual(values = c("Mean RMSE" = "blue")) +
  scale_fill_manual(values = c("RMSE SD" = "blue")) +
  theme_minimal()

# Use the best ratio for the final split
set.seed(123)
train_index <- sample(1:nrow(DB), floor(nrow(DB) * best_ratio))
train_data <- DB[train_index, ]
test_data <- DB[-train_index, ]

cat("\nFinal split sizes:\n")
cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

# Build multiple linear regression model
nox_model <- lm(AQ_nox ~ Time + Month_num + Day_of_week + Province, data = train_data)

# Print model summary
summary(nox_model)

# Make predictions on test set
predictions <- predict(nox_model, newdata = test_data)

sum(is.na(predictions))  # Controlla se ci sono valori NA nelle previsioni

# Calculate RMSE
rmse <- sqrt(mean((test_data$AQ_nox - predictions)^2, na.rm = TRUE))
cat("Root Mean Square Error:", rmse, "\n")

# Plot actual vs predicted values
ggplot(data.frame(actual = test_data$AQ_nox, predicted = predictions), 
       aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted NOx Values",
       x = "Actual NOx",
       y = "Predicted NOx") +
  theme_minimal()

# Calculate residuals
residuals <- test_data$AQ_nox - predictions

# Control for NA values in residuals
sum(is.na(residuals))



# Create a data frame with residuals and actual values
residual_df <- data.frame(
  actual = test_data$AQ_nox,
  predicted = predictions,
  residuals = residuals,
  time = test_data$Time
)

# Plot residuals vs predicted values
ggplot(residual_df, aes(x = predicted, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Residuals vs Predicted Values",
       x = "Predicted Values",
       y = "Residuals") +
  theme_minimal()

# Plot residuals vs time
ggplot(residual_df, aes(x = time, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Residuals vs Time",
       x = "Time",
       y = "Residuals") +
  theme_minimal()

# Plot residuals histogram with normal curve
ggplot(residual_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(residual_df$residuals), 
                                       sd = sd(residual_df$residuals)),
                color = "red", size = 1) +
  labs(title = "Distribution of Residuals",
       x = "Residuals",
       y = "Density") +
  theme_minimal()

# Calculate and plot autocorrelation of residuals
acf_residuals <- acf(residuals, plot = FALSE)
plot(acf_residuals, main = "Autocorrelation of Residuals")

# Durbin-Watson test for autocorrelation
library(lmtest)
dwtest(nox_model)

# Shapiro-Wilk test for normality of residuals
shapiro.test(residuals)

# Print summary statistics of residuals
cat("\nResiduals Summary Statistics:\n")
summary(residuals)
cat("\nStandard Deviation of Residuals:", sd(residuals), "\n")

# Load required package for ARMA modeling
#install.packages("forecast")
library(forecast)

# Convert residuals to time series object
residuals_ts <- ts(residuals, frequency = 24)  # Assuming daily data

# Find best ARMA model using auto.arima
best_arma <- auto.arima(residuals_ts, 
                       seasonal = TRUE,
                       stepwise = FALSE,
                       approximation = FALSE,
                       trace = TRUE)

# Print the best model summary
print(summary(best_arma))

# Get the residuals from the ARMA model
arma_residuals <- residuals(best_arma)

# Plot the original residuals vs ARMA model residuals
par(mfrow = c(2, 2))
plot(residuals_ts, main = "Original Residuals")
plot(arma_residuals, main = "ARMA Model Residuals")
acf(residuals_ts, main = "ACF of Original Residuals")
acf(arma_residuals, main = "ACF of ARMA Residuals")

# Test for autocorrelation in ARMA residuals
Box.test(arma_residuals, type = "Ljung-Box")

# Shapiro test for normality of ARMA residuals
shapiro.test(arma_residuals)

# Create a new model that combines the original regression with ARMA
# First, get the fitted values from the original model
fitted_values <- fitted(nox_model)

# Create a new time series with the original data
y_ts <- ts(train_data$AQ_nox, frequency = 24)

# Fit ARIMA model to the original data
best_arima <- auto.arima(y_ts, 
                        xreg = model.matrix(nox_model)[,-1],  # Remove intercept
                        seasonal = TRUE,
                        stepwise = TRUE,
                        approximation = TRUE)

# Print the combined model summary
print(summary(best_arima))

# Make predictions with the new model
new_xreg <- model.matrix(nox_model, data = test_data)[,-1]  # Remove intercept
arima_forecast <- forecast(best_arima, xreg = new_xreg, h = nrow(test_data))

# Calculate new RMSE
new_rmse <- sqrt(mean((test_data$AQ_nox - arima_forecast$mean)^2, na.rm = TRUE))
cat("\nNew RMSE with ARIMA model:", new_rmse, "\n")

# Compare original and new residuals
par(mfrow = c(2, 2))
plot(residuals, main = "Original Model Residuals")
plot(arima_forecast$residuals, main = "ARIMA Model Residuals")
acf(residuals, main = "ACF of Original Residuals")
acf(arima_forecast$residuals, main = "ACF of ARIMA Residuals")

# Test for autocorrelation in new residuals
Box.test(arima_forecast$residuals, type = "Ljung-Box")

# Shapiro test for new residuals
shapiro.test(arima_forecast$residuals)

