rm(list=ls())

library(dplyr)
library(glmnet)
library(forecast)
library(dplyr)
library(lubridate)
library("ggplot2")
library(tidyverse)
library(car) # vif
library(glmnet) # LASSO
library(moments) # jarque.test
library(caret) # external validation
library(DataExplorer)
library(zoo)
library(imputeTS)
library(openxlsx)
library(tseries) # For adf.test
library(forecast)
library(future.apply)
library(imputeTS)
library(future)
library(rugarch) #garch
library(randomForest) # For random forest

plan(multisession, workers = 12 )  # Usa tutti i core

BG_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/BG_DB_impute.xlsx", sheet = "Sheet1")


#remove the "IDStations" column from the dataset
BG_DBi_clean <- BG_DBi %>% select(-IDStations)

#creating train + test data
cutoff_trend <- 18261
train_data <- BG_DBi_clean %>% filter(Trend <= cutoff_trend)
test_data  <- BG_DBi_clean %>% filter(Trend > cutoff_trend)

# dickey fuller test for stationarity on AQ_nox on Bergamo
adf_result_default_BG <- adf.test(BG_DBi_clean$AQ_nox)
cat("ADF Test for Stationarity on AQ_nox:\n")
print(adf_result_default_BG)

#baseline model for AQ_nox using mean
baseline_mean <- mean(BG_DBi_clean$AQ_nox)
cat("Baseline Mean for AQ_nox:", baseline_mean, "\n")

#calculate root Mean Squared Error (RMSE) for the baseline model
baseline_rmse <- sqrt(mean((BG_DBi_clean$AQ_nox - baseline_mean)^2))
cat("Baseline RMSE for AQ_nox:", baseline_rmse, "\n")

# Load the randomForest library
library(randomForest)

# Set seed for reproducibility
set.seed(123)


# Build the random forest model predicting AQ_nox using all other predictors from training data
rf_model <- randomForest(AQ_nox ~ ., data = train_data)
print(rf_model)
#calculating residuals on train data
train_predictions <- predict(rf_model, newdata = train_data)
train_residuals <- train_data$AQ_nox - train_predictions

# Use the model to predict AQ_nox on the test set
predictions <- predict(rf_model, newdata = test_data)

# calculate RMSE on train set
train_mse <- mean((train_predictions - train_data$AQ_nox)^2)
train_rmse <- sqrt(train_mse)
cat("Train Root Mean Squared Error:", train_rmse, "\n")

# Calculate Mean Squared Error (MSE) for the predictions
mse <- mean((predictions - test_data$AQ_nox)^2)
cat("Mean Squared Error:", mse, "\n")

# Calculate Root Mean Squared Error (RMSE) for the predictions
rmse <- sqrt(mse)
cat("Root Mean Squared Error:", rmse, "\n")


# Plot predicted vs. actual values
library(ggplot2)

plot_data <- data.frame(
  Actual = test_data$AQ_nox,
  Predicted = predictions
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Random Forest Predictions vs. Actual Values BG",
    x = "Actual AQ_nox",
    y = "Predicted AQ_nox"
  ) +
  theme_minimal()

# Calculate residuals
test_residuals <- test_data$AQ_nox - predictions

# Prepare data for residual plot
residual_plot_data <- data.frame(
  Actual = test_data$AQ_nox,
  Residuals = test_residuals
)

# Plot residuals
ggplot(residual_plot_data, aes(x = Actual, y = test_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals of Random Forest Model BG",
    x = "Actual AQ_nox",
    y = "test_residuals"
  ) +
  theme_minimal()

# Plot histogram of residuals to check normality
ggplot(residual_plot_data, aes(x = test_residuals)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Residuals BG",
    x = "test_residuals",
    y = "Frequency"
  ) +
  theme_minimal()

# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(test_residuals)
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test)

# Perform Durbin-Watson test for autocorrelation
library(lmtest)
dw_test <- dwtest(test_data$AQ_nox ~ predictions)
cat("Durbin-Watson Test for Autocorrelation:\n")
print(dw_test)

# Q-Q plot for residuals
qqnorm(test_residuals)
qqline(test_residuals, col = "red", lwd = 2)

# ACF plot for residuals
library(forecast)
Acf(test_residuals, main = "Autocorrelation of Residuals")

# PACF plot for residuals
library(forecast)
Pacf(test_residuals, main = "Partial Autocorrelation of Residuals")

# Train the ARMA model on train_residuals
arma_model <- auto.arima(train_residuals, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)

#calculate ARMA train residuals
arma_residuals <- residuals(arma_model)

# Fit the ARMA model on test_residuals
arma_fitted_test <- Arima(test_residuals, model = arma_model)
cat("Fitted ARMA Model on Test Residuals:\n")
print(arma_fitted_test)

# Extract residuals from the fitted ARMA model on test data
arma_test_residuals <- residuals(arma_fitted_test)

# Calculate RMSE for the ARMA model on train residuals
arma_train_rmse <- sqrt(mean(arma_residuals^2))
cat("RMSE for ARMA Model on Test Residuals:", arma_train_rmse, "\n")

# Calculate corrected train RMSE after ARMA modeling
corrected_train_mse <- mean(arma_residuals^2)
corrected_train_rmse <- sqrt(corrected_train_mse)
cat("Corrected RMSE with ARMA residual modeling:", corrected_train_rmse, "\n")

# Calculate RMSE for the ARMA model on test residuals
arma_test_rmse <- sqrt(mean(arma_test_residuals^2))
cat("RMSE for ARMA Model on Test Residuals:", arma_test_rmse, "\n")

# Calculate corrected test RMSE after ARMA modeling
corrected_mse <- mean(arma_test_residuals^2)
corrected_rmse <- sqrt(corrected_mse)
cat("Corrected RMSE with ARMA residual modeling:", corrected_rmse, "\n")

# ACF and PACF of residuals after ARMA correction
Acf(residuals(arma_fitted_test), main = "ACF of ARMA residuals")
Pacf(residuals(arma_fitted_test), main = "PACF of ARMA residuals")

# QQ plot of residuals after ARMA
qqnorm(residuals(arma_fitted_test), main = "Q-Q Plot of ARMA Residuals")
qqline(residuals(arma_fitted_test), col = "red", lwd = 2)

# Print the final ARMA model
cat("Final ARMA Model:\n")
print(arma_model)


# Check for heteroscedasticity in the residuals
library(FinTS)
ArchTest(residuals(arma_fitted_test), lags = 12)

# ------------------------
# Fit GARCH model to ARMA residuals
# ------------------------
library(rugarch)

# Define GARCH(1,1) model with t-distribution
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(7, 5), include.mean = FALSE),
  distribution.model = "std"
)

# Fit the GARCH train model
fit_garch <- ugarchfit(spec = spec_garch, data = arma_residuals)

# Fit the GARCH test model
fit_garch_test <- ugarchfit(spec = spec_garch, data = arma_test_residuals)

# Print GARCH model summary
cat("GARCH model summary:\n")
show(fit_garch_test)

# Plot standardized residuals from test set
garch_resid <- residuals(fit_garch_test, standardize = TRUE)

#standardized residuals from train set
garch_resid_train <- residuals(fit_garch, standardize = TRUE)

# ACF and PACF of GARCH residuals
Acf(garch_resid, main = "ACF of GARCH Standardized Residuals")
Pacf(garch_resid, main = "PACF of GARCH Standardized Residuals")

# Q-Q plot of GARCH standardized residuals
qqnorm(garch_resid, main = "Q-Q Plot of GARCH Residuals")
qqline(garch_resid, col = "red", lwd = 2)

#Transforming garch_resid in vector
garch_resid <- as.vector(garch_resid)

# Calculate RMSE of standardizedGARCH residuals for Bergamo on test set
BG_garch_rmse <- sqrt(mean(garch_resid^2))
cat("RMSE of standardized GARCH residuals on test set (BG):", BG_garch_rmse, "\n")

# Calculate RMSE of standardizedGARCH residuals for Bergamo on train set
BG_garch_train_rmse <- sqrt(mean(garch_resid_train^2))
cat("RMSE of standardized GARCH residuals on train set (BG):", BG_garch_train_rmse, "\n")

# Jarque-Bera test for normality
cat("Jarque-Bera Test for GARCH Residuals:\n")
print(jarque.test(garch_resid))

# Ljung-Box test for autocorrelation
cat("Ljung-Box Test for GARCH Residuals:\n")
print(Box.test(garch_resid, lag = 20, type = "Ljung-Box"))

cat("ARCH Test for Homoskedasticity:\n")
arch_test <- ArchTest(garch_resid, lags = 12)
print(arch_test)

# Test if the mean of residuals is 0
cat("Test if the mean of residuals is 0:\n")
mean_residuals <- mean(garch_resid)
cat("Mean of residuals:", mean_residuals, "\n")
t_test <- t.test(garch_resid, mu = 0)
print(t_test)

# Augmented Dickey-Fuller test for stationarity of residuals
adf_result <- adf.test(garch_resid)
print(adf_result)

# Load the MI_DBi dataset
MI_DBi <- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MI_DB_impute.xlsx", sheet = "Sheet1")

# Clean the MI_DBi dataset
MI_DBi_clean <- MI_DBi %>% select(-IDStations)  # Remove the "IDStations" column

# dickey fuller test for stationarity on AQ_nox on Milano
adf_result_default_MI <- adf.test(MI_DBi_clean$AQ_nox)
cat("ADF Test for Stationarity on AQ_nox:\n")
print(adf_result_default_MI)

#baseline model for AQ_nox using mean
baseline_mean <- mean(MI_DBi_clean$AQ_nox)
cat("Baseline Mean for AQ_nox:", baseline_mean, "\n")

#calculate root Mean Squared Error (RMSE) for the baseline model
baseline_rmse <- sqrt(mean((MI_DBi_clean$AQ_nox - baseline_mean)^2))
cat("Baseline RMSE for AQ_nox:", baseline_rmse, "\n")

#creating train + test data
MI_train_data <- MI_DBi_clean %>% filter(Trend <= cutoff_trend)
MI_test_data  <- MI_DBi_clean %>% filter(Trend > cutoff_trend)

# Build the random forest model predicting AQ_nox using all other predictors from train set
MI_rf_model <- randomForest(AQ_nox ~ ., data = MI_train_data)
print(MI_rf_model)

# Use the model to predict AQ_nox on the test set
MI_predictions <- predict(MI_rf_model, newdata = MI_test_data)

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE) on train set
MI_train_predictions <- predict(MI_rf_model, newdata = MI_train_data)
MI_train_mse <- mean((MI_train_predictions - MI_train_data$AQ_nox)^2)
MI_train_rmse <- sqrt(MI_train_mse)
cat("Train Mean Squared Error (MSE) for MI_DBi:", MI_train_mse, "\n")
cat("Train Root Mean Squared Error (RMSE) for MI_DBi:", MI_train_rmse, "\n")

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE) on test set
MI_mse <- mean((MI_predictions - MI_test_data$AQ_nox)^2)
MI_rmse <- sqrt(MI_mse)
cat("Mean Squared Error (MSE) for MI_DBi:", MI_mse, "\n")
cat("Root Mean Squared Error (RMSE) for MI_DBi:", MI_rmse, "\n")

# Calculate test residuals
MI_test_residuals <- MI_test_data$AQ_nox - MI_predictions

# calculate traiin residuals
MI_train_residuals <- MI_train_data$AQ_nox - MI_train_predictions

ggplot(plot_data, aes(x = Actual, y = MI_test_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Random Forest Predictions vs. Actual Values MI",
    x = "Actual AQ_nox",
    y = "Predicted AQ_nox_MI"
  ) +
  theme_minimal()

# Plot residuals
ggplot(residual_plot_data, aes(x = Actual, y = MI_test_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals of Random Forest Model MI",
    x = "Actual AQ_nox",
    y = "MI_Residuals"
  ) +
  theme_minimal()

# Plot histogram of residuals to check normality
ggplot(residual_plot_data, aes(x = MI_test_residuals)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Residuals MI",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(MI_test_residuals)
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test)

# Perform Durbin-Watson test for autocorrelation
library(lmtest)
dw_test <- dwtest(test_data$AQ_nox ~ MI_test_residuals)
cat("Durbin-Watson Test for Autocorrelation:\n")
print(dw_test)

# Q-Q plot for residuals
qqnorm(MI_test_residuals)
qqline(MI_test_residuals, col = "red", lwd = 2)

# ACF plot for residuals
library(forecast)
Acf(MI_test_residuals, main = "Autocorrelation of Residuals")

# PACF plot for residuals
library(forecast)
Pacf(MI_test_residuals, main = "Partial Autocorrelation of Residuals")

#train arma model on train residuals
MI_arma_model <- auto.arima(MI_train_residuals, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
#calculate ARMA train residuals
MI_arma_train_residuals <- residuals(MI_arma_model)
#print ARMA
cat("ARMA Model Summary:\n")
print(MI_arma_model)


# Fit the ARMA model on test_residuals MI
MI_arma_fitted_test <- Arima(MI_test_residuals, model = MI_arma_model)
cat("Fitted ARMA Model on Test Residuals:\n")
print(MI_arma_fitted_test)
# Extract residuals from the fitted ARMA model on test data MI
MI_arma_test_residuals <- residuals(MI_arma_fitted_test)

#Calculate  RMSE on test set after ARMA modeling
MI_arma_test_mse <- mean(MI_arma_test_residuals^2)
MI_arma_test_rmse <- sqrt(MI_arma_test_mse)
cat(" RMSE with ARMA test residual modeling (MI):", MI_arma_test_rmse, "\n")

# Calculate train RMSE after ARMA modeling
MI_arma_train_mse <- mean(MI_arma_train_residuals^2)
MI_arma_train_rmse <- sqrt(MI_arma_train_mse)
cat("Train RMSE with ARMA train residual modeling (MI):", MI_arma_train_rmse, "\n")

# ACF and PACF of residuals after ARMA correction
Acf(residuals(MI_arma_fitted_test), main = "ACF of ARMA residuals")
Pacf(residuals(MI_arma_fitted_test), main = "PACF of ARMA residuals")

# QQ plot of residuals after ARMA
qqnorm(residuals(MI_arma_fitted_test), main = "Q-Q Plot of ARMA Residuals")
qqline(residuals(MI_arma_fitted_test), col = "red", lwd = 2)

# Check for heteroscedasticity in the residuals
library(FinTS)
ArchTest(residuals(MI_arma_fitted_test), lags = 12)

# Fit a GARCH model to the ARMA residuals
MI_spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(7, 5), include.mean = FALSE),
  distribution.model = "std"
)
MI_fit_test_garch <- ugarchfit(spec = MI_spec_garch, data = MI_arma_test_residuals)

#fit to train set
MI_fit_train_garch <- ugarchfit(spec = MI_spec_garch, data = MI_arma_train_residuals)

# Extract standardized test residuals from the GARCH model
MI_garch_test_resid <- residuals(MI_fit_test_garch, standardize = TRUE)

# Extract standardized train residuals from the GARCH model
MI_garch_train_resid <- residuals(MI_fit_train_garch, standardize = TRUE)


# Perform diagnostic tests
cat("ARCH Test for Homoskedasticity (MI_DBi):\n")
  MI_arch_test <- ArchTest(MI_garch_test_resid, lags = 12)
  print(MI_arch_test)


cat("Test if the mean of residuals is 0 (MI_DBi):\n")
MI_mean_residuals <- mean(MI_garch_test_resid)
cat("Mean of residuals (MI_DBi):", MI_mean_residuals, "\n")
MI_t_test <- t.test(MI_garch_test_resid, mu = 0)
print(MI_t_test)

# Calculate test RMSE of test standardized GARCH residuals for Milan
MI_garch_test_rmse <- sqrt(mean(MI_garch_test_resid^2))
cat("RMSE of test set standardized GARCH residuals (MI):", MI_garch_test_rmse, "\n")

# Calculate train RMSE of standardized GARCH residuals for Milan
MI_garch_train_rmse <- sqrt(mean(MI_garch_train_resid^2))
cat("RMSE of train set standardized GARCH residuals (MI):", MI_garch_train_rmse, "\n")

# Augmented Dickey-Fuller test for stationarity of residuals
adf_result <- adf.test(MI_garch_test_resid)
print(adf_result)



# Additional diagnostic plots
Acf(MI_garch_test_resid, main = "ACF of GARCH Standardized Residuals (MI_DBi)")
Pacf(MI_garch_test_resid, main = "PACF of GARCH Standardized Residuals (MI_DBi)")
qqnorm(MI_garch_test_resid, main = "Q-Q Plot of GARCH Residuals (MI_DBi)")
qqline(MI_garch_test_resid, col = "red", lwd = 2)

# Load the MN_DBi dataset
MN_DBi <- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MN_DB_impute.xlsx", sheet = "Sheet1")

# Clean the MN_DBi dataset
MN_DBi_clean <- MN_DBi %>% select(-IDStations)  # Remove the "IDStations" column

# dickey fuller test for stationarity on AQ_nox on Mantova
adf_result_default_MN <- adf.test(MN_DBi_clean$AQ_nox)
cat("ADF Test for Stationarity on AQ_nox:\n")
print(adf_result_default_MN)

#baseline model for AQ_nox using mean
baseline_mean_MN <- mean(MN_DBi_clean$AQ_nox)
cat("Baseline Mean for AQ_nox:", baseline_mean_MN, "\n")

#calculate root Mean Squared Error (RMSE) for the baseline model
baseline_MN_rmse <- sqrt(mean((MN_DBi_clean$AQ_nox - baseline_mean_MN)^2))
cat("Baseline RMSE for AQ_nox:", baseline_MN_rmse, "\n")

#creating train + test data
MN_train_data <- MN_DBi_clean %>% filter(Trend <= cutoff_trend)
MN_test_data  <- MN_DBi_clean %>% filter(Trend > cutoff_trend)

# Build the random forest model predicting AQ_nox using all other predictors from train set
MN_rf_model <- randomForest(AQ_nox ~ ., data = MN_train_data)
print(MN_rf_model)

# Use the model to predict AQ_nox on the test set
MN_predictions <- predict(MN_rf_model, newdata = MN_test_data)

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE) on train set
MN_train_predictions <- predict(MN_rf_model, newdata = MN_train_data)
MN_train_mse <- mean((MN_train_predictions - MN_train_data$AQ_nox)^2)
MN_train_rmse <- sqrt(MN_train_mse)
cat("Train Mean Squared Error (MSE) for MN_DBi:", MN_train_mse, "\n")
cat("Train Root Mean Squared Error (RMSE) for MN_DBi:", MN_train_rmse, "\n")

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE) on test set
MN_mse <- mean((MN_predictions - MN_test_data$AQ_nox)^2)
MN_rmse <- sqrt(MN_mse)
cat("Mean Squared Error (MSE) for MN_DBi:", MN_mse, "\n")
cat("Root Mean Squared Error (RMSE) for MN_DBi:", MN_rmse, "\n")

# Calculate test residuals
MN_test_residuals <- MN_test_data$AQ_nox - MN_predictions

# calculate traiin residuals
MN_train_residuals <- MN_train_data$AQ_nox - MN_train_predictions

ggplot(plot_data, aes(x = Actual, y = MN_test_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Random Forest Predictions vs. Actual Values MN",
    x = "Actual AQ_nox",
    y = "Predicted AQ_nox_MN"
  ) +
  theme_minimal()

# Plot residuals
ggplot(residual_plot_data, aes(x = Actual, y = MN_test_residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals of Random Forest Model MN",
    x = "Actual AQ_nox",
    y = "MN_Residuals"
  ) +
  theme_minimal()

# Plot histogram of residuals to check normality
ggplot(residual_plot_data, aes(x = MN_test_residuals)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Residuals MN",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

# Perform Shapiro-Wilk test for normality
shapiro_test_MN <- shapiro.test(MN_test_residuals)
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test_MN)

# Perform Durbin-Watson test for autocorrelation
library(lmtest)
dw_test_MN <- dwtest(test_data$AQ_nox ~ MN_test_residuals)
cat("Durbin-Watson Test for Autocorrelation:\n")
print(dw_test_MN)

# Q-Q plot for residuals
qqnorm(MN_test_residuals)
qqline(MN_test_residuals, col = "red", lwd = 2)

# ACF plot for residuals
library(forecast)
Acf(MN_test_residuals, main = "Autocorrelation of Residuals")

# PACF plot for residuals
library(forecast)
Pacf(MN_test_residuals, main = "Partial Autocorrelation of Residuals")

#train arma model on train residuals
MN_arma_model <- auto.arima(MN_train_residuals, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
#calculate ARMA train residuals
MN_arma_train_residuals <- residuals(MN_arma_model)
#print ARMA
cat("ARMA Model Summary:\n")
print(MN_arma_model)


# Fit the ARMA model on test_residuals MN
MN_arma_fitted_test <- Arima(MN_test_residuals, model = MN_arma_model)
cat("Fitted ARMA Model on Test Residuals:\n")
print(MN_arma_fitted_test)
# Extract residuals from the fitted ARMA model on test data MN
MN_arma_test_residuals <- residuals(MN_arma_fitted_test)

#Calculate  RMSE on test set after ARMA modeling
MN_arma_test_mse <- mean(MN_arma_test_residuals^2)
MN_arma_test_rmse <- sqrt(MN_arma_test_mse)
cat(" RMSE with ARMA test residual modeling (MN):", MN_arma_test_rmse, "\n")

# Calculate train RMSE after ARMA modeling
MN_arma_train_mse <- mean(MN_arma_train_residuals^2)
MN_arma_train_rmse <- sqrt(MN_arma_train_mse)
cat("Train RMSE with ARMA train residual modeling (MN):", MN_arma_train_rmse, "\n")

# ACF and PACF of residuals after ARMA correction
Acf(residuals(MN_arma_fitted_test), main = "ACF of ARMA residuals")
Pacf(residuals(MN_arma_fitted_test), main = "PACF of ARMA residuals")

# QQ plot of residuals after ARMA
qqnorm(residuals(MN_arma_fitted_test), main = "Q-Q Plot of ARMA Residuals")
qqline(residuals(MN_arma_fitted_test), col = "red", lwd = 2)

# Check for heteroscedasticity in the residuals
library(FinTS)
ArchTest(residuals(MN_arma_fitted_test), lags = 12)

# Fit a GARCH model to the ARMA residuals
MN_spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(7, 5), include.mean = FALSE),
  distribution.model = "std"
)
MN_fit_test_garch <- ugarchfit(spec = MN_spec_garch, data = MN_arma_test_residuals)

#fit to train set
MN_fit_train_garch <- ugarchfit(spec = MN_spec_garch, data = MN_arma_train_residuals)

# Extract standardized test residuals from the GARCH model
MN_garch_test_resid <- residuals(MN_fit_test_garch, standardize = TRUE)

# Extract standardized train residuals from the GARCH model
MN_garch_train_resid <- residuals(MN_fit_train_garch, standardize = TRUE)


# Perform diagnostic tests
cat("ARCH Test for Homoskedasticity (MN_DBi):\n")
MN_arch_test <- ArchTest(MN_garch_test_resid, lags = 12)
print(MN_arch_test)


cat("Test if the mean of residuals is 0 (MN_DBi):\n")
MN_mean_residuals <- mean(MN_garch_test_resid)
cat("Mean of residuals (MN_DBi):", MN_mean_residuals, "\n")
MN_t_test <- t.test(MN_garch_test_resid, mu = 0)
print(MN_t_test)

# Calculate test RMSE of test standardized GARCH residuals for Mantova
MN_garch_test_rmse <- sqrt(mean(MN_garch_test_resid^2))
cat("RMSE of test set standardized GARCH residuals (MN):", MN_garch_test_rmse, "\n")

# Calculate train RMSE of standardized GARCH residuals for Mantova
MN_garch_train_rmse <- sqrt(mean(MN_garch_train_resid^2))
cat("RMSE of train set standardized GARCH residuals (MN):", MN_garch_train_rmse, "\n")

# Augmented Dickey-Fuller test for stationarity of residuals
adf_result <- adf.test(MN_garch_test_resid)
print(adf_result)



# Additional diagnostic plots
Acf(MN_garch_test_resid, main = "ACF of GARCH Standardized Residuals (MN_DBi)")
Pacf(MN_garch_test_resid, main = "PACF of GARCH Standardized Residuals (MN_DBi)")
qqnorm(MN_garch_test_resid, main = "Q-Q Plot of GARCH Residuals (MN_DBi)")
qqline(MN_garch_test_resid, col = "red", lwd = 2)




