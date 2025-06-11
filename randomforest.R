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

# dickey fuller test for stationarity on AQ_nox on Bergamo
adf_result_default_BG <- adf.test(BG_DBi_clean$AQ_nox)
cat("ADF Test for Stationarity on AQ_nox:\n")
print(adf_result_default_BG)

# Load the randomForest library
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%) and testing (30%) sets
sample_index <- sample(1:nrow(BG_DBi_clean), size = 0.7 * nrow(BG_DBi_clean))
train_data <- BG_DBi_clean[sample_index, ]
test_data <- BG_DBi_clean[-sample_index, ]

# Build the random forest model predicting AQ_nox using all other predictors
rf_model <- randomForest(AQ_nox ~ ., data = train_data)
print(rf_model)

# Use the model to predict AQ_nox on the test set
predictions <- predict(rf_model, newdata = test_data)



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
    title = "Random Forest Predictions vs. Actual Values",
    x = "Actual AQ_nox",
    y = "Predicted AQ_nox"
  ) +
  theme_minimal()

# Calculate residuals
residuals <- test_data$AQ_nox - predictions

# Prepare data for residual plot
residual_plot_data <- data.frame(
  Actual = test_data$AQ_nox,
  Residuals = residuals
)

# Plot residuals
ggplot(residual_plot_data, aes(x = Actual, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals of Random Forest Model",
    x = "Actual AQ_nox",
    y = "Residuals"
  ) +
  theme_minimal()

# Plot histogram of residuals to check normality
ggplot(residual_plot_data, aes(x = Residuals)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals",
    y = "Frequency"
  ) +
  theme_minimal()

# Perform Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals)
cat("Shapiro-Wilk Test for Normality:\n")
print(shapiro_test)

# Perform Durbin-Watson test for autocorrelation
library(lmtest)
dw_test <- dwtest(test_data$AQ_nox ~ predictions)
cat("Durbin-Watson Test for Autocorrelation:\n")
print(dw_test)

# Q-Q plot for residuals
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

# ACF plot for residuals
library(forecast)
Acf(residuals, main = "Autocorrelation of Residuals")

# PACF plot for residuals
library(forecast)
Pacf(residuals, main = "Partial Autocorrelation of Residuals")

# Fit an ARMA model to the residuals to try to capture autocorrelation
arma_model <- auto.arima(residuals, stationary=TRUE, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)

# Get residuals from ARMA model
arma_residuals <- residuals(arma_model)

# Calculate corrected RMSE after ARMA modeling
corrected_mse <- mean(arma_residuals^2)
corrected_rmse <- sqrt(corrected_mse)
cat("Corrected RMSE with ARMA residual modeling:", corrected_rmse, "\n")

# ACF and PACF of residuals after ARMA correction
Acf(residuals(arma_model), main = "ACF of ARMA residuals")
Pacf(residuals(arma_model), main = "PACF of ARMA residuals")

# QQ plot of residuals after ARMA
qqnorm(residuals(arma_model), main = "Q-Q Plot of ARMA Residuals")
qqline(residuals(arma_model), col = "red", lwd = 2)

# Print the final ARMA model
cat("Final ARMA Model:\n")
print(arma_model)


# Check for heteroscedasticity in the residuals
library(FinTS)
ArchTest(residuals(arma_model), lags = 12)

# ------------------------
# Fit GARCH model to ARMA residuals
# ------------------------
library(rugarch)

# Define GARCH(1,1) model with t-distribution
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(1, 1), include.mean = FALSE),
  distribution.model = "std"
)

# Fit the GARCH model
fit_garch <- ugarchfit(spec = spec_garch, data = arma_residuals)

# Print GARCH model summary
cat("GARCH model summary:\n")
show(fit_garch)

# Plot standardized residuals
garch_resid <- residuals(fit_garch, standardize = TRUE)

# ACF and PACF of GARCH residuals
Acf(garch_resid, main = "ACF of GARCH Standardized Residuals")
Pacf(garch_resid, main = "PACF of GARCH Standardized Residuals")

# Q-Q plot of GARCH standardized residuals
qqnorm(garch_resid, main = "Q-Q Plot of GARCH Residuals")
qqline(garch_resid, col = "red", lwd = 2)

#Transforming garch_resid in vector
garch_resid <- as.vector(garch_resid)

# Calculate RMSE of standardized GARCH residuals for Bergamo
BG_garch_rmse <- sqrt(mean(garch_resid^2))
cat("RMSE of standardized GARCH residuals (BG):", BG_garch_rmse, "\n")

#transforming garch_resid in logaritmic. CAREFULL THIS BREAKS MEAN
#garch_resid <- log(abs(garch_resid) + 1e-6)  # Adding a small constant to avoid log(0)

# Jarque-Bera test for normality
cat("Jarque-Bera Test for GARCH Residuals:\n")
print(jarque.test(garch_resid))

# Ljung-Box test for autocorrelation
cat("Ljung-Box Test for GARCH Residuals:\n")
print(Box.test(garch_resid, lag = 20, type = "Ljung-Box"))

cat("ARCH Test for Homoskedasticity:\n")
if (exists("garch_resid")) {
  arch_test <- ArchTest(garch_resid, lags = 12)
  print(arch_test)
} else {
  cat("Error: 'garch_resid' object not found. Ensure the GARCH model is fitted correctly.\n")
}

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

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # Set seed for reproducibility
MI_sample_index <- sample(1:nrow(MI_DBi_clean), size = 0.7 * nrow(MI_DBi_clean))
MI_train_data <- MI_DBi_clean[MI_sample_index, ]
MI_test_data <- MI_DBi_clean[-MI_sample_index, ]

# Build the random forest model predicting AQ_nox using all other predictors
MI_rf_model <- randomForest(AQ_nox ~ ., data = MI_train_data)
print(MI_rf_model)

# Use the model to predict AQ_nox on the test set
MI_predictions <- predict(MI_rf_model, newdata = MI_test_data)

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
MI_mse <- mean((MI_predictions - MI_test_data$AQ_nox)^2)
MI_rmse <- sqrt(MI_mse)
cat("Mean Squared Error (MSE) for MI_DBi:", MI_mse, "\n")
cat("Root Mean Squared Error (RMSE) for MI_DBi:", MI_rmse, "\n")

# Calculate residuals
MI_residuals <- MI_test_data$AQ_nox - MI_predictions

# Fit an ARMA model to the residuals
MI_arma_model <- auto.arima(MI_residuals, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
MI_arma_residuals <- residuals(MI_arma_model)

#Calculate corrected RMSE after ARMA modeling
MI_corrected_mse <- mean(MI_arma_residuals^2)
MI_corrected_rmse <- sqrt(MI_corrected_mse)
cat("Corrected RMSE with ARMA residual modeling (MI):", MI_corrected_rmse, "\n")

# Fit a GARCH model to the ARMA residuals
MI_spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(7, 5), include.mean = FALSE),
  distribution.model = "std"
)
MI_fit_garch <- ugarchfit(spec = MI_spec_garch, data = MI_arma_residuals)

# Extract standardized residuals from the GARCH model
MI_garch_resid <- residuals(MI_fit_garch, standardize = TRUE)

# Perform diagnostic tests
cat("ARCH Test for Homoskedasticity (MI_DBi):\n")
if (exists("MI_garch_resid")) {
  MI_arch_test <- ArchTest(MI_garch_resid, lags = 12)
  print(MI_arch_test)
} else {
  cat("Error: 'MI_garch_resid' object not found. Ensure the GARCH model is fitted correctly.\n")
}

cat("Test if the mean of residuals is 0 (MI_DBi):\n")
MI_mean_residuals <- mean(MI_garch_resid)
cat("Mean of residuals (MI_DBi):", MI_mean_residuals, "\n")
MI_t_test <- t.test(MI_garch_resid, mu = 0)
print(MI_t_test)


# Calculate RMSE of standardized GARCH residuals for Milan
MI_garch_rmse <- sqrt(mean(MI_garch_resid^2))
cat("RMSE of standardized GARCH residuals (MI):", MI_garch_rmse, "\n")

# Augmented Dickey-Fuller test for stationarity of residuals
adf_result <- adf.test(MI_garch_resid)
print(adf_result)

# Additional diagnostic plots
Acf(MI_garch_resid, main = "ACF of GARCH Standardized Residuals (MI_DBi)")
Pacf(MI_garch_resid, main = "PACF of GARCH Standardized Residuals (MI_DBi)")
qqnorm(MI_garch_resid, main = "Q-Q Plot of GARCH Residuals (MI_DBi)")
qqline(MI_garch_resid, col = "red", lwd = 2)

# Load the MN_DBi dataset
MN_DBi <- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MN_DB_impute.xlsx", sheet = "Sheet1")

# Clean the MN_DBi dataset
MN_DBi_clean <- MN_DBi %>% select(-IDStations)  # Remove the "IDStations" column

# dickey fuller test for stationarity on AQ_nox on Mantova
adf_result_default_MN <- adf.test(MN_DBi_clean$AQ_nox)
cat("ADF Test for Stationarity on AQ_nox:\n")
print(adf_result_default_MN)

# Split the data into training (70%) and testing (30%) sets
set.seed(123)  # Set seed for reproducibility
MN_sample_index <- sample(1:nrow(MN_DBi_clean), size = 0.7 * nrow(MN_DBi_clean))
MN_train_data <- MN_DBi_clean[MN_sample_index, ]
MN_test_data <- MN_DBi_clean[-MN_sample_index, ]

# Build the random forest model predicting AQ_nox using all other predictors
MN_rf_model <- randomForest(AQ_nox ~ ., data = MN_train_data)
print(MN_rf_model)

# Use the model to predict AQ_nox on the test set
MN_predictions <- predict(MN_rf_model, newdata = MN_test_data)

# Calculate Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
MN_mse <- mean((MN_predictions - MN_test_data$AQ_nox)^2)
MN_rmse <- sqrt(MN_mse)
cat("Mean Squared Error (MSE) for MN_DBi:", MN_mse, "\n")
cat("Root Mean Squared Error (RMSE) for MN_DBi:", MN_rmse, "\n")

# Calculate residuals
MN_residuals <- MN_test_data$AQ_nox - MN_predictions

# Fit an ARMA model to the residuals
MN_arma_model <- auto.arima(MN_residuals, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
MN_arma_residuals <- residuals(MN_arma_model)

#Calculate corrected RMSE after ARMA modeling
MN_arma_model <- auto.arima(MN_residuals, stationary = TRUE, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
MN_arma_residuals <- residuals(MN_arma_model)

# Calculate corrected RMSE after ARMA modeling
MN_arma_rmse <- sqrt(mean(MN_arma_residuals^2))
cat("RMSE dopo ARIMA (MN):", MN_arma_rmse, "\n")

# Fit a GARCH model to the ARMA residuals
MN_spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(7, 5), include.mean = FALSE),
  distribution.model = "std"
)
MN_fit_garch <- ugarchfit(spec = MN_spec_garch, data = MN_arma_residuals)

MN_garch_resid <- residuals(MN_fit_garch, standardize = TRUE)

# Calculate RMSE of standardized GARCH residuals for Mantova
MN_garch_rmse <- sqrt(mean(MN_garch_resid^2))
cat("RMSE of standardized GARCH residuals (MN):", MN_garch_rmse, "\n")

# Perform diagnostic tests
cat("ARCH Test for Homoskedasticity (MN_DBi):\n")
if (exists("MN_garch_resid")) {
  MN_arch_test <- ArchTest(MN_garch_resid, lags = 12)
  print(MN_arch_test)
} else {
  cat("Error: 'MN_garch_resid' object not found. Ensure the GARCH model is fitted correctly.\n")
}

cat("Test if the mean of residuals is 0 (MN_DBi):\n")
MN_mean_residuals <- mean(MN_garch_resid)
cat("Mean of residuals (MN_DBi):", MN_mean_residuals, "\n")
MN_t_test <- t.test(MN_garch_resid, mu = 0)
print(MN_t_test)

# Augmented Dickey-Fuller test for stationarity of residuals
adf_result <- adf.test(MN_garch_resid)
print(adf_result)

# Additional diagnostic plots
Acf(MN_garch_resid, main = "ACF of GARCH Standardized Residuals (MN_DBi)")
Pacf(MN_garch_resid, main = "PACF of GARCH Standardized Residuals (MN_DBi)")
qqnorm(MN_garch_resid, main = "Q-Q Plot of GARCH Residuals (MN_DBi)")
qqline(MN_garch_resid, col = "red", lwd = 2)



