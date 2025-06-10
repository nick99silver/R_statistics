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




#1. Rimuovi la colonna "IDstations" perchè rompe il cazzo
BG_DBi_clean <- BG_DBi %>% select(-IDStations)

# Ensure that the outcome variable AQ_nox is available and remove missing values.
BG_DBi_clean <- BG_DBi_clean %>% filter(!is.na(AQ_nox))

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
  mean.model = list(armaOrder = c(7, 1), include.mean = FALSE),
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

# Jarque-Bera test for normality
cat("Jarque-Bera Test for GARCH Residuals:\n")
print(jarque.test(garch_resid))

# Ljung-Box test for autocorrelation
cat("Ljung-Box Test for GARCH Residuals:\n")
print(Box.test(garch_resid, lag = 20, type = "Ljung-Box"))
