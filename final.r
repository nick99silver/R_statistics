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
library(lmtest) # For Durbin-Watson test

plan(multisession, workers = 12 )  # Usa tutti i core

BG_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/BG_DB_impute.xlsx", sheet = "Sheet1")




#1. Rimuovi la colonna "IDstations" perchè rompe il cazzo
BG_DBi_clean <- BG_DBi %>% select(-IDStations)

# Prepare the data for LASSO
# Assuming 'AQ_nox' is the dependent variable and the rest are predictors
x <- model.matrix(AQ_nox ~ ., data = BG_DBi_clean)[, -1]  # Remove intercept
y <- BG_DBi_clean$AQ_nox

# Perform LASSO regression with cross-validation to find the optimal lambda
set.seed(123)  # For reproducibility
lasso_cv <- cv.glmnet(x, y, alpha = 1)

# Plot the cross-validation results
plot(lasso_cv)

# Extract the best lambda
best_lambda <- lasso_cv$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Fit the LASSO model using the best lambda
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# Extract the variables selected by LASSO
selected_variables <- rownames(coef(lasso_model))[which(coef(lasso_model) != 0)]
cat("Variables selected by LASSO:\n")
print(selected_variables)

# Calculate predictions using the LASSO model
predictions <- predict(lasso_model, s = best_lambda, newx = x)

# Calculate MAE, MSE, and RMSE
mae <- mean(abs(predictions - y))
mse <- mean((predictions - y)^2)
rmse <- sqrt(mse)

cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Output the LASSO model
cat("LASSO Model Coefficients:\n")
print(coef(lasso_model, s = best_lambda))

# Prepare data for plotting
plot_data <- data.frame(
  Actual = y,
  Predicted = as.vector(predictions)
)

# Plot LASSO model predictions vs. real data
library(ggplot2)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "LASSO Model Predictions vs. Real Data",
    x = "Actual AQ_nox",
    y = "Predicted AQ_nox"
  ) +
  theme_minimal()

# Calculate residuals
residuals <- y - as.vector(predictions)

# Prepare data for residual plot
residual_plot_data <- data.frame(
  Actual = y,
  Residuals = residuals
)

# Plot residuals
ggplot(residual_plot_data, aes(x = Actual, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Residuals of LASSO Model",
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
dw_test <- dwtest(y ~ as.vector(predictions))
cat("Durbin-Watson Test for Autocorrelation:\n")
print(dw_test)

# Q-Q plot for residuals
qqnorm(residuals)
qqline(residuals, col = "red", lwd = 2)

# ACF plot for residuals
library(forecast)
Acf(residuals, main = "Autocorrelation of Residuals")


# we can now say LASSO sucks

# Convert the target variable to a time series object
# Assuming 'AQ_nox' is the target variable and has a time component
ts_data <- ts(BG_DBi_clean$AQ_nox, frequency = 7)  # Adjust frequency if needed (e.g., 12 for monthly data)

# Fit the best ARIMA model
cat("Fitting the best ARIMA model...\n")
best_arima <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
print(summary(best_arima))

# Extract residuals from the ARIMA model
arima_residuals <- residuals(best_arima)

# Plot ACF and PACF of ARIMA residuals
Acf(arima_residuals, main = "ACF of ARIMA Residuals")
Pacf(arima_residuals, main = "PACF of ARIMA Residuals")

library(rugarch)

# Define a GARCH(1,1) model with a normal distribution
spec_garch <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = FALSE),
  distribution.model = "norm"
)

# Fit the GARCH model to the ARIMA residuals
cat("Fitting the GARCH model...\n")
fit_garch <- ugarchfit(spec = spec_garch, data = arima_residuals)

# Print the GARCH model summary
cat("GARCH Model Summary:\n")
show(fit_garch)

# Extract standardized residuals
garch_residuals <- residuals(fit_garch, standardize = TRUE)

# Plot ACF and PACF of GARCH residuals
Acf(garch_residuals, main = "ACF of GARCH Residuals")
Pacf(garch_residuals, main = "PACF of GARCH Residuals")

# Perform diagnostic tests
cat("Jarque-Bera Test for Normality:\n")
print(jarque.test(garch_residuals))

cat("Ljung-Box Test for Autocorrelation:\n")
print(Box.test(garch_residuals, lag = 20, type = "Ljung-Box"))

# Get ARIMA fitted values
arima_fitted <- fitted(best_arima)

# Combine ARIMA fitted values with GARCH residuals to get final predictions
final_predictions <- arima_fitted + garch_residuals

# Calculate RMSE for the final model
actual_values <- ts_data  # Assuming ts_data contains the actual values
final_rmse <- sqrt(mean((actual_values - final_predictions)^2))
cat("Root Mean Squared Error (RMSE) for the Final Model (ARIMA + GARCH):", final_rmse, "\n")








