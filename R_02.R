rm(list=ls())

#Libraries installation
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

print("Libraries loaded")
BG_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/BG_DB_impute.xlsx", sheet = "Sheet1")
MI_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MI_DB_impute.xlsx", sheet = "Sheet1")
MN_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MN_DB_impute.xlsx", sheet = "Sheet1")

# Esempio LASSO con cross-validation su BG_DBi per la variabile AQ_nox

# Prepara i dati
y <- BG_DBi$AQ_nox
X <- as.matrix(BG_DBi[ , !(names(BG_DBi) %in% "AQ_nox")])

# Esegui la LASSO con cross-validation
set.seed(123)
cv.lasso <- cv.glmnet(X, y, alpha = 1, standardize = TRUE)

# Lambda ottimale
print(cv.lasso$lambda.min)

# Coefficienti del modello ottimale
print(coef(cv.lasso, s = "lambda.min"))

# (Opzionale) Visualizza la curva di cross-validation
plot(cv.lasso)

# Plot ACF e PACF per AQ_nox
par(mfrow = c(1, 2)) # due grafici affiancati
acf(y, main = "ACF di AQ_nox")
pacf(y, main = "PACF di AQ_nox")
par(mfrow = c(1, 1)) # reset layout

# Test di Dickey-Fuller per la stazionarietà di AQ_nox
adf_result <- adf.test(y)
print(adf_result)

# Test di Ljung-Box per l'autocorrelazione (lag 20, ad esempio)
ljung_result <- Box.test(y, lag = 20, type = "Ljung-Box")
print(ljung_result)

# Modello SARIMA con stagionalità annuale (m = 365)
ts_aq_nox <- ts(BG_DBi$AQ_nox, frequency = 365)
model_sarima <- auto.arima(ts_aq_nox, seasonal = TRUE)
summary(model_sarima)

# Diagnostica dei residui
checkresiduals(model_sarima)

# --- Analisi su MI_DBi ---
y_mi <- MI_DBi$AQ_nox
X_mi <- as.matrix(MI_DBi[ , !(names(MI_DBi) %in% "AQ_nox")])

set.seed(123)
cv.lasso_mi <- cv.glmnet(X_mi, y_mi, alpha = 1, standardize = TRUE)
print(cv.lasso_mi$lambda.min)
print(coef(cv.lasso_mi, s = "lambda.min"))
plot(cv.lasso_mi)

par(mfrow = c(1, 2))
acf(y_mi, main = "ACF di AQ_nox (MI)")
pacf(y_mi, main = "PACF di AQ_nox (MI)")
par(mfrow = c(1, 1))

adf_result_mi <- adf.test(y_mi)
print(adf_result_mi)
ljung_result_mi <- Box.test(y_mi, lag = 20, type = "Ljung-Box")
print(ljung_result_mi)

ts_aq_nox_mi <- ts(MI_DBi$AQ_nox, frequency = 365)
model_sarima_mi <- auto.arima(ts_aq_nox_mi, seasonal = TRUE)
summary(model_sarima_mi)
checkresiduals(model_sarima_mi)

# --- Analisi su MN_DBi ---
y_mn <- MN_DBi$AQ_nox
X_mn <- as.matrix(MN_DBi[ , !(names(MN_DBi) %in% "AQ_nox")])

set.seed(123)
cv.lasso_mn <- cv.glmnet(X_mn, y_mn, alpha = 1, standardize = TRUE)
print(cv.lasso_mn$lambda.min)
print(coef(cv.lasso_mn, s = "lambda.min"))
plot(cv.lasso_mn)

par(mfrow = c(1, 2))
acf(y_mn, main = "ACF di AQ_nox (MN)")
pacf(y_mn, main = "PACF di AQ_nox (MN)")
par(mfrow = c(1, 1))

adf_result_mn <- adf.test(y_mn)
print(adf_result_mn)
ljung_result_mn <- Box.test(y_mn, lag = 20, type = "Ljung-Box")
print(ljung_result_mn)

ts_aq_nox_mn <- ts(MN_DBi$AQ_nox, frequency = 365)
model_sarima_mn <- auto.arima(ts_aq_nox_mn, seasonal = TRUE)
summary(model_sarima_mn)
checkresiduals(model_sarima_mn)










