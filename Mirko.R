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

plan(multisession, workers = 12 )  # Usa tutti i core

BG_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/BG_DB_impute.xlsx", sheet = "Sheet1")




#1. Rimuovi la colonna "IDstations" perchè rompe il cazzo
BG_DBi_clean <- BG_DBi %>% select(-IDStations)

###########################################################

# 2. Se volete forzare i lag sulla settimana per alcune variabili, usate sta roba
vars_to_lag <- c("WE_temp_2m", "WE_blh_layer_max")

for (v in vars_to_lag) {
  lag_name <- paste0(v, "_lag")
  BG_DBi_clean[[lag_name]] <- dplyr::lag(BG_DBi_clean[[v]], 10)
}

###########################################################


# 2. Aggiungi lag da 1 a 7 giorni per ciascuna variabile in modo da fare prendere a quella cazzo di lasso i più significativi sugli nox (vedere se eventualmente prendere altre variabili al posto di queste)

vars_to_lag <- c("AQ_pm25", "WE_temp_2m", "WE_wind_speed_10m_mean")

for (v in vars_to_lag) {
  for (l in 1:7) {
    lag_name <- paste0(v, "_lag", l)
    BG_DBi_clean[[lag_name]] <- dplyr::lag(BG_DBi_clean[[v]], l)
  }
}

# 3. Elimina gli na che si vanno a creare con i valori laggati
BG_DBi_clean <- na.omit(BG_DBi_clean)


# 4. Qua si prepara il dataset per la lasso
x <- as.matrix(BG_DBi_clean[, !(names(BG_DBi_clean) %in% c("AQ_nox"))])
y <- BG_DBi_clean$AQ_nox

# 5. LASSO con lambda.1se (ti fa avere un modello più stabile e snello, altrimenti mettere lambda.min per avere quello più conservativo)
set.seed(123)
cv.lasso <- cv.glmnet(x, y, alpha = 1, standardize = TRUE)
lasso_coef <- coef(cv.lasso, s = "lambda.1se")
selected_vars <- rownames(lasso_coef)[which(lasso_coef != 0)]
selected_vars <- setdiff(selected_vars, "(Intercept)")

#Qui vedi quelle che seleziona la lasso, molte le tira a zero e le leva dalle scatole
cat("Variabili selezionate dalla LASSO:\n")
print(selected_vars)

# 6. Qui si va a crreare la matrice finale con solo variabili selezionate
x_lasso <- as.matrix(BG_DBi_clean[, selected_vars])
x_lasso_clean <- x_lasso[, apply(x_lasso, 2, var) > 0]


# 7. Serie temporale e TBATS
y_ts <- msts(y, seasonal.periods = c(7, 365))  # Define multiple seasonal periods (weekly and yearly)

fit_tbats <- tbats(y_ts)  # Fit TBATS model

# 8. Output
summary(fit_tbats)
checkresiduals(fit_tbats)

residui <- residuals(fit_tbats)

# Qui mi tocca usare ugarch perchè almeno gli posso inserire un arma per togliere autocorrelazione nei valori. Otto lui usava garchfit facendo arma () + garch ()

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 2)),
  mean.model = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "std"  # Student-t per gestire code larghe
)

fit_garch <- ugarchfit(spec, data = residui)

# Supponendo che 'residui' sia il vettore dei residui standardizzati, estrae i residui standardizzati come vettore numerico
resid_std <- residuals(fit_garch, standardize = TRUE)

# Converte in serie temporale i residui
resid_std_ts <- ts(resid_std)

# Plotta ACF e PACF con lag sul 20 
acf(resid_std_ts, lag.max = 20, main = "ACF dei residui standardizzati GARCH")
pacf(resid_std_ts, lag.max = 20, main = "PACF dei residui standardizzati GARCH")

Box.test(residuals(fit_garch, standardize = TRUE), lag = 20, type = "Ljung-Box")


#spoiler, la varianza viene gestita bene ma con quel modello di arma mi da ancora ancora autocorrelazione

# Perform CCF analysis to identify lagged variables
ccf_analysis <- function(target, predictors, max_lag = 20) {
  for (var in predictors) {
    cat("\nCCF between", target, "and", var, ":\n")
    ccf_result <- ccf(BG_DBi[[target]], BG_DBi[[var]], lag.max = max_lag, plot = TRUE, main = paste("CCF:", target, "vs", var))
  }
}

# Define target and predictors for CCF analysis
target_var <- "AQ_nox"
predictor_vars <- c("AQ_pm25", "WE_temp_2m", "WE_wind_speed_10m_mean")

# Run CCF analysis
ccf_analysis(target_var, predictor_vars)