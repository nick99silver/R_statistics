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
plan(multisession, workers = 12 )  # Use all the core

# Dataset Bergamo Import
BG_DBi<- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/BG_DB_impute.xlsx", sheet = "Sheet1")

# Remove the first column "IDstations" before the Lasso
BG_DBi_clean <- BG_DBi %>% select(-IDStations)

# Prepare the Dataset for the Lasso
x_BG <- as.matrix(BG_DBi_clean[, !(names(BG_DBi_clean) %in% c("AQ_nox"))])
y_BG <- BG_DBi_clean$AQ_nox

# Performed the Lasso
set.seed(123)
cv.lasso_BG <- cv.glmnet(x_BG, y_BG, alpha = 1, standardize = TRUE)
lasso_coef_BG <- coef(cv.lasso_BG, s = "lambda.1se")
selected_vars_BG <- rownames(lasso_coef_BG)[which(lasso_coef_BG != 0)]
selected_vars_BG <- setdiff(selected_vars_BG, "(Intercept)")

# Print the variables used in the Lasso
cat("Selected Variable from the Lasso:\n")
print(selected_vars_BG)

# Print lasso coefficients
print(lasso_coef_BG)

#Print selected lambda value
cat("Selected lambda value for LASSO:\n")
print(cv.lasso_BG$lambda.1se)

# Performede the RMSE on lasso
predictions_BG <- predict(cv.lasso_BG, newx = x_BG, s = "lambda.1se")
rmse_BG <- sqrt(mean((y_BG - predictions_BG)^2))
cat("RMSE della LASSO:", rmse_BG, "\n")


################### Milano Dataset #################

# Dataset Milano Import
MI_DBi <- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MI_DB_impute.xlsx", sheet = "Sheet1")

# Remove the first column "IDstations" before the Lasso
MI_DBi_clean <- MI_DBi %>% select(-IDStations)

# Prepare the Dataset for the Lasso
x_MI <- as.matrix(MI_DBi_clean[, !(names(MI_DBi_clean) %in% c("AQ_nox"))])
y_MI <- MI_DBi_clean$AQ_nox

# Performed the Lasso
set.seed(123)
cv.lasso_MI <- cv.glmnet(x_MI, y_MI, alpha = 1, standardize = TRUE)
lasso_coef_MI <- coef(cv.lasso_MI, s = "lambda.1se")
selected_vars_MI <- rownames(lasso_coef_MI)[which(lasso_coef_MI != 0)]
selected_vars_MI <- setdiff(selected_vars_MI, "(Intercept)")

# Print the variables used in the Lasso
cat("Selected Variable from the Lasso:\n")
print(selected_vars_MI)

# Print lasso coefficients
print(lasso_coef_MI)

#Print selected lambda value
cat("Selected lambda value for LASSO:\n")
print(cv.lasso_MI$lambda.1se)

# Performede the RMSE on lasso
predictions_MI <- predict(cv.lasso_MI, newx = x_MI, s = "lambda.1se")
rmse_MI <- sqrt(mean((y_MI - predictions_MI)^2))
cat("RMSE from LASSO:", rmse_MI, "\n")


################### Mantova Dataset #################

# Dataset Milano Import
MN_DBi <- read.xlsx("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/MN_DB_impute.xlsx", sheet = "Sheet1")


# Remove the first column "IDstations" before the Lasso
MN_DBi_clean <- MN_DBi %>% select(-IDStations)

# Prepare the Dataset for the Lasso
x_MN <- as.matrix(MN_DBi_clean[, !(names(MN_DBi_clean) %in% c("AQ_nox"))])
y_MN <- MN_DBi_clean$AQ_nox

# Performed the Lasso
set.seed(123)
cv.lasso_MN <- cv.glmnet(x_MN, y_MN, alpha = 1, standardize = TRUE)
lasso_coef_MN <- coef(cv.lasso_MN, s = "lambda.1se")
selected_vars_MN <- rownames(lasso_coef_MN)[which(lasso_coef_MN != 0)]
selected_vars_MN <- setdiff(selected_vars_MN, "(Intercept)")

# Print the variables used in the Lasso
cat("Selected lambda value from LASSO:\n")
print(selected_vars_MN)

#print lasso coefficients
print(lasso_coef_MN)

#Print selected lambda value
cat("Selected lambda value for LASSO:\n")
print(cv.lasso_MN$lambda.1se)

# Performede the RMSE on lasso
predictions_MN <- predict(cv.lasso_MN, newx = x_MN, s = "lambda.1se")
rmse_MN <- sqrt(mean((y_MN - predictions_MN)^2))
cat("RMSE for the LASSO:", rmse_MN, "\n")



