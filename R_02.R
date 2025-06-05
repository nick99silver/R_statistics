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




