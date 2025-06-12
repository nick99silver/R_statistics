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
library(readr)
library(tseries)
library(moments)
library(lmtest)

#Dataset import
Agrimonia_Dataset <- read_csv("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/Agrimonia_Dataset_v_3_0_0.csv") #From .csv
Metadata_stations <- read_csv("/Users/nicolasilvestri/Desktop/Unibg/Statistics/PART 1/R scripts and data/Databases/Metadata_monitoring_network_registry_v_2_0_1.csv")

#-------------------------------------------------------------------------------

#Preliminary data handling
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

#Creating day_of_year and trend_time columns
DB$day_of_year <- yday(DB$Time)
DB$Trend <- as.numeric(DB$Time)

# transforming Stations_name into categorical data
DB$NameStation <- as.factor(DB$NameStation)


MI_DB <- DB %>% 
  filter(IDStations==504)
BG_DB <- DB %>% 
  filter(IDStations==583)
MN_DB <- DB %>% 
  filter(IDStations==697)

#-------------------------------------------------------------------------------

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

ggplot(DB, aes(x = Time, y = AQ_nox)) +
  geom_point(aes(color = Season)) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
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
       y = "NOₓ Concentration") +
  theme_minimal() +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# AQ_nox interpolation
DB$AQ_nox <- na.approx(DB$AQ_nox)

#testing stationarity
# Augmented Dickey-Fuller test for stationarity of AQ_nox
adf_result <- adf.test(DB$AQ_nox)
print(adf_result)

#-------------------------------------------------------------------------------

#NA manipulation

#select only considered columns
MI_DB <- MI_DB %>%
  select(IDStations,AQ_pm25, AQ_nox, WE_temp_2m, WE_wind_speed_10m_mean, WE_wind_speed_10m_max,
         WE_wind_speed_100m_mean, WE_wind_speed_100m_max, WE_tot_precipitation, WE_surface_pressure,
         WE_solar_radiation, WE_rh_mean, WE_blh_layer_min, WE_blh_layer_max, Month_num, Season, Day_of_week, 
         day_of_year, Trend)
BG_DB <- BG_DB %>%
  select(IDStations,AQ_pm25, AQ_nox, WE_temp_2m, WE_wind_speed_10m_mean, WE_wind_speed_10m_max,
         WE_wind_speed_100m_mean, WE_wind_speed_100m_max, WE_tot_precipitation, WE_surface_pressure,
         WE_solar_radiation, WE_rh_mean, WE_blh_layer_min, WE_blh_layer_max, Month_num, Season, Day_of_week, 
         day_of_year, Trend)
MN_DB <- MN_DB %>%
  select(IDStations,AQ_pm25, AQ_nox, WE_temp_2m, WE_wind_speed_10m_mean, WE_wind_speed_10m_max,
         WE_wind_speed_100m_mean, WE_wind_speed_100m_max, WE_tot_precipitation, WE_surface_pressure,
         WE_solar_radiation, WE_rh_mean, WE_blh_layer_min, WE_blh_layer_max, Month_num, Season, Day_of_week, 
         day_of_year, Trend)

#Applying kalman to all numeric columns in my dataset
vars_to_impute <- c(
  "AQ_pm25", "AQ_nox", "WE_temp_2m", "WE_wind_speed_10m_mean", "WE_wind_speed_10m_max",
  "WE_wind_speed_100m_mean", "WE_wind_speed_100m_max", "WE_tot_precipitation", "WE_surface_pressure",
  "WE_solar_radiation", "WE_rh_mean", "WE_blh_layer_min", "WE_blh_layer_max"
)

for (col in vars_to_impute) {
  if (col %in% names(BG_DB)) {
    BG_DB[[col]] <- na_kalman(ts(BG_DB[[col]], frequency = 365), model = "auto.arima")
  }
}

#plotting NA to be fitted
ggplot_na_imputations(ts_data, ts_filled) +
  labs(title = "NA Imputation using Kalman Filter")

#creating a new database with the fitted values
DB_fitted <- DB %>%
  mutate(AQ_nox = ts_filled)

# Plot the original and filled time series  
ggplot_na_imputations(ts_data, ts_filled)

# Plot the distribution of AQ_nox after Kalman filtering
hist(DB_fitted$AQ_nox, main = "Distribution after Kalman", col = "skyblue")

#transforming categorical variables into factors
DB_fitted$Day_of_week <- as.factor(DB_fitted$Day_of_week)
DB_fitted$Season <- as.factor(DB_fitted$Season)
DB_fitted$WE_mode_wind_direction_100m <- as.factor(DB_fitted$WE_mode_wind_direction_100m)
DB_fitted$WE_mode_wind_direction_10m <- as.factor(DB_fitted$WE_mode_wind_direction_10m)

#Dropping constant columns or all NAs and useless chr
DB_fitted <- DB_fitted[, sapply(DB_fitted, function(x) {
  !all(is.na(x)) && length(unique(na.omit(x))) > 1
})]
DB_fitted <- DB_fitted[, !(names(DB_fitted) %in% c("Province", "IDStations"))]

#-------------------------------------------------------------------------------

