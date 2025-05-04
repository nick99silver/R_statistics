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
              output_file = "EDA_Prov_Report.html")

ggplot(DB, aes(x = Time, y = AQ_nox)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Province) +
  labs(title = "AQ_nox over time by Province") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Create a scatter plot with a linear regression line
ggplot(DB, aes(x = Time, y = AQ_nox)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
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
  labs(title = "AQ_nox by Day of the Week (Mean Highlighted)",
       y = "NOₓ Concentration (µg/m³)") +
  theme_minimal() +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))



