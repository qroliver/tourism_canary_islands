# import libraries
library(tidyverse)
library(eurostat)
library(readxl)
library(giscoR)
library(ineapir)



# --- PART I ---

# Nights spent by EU region in 2023
eu_nights_spent_raw <- get_eurostat(id = "tour_occ_nin2m", type = "code", stringsAsFactors = TRUE)

write_csv(eu_nights_spent_raw, "data/raw/eu_nights_spent_raw.csv")



# total number of tourists arrived to the Canary Islands (ISTAC)

tourists_can_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/E16028B_000003/~latest.csv")
write_csv(tourists_can_raw, "data/raw/tourists_can_raw.csv")



# number of tourists arrived by island and place of residence
tour_by_island_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/E16028B_000011/~latest.csv")
write_csv(tour_by_island_raw, "data/raw/tour_by_island_raw.csv")



# average stay
average_stay_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00065A_000036/1.46.csv")
write_csv(average_stay_raw, "data/raw/average_stay_raw.csv")



# daily average expenditure per tourist
daily_exp_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00028A_000011/~latest.csv")
write_csv(daily_exp_raw, "data/raw/daily_expenditure_raw.csv")



# average daily rate
adr_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00065A_000033/~latest.csv")
write_csv(adr_raw, "data/raw/adr_raw.csv")



# Canary Islands geo data
# downloading NUTS boundaries for the Canary Islands at the island level
canary_islands_geo_raw <- gisco_get_nuts(resolution = "03",
                                     country = "ES",
                                     year = "2024",
                                     nuts_level = "3")

write_csv(canary_islands_geo_raw, "data/raw/can_islands_geo_raw.csv")




# --- PART II ---

# average salary in Spain
avg_salary_raw <- ineapir::get_data_table(idTable = "6061", tip = "A", unnest = TRUE)
write_csv(avg_salary_raw, "data/raw/avg_salary_raw.csv")



# unemployment
unemployment_raw <- get_data_table(
  idTable = "65334",
  tip = "A",
  unnest = TRUE
)

write_csv(unemployment_raw, "data/raw/unemployment_raw.csv")



# poverty & social exclusion rate (AROPE)
poverty_raw <- get_eurostat(
  id = "ilc_li41", type = "code"
)

write_csv(poverty_raw, "data/raw/poverty_raw.csv")



# income inequality (S80/S20)
s80s20_raw <- get_eurostat(
  id = "ilc_di11_r", type = "code"
)

write_csv(s80s20_raw, "data/raw/s80s20_raw.csv")




# Inability to afford a meal with meat, chicken, fish (or vegetarian equivalent) every second day
inab_meal_raw <- get_eurostat(
  id = "ilc_mdes03_r", type = "code"
)

write_csv(inab_meal_raw, "data/raw/inab_meal_raw.csv")



# Severe material and social deprivation
mat_soc_depr_raw <- get_eurostat(id = "ilc_mdsd18", type = "code")

write_csv(mat_soc_depr_raw, "data/raw/mat_soc_depr_raw.csv")