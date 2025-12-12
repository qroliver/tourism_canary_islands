library(tidyverse)
library(eurostat)
library(janitor)
library(readxl)
library(rjstat)
library(ineapir)



# Impact of the tourist sector on the GDP
gdp_impact_raw <- read_xlsx("Data/Datos-IMPACTUR-2023_raw.xlsx")

gdp_impact <- gdp_impact_raw[-c(1:2), ] %>%
  row_to_names(row_number = 1) %>%
  clean_names() %>%
  filter(!is.na(na)) %>%
  mutate(na = make_clean_names(na)) %>%
  pivot_longer(cols = !na,
               names_to = "region",
               values_to = "value") %>%
  filter(na %in% c("pib", "empleo")) %>%
  rename(variable = na) %>%
  mutate(value = as.numeric(value),
         percentage = 100 * value) %>%
  bind_rows(tibble(variable = c("pib", "empleo"),
                   region = "catalonia",
                   percentage = c(12, 14)))

#write_csv(gdp_impact, "Data/gdp_impact_spa.csv")


#tourism gdp in the Canary Islands
gdp_can_raw <- read_csv2("Data/GDP tourism Canaries.csv")


gdp_can <- gdp_can %>%
  clean_names() %>%
  select(year, gdp_millions, gdp_percent, empl_thousands, empl_percent)


#write_csv(gdp_can, "Data/gdp_can.csv")


# average salary in Spain
avg_salary_raw <- ineapir::get_data_table(idTable = "6061", tip = "A", unnest = TRUE)
#write_csv(avg_salary_raw, "Data/avg_salary_raw.csv")

#avg_salary_raw <- read_csv("Data/avg_salary_raw.csv")

avg_salary <- avg_salary_raw %>%
  mutate(
    Nombre = str_replace(Nombre, "\\(.*?\\)", "") %>%
      str_replace(., "S.Social", "SSocial") %>%
      str_squish(),
    quarter = as.integer(str_remove(T3_Periodo, "^T")),
    month = (quarter * 3) - 2,
    date = make_date(year = Anyo, month = month)
  ) %>%
  separate(
    col = Nombre,
    into = c("region", "sector", "component1", "component2", "currency"),
    sep = "\\. "
  ) %>%
  mutate(
    across(c(region, sector, component1), ~str_squish(.))
  ) %>%
  select(date, year = Anyo, quarter, region, sector, component = component1, euros = Valor)

write_csv(avg_salary, "Data/avg_salary.csv")



# unemployment
unemployment_raw <- get_data_table(
  idTable = "65334",
  tip = "A",
  unnest = TRUE
)

#write_csv(unemployment_raw, "Data/unemployment_raw.csv")

unemployment_raw <- read_csv("Data/unemployment_raw.csv")

unemployment <- unemployment_raw %>%
  clean_names() %>%
  filter(str_detect(nombre, "Ambos sexos")) %>%
  mutate(
    nombre = str_remove(nombre, "Tasa de paro de la población.") %>%
      str_remove(., "Ambos sexos.") %>%
      str_trim() %>%
      str_replace(., "\\.$", ""),
    date = str_extract(fecha, "^\\d{4}\\-\\d{2}\\-\\d{2}") %>%
      as.Date()
  ) %>%
  separate(nombre, into = c("region", "age"), sep = "\\.\\s+") %>%
  select(date, region, age, unemp_rate = valor)

write_csv(unemployment, "Data/unemployment.csv")


# poverty & social exclusion rate (AROPE)
poverty_raw <- get_eurostat(
  id = "ilc_li41", type = "code"
)

write_csv(poverty_raw, "Data/poverty_raw.csv")


poverty <- label_eurostat(poverty_raw, code = "geo", fix_duplicated = TRUE) %>%
  rename(
    date = TIME_PERIOD,
    poverty_rate = values
  )

write_csv(poverty, "Data/poverty.csv")



# income inequality (S80/S20)
s80s20_raw <- get_eurostat(
  id = "ilc_di11_r", type = "code"
)


write_csv(s80s20_raw, "Data/s80s20_raw.csv")


s80s20 <- label_eurostat(
  s80s20_raw, code = "geo", fix_duplicated = TRUE
) %>%
rename(
  date = TIME_PERIOD,
  s80s20 = values
)

write_csv(s80s20, "Data/s80s20.csv")



# Inability to afford a meal with meat, chicken, fish (or vegetarian equivalent) every second day

inab_meal_raw <- get_eurostat(
  id = "ilc_mdes03_r", type = "code"
)

write_csv(inab_meal_raw, "Data/inab_meal_raw.csv")


inab_meal <- label_eurostat(inab_meal_raw, code = "geo", fix_duplicated = TRUE) %>%
  rename(
    date = TIME_PERIOD,
    perc = values
  )

write_csv(inab_meal, "Data/inab_meal.csv")



# Severe material and social deprivation
mat_soc_depr_raw <- get_eurostat(id = "ilc_mdsd18", type = "code")

write_csv(mat_soc_depr_raw, "Data/mat_soc_depr_raw.csv")


mat_soc_depr <- label_eurostat(mat_soc_depr_raw, code = "geo", fix_duplicated = TRUE) %>%
  rename(
    date = TIME_PERIOD,
    perc = values
  )

write_csv(mat_soc_depr, "Data/mat_soc_depr.csv")
