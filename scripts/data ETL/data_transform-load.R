# import libraries
library(tidyverse)
library(janitor)
library(eurostat)
library(readxl)



# --- PART I ---

# Nights spent by EU region in 2023
eu_nights_spent_raw <- read_csv("data/raw/eu_nights_spent_raw.csv")

eu_nights_spent_23 <- eu_nights_spent_raw %>%
  filter(nchar(as.character(geo)) == 4) %>%
  label_eurostat(fix_duplicated = TRUE) %>%
  filter(c_resid == "Total",
         month == "Total",
         unit %in% c("Number", "Per square kilometre", "Per thousand inhabitants"),
         year(TIME_PERIOD) == 2023) %>%
  select(-freq, -c_resid, -nace_r2, -month)

write_csv(eu_nights_spent_23, "data/clean/eu_nights_spent.csv")



# total number of tourists arrived to the Canary Islands (ISTAC)
tourists_can_raw <- read_csv("data/raw/tourists_can_raw.csv")

tourists_can <- tourists_can_raw %>%
  clean_names() %>%
  filter(medidas_number_es == "Turistas") %>%
  separate(time_period_number_es, into = c("month", "year"), sep = "/") %>%
  mutate(across(c(month, year), ~as.numeric(.)),
         date = ym(paste(year, month, sep = "-"))) %>%
  distinct() %>%
  select(date, year, month, country = lugar_residencia_number_es, tourists = obs_value)

write_csv(tourists_can, "data/clean/tourists_can.csv")



# number of tourists arrived by island and place of residence
tour_by_island_raw <- read_csv("data/raw/tour_by_island_raw.csv")

tour_by_island <- tour_by_island_raw %>%
  clean_names() %>%
  separate(time_period_number_es, into = c("month", "year"), sep = "/") %>%
  filter(tipo_viajero_number_es == "Turistas principales",
         medidas_number_es == "Turistas") %>%
  mutate(across(c(month, year), ~as.numeric(.)),
         date = lubridate::ym(paste(year, month, sep = "-"))) %>%
  select(date, year, month, country = lugar_residencia_number_es,
         island = territorio_number_es, tourists = obs_value) %>%
  distinct()


# calculating number of tourists visiting La Gomera & El Hierro (together)
unique(tourists_can$country)
unique(tour_by_island$country)
setdiff(unique(tourists_can$country), unique(tour_by_island$country))


lg_eh <- tourists_can %>%
  mutate(country_adj = case_when(country %in% c("Dinamarca", "Finlandia", "Noruega", "Suecia") ~ "Países Nórdicos",
                                 country == "Suiza" ~ "Total",
                                 .default = country)) %>%
  group_by(date, year, month, country_adj) %>%
  summarise(tourists_can = sum(tourists, na.rm = TRUE), .groups = "drop") %>%
  left_join(tour_by_island %>%
              group_by(date, year, month, country) %>%
              summarise(tourists5 = sum(tourists, na.rm = TRUE), .groups = "drop"),
            by = c("date", "year", "month", "country_adj" = "country")) %>%
  mutate(lg_eh_tourists = tourists_can - tourists5) %>%
  select(-tourists_can, -tourists5) %>%
  rename(country = country_adj)


lg_eh_adj <- lg_eh %>%
  mutate(lg_eh_tourists = if_else(lg_eh_tourists < 0 &
                                    country != "Otros países o territorios del mundo (excluida España)",
                                  0, lg_eh_tourists)) %>%
  filter(lg_eh_tourists >= 0)


lg_eh_other <- lg_eh_adj %>%
  group_by(date, year, month, group = if_else(country == "Total", "Total", "countries")) %>%
  summarise(lg_eh_tourists = sum(lg_eh_tourists, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = lg_eh_tourists) %>%
  mutate(country = "Otros países o territorios del mundo (excluida España)",
         island = "La Gomera & El Hierro",
         tourists = Total - countries) %>%
  select(date, year, month, country, island, tourists)


tour_by_island2 <- tour_by_island %>%
  bind_rows(lg_eh_adj %>%
              rename(tourists = lg_eh_tourists) %>%
              mutate(island = "La Gomera & El Hierro"),
            lg_eh_other)

write_csv(tour_by_island2, "data/clean/tour_by_island.csv")



# average stay
average_stay_raw <- read_csv("data/raw/average_stay_raw.csv")

average_stay <- average_stay_raw %>%
  clean_names() %>%
  separate(col = time_period_number_es, into = c("month", "year"), sep = "/") %>%
  filter(medidas_number_es == "Estancia media",
         territorio_number_es %in% c("Canarias", "Lanzarote", "Fuerteventura", "Gran Canaria",
                                     "Tenerife", "La Gomera", "La Palma", "El Hierro"),
         !is.na(month),
         !is.na(year)) %>%
  mutate(across(c(year, month), ~as.integer(.)),
         date = ym(paste(year, month, sep = "-"))) %>%
  select(date, year, month, island = territorio_number_es, country = nacionalidad_number_es,
         stays = obs_value)

write_csv(average_stay, "data/clean/average_stay.csv")



# daily average expenditure per tourist
daily_exp_raw <- read_csv("data/raw/daily_expenditure_raw.csv")

daily_exp <- daily_exp_raw %>%
  clean_names() %>%
  filter(medidas_number_es == "Gasto por turista y día") %>%
  separate(col = time_period_code, into = c("year", "quarter"), sep = "-") %>%
  mutate(year = as.integer(year),
         quarter = str_extract(quarter, "\\d") %>%
           as.integer()) %>%
  select(year, quarter, concept = conceptos_gastos_turisticos_number_es,
         country = pais_residencia_number_es, avg_exp = obs_value)

write_csv(daily_exp, "data/clean/average_expenditure_by_tour_day.csv")



# average daily rate
adr_raw <- read_csv("data/raw/adr_raw.csv")

adr <- adr_raw %>%
  clean_names() %>%
  filter(medidas_number_es %in% c("Ingresos totales", "Tarifa media diaria")) %>%
  separate(time_period_code, into = c("year", "month"), sep = "-") %>%
  mutate(
    month = parse_number(month),
    year = as.integer(year),
    date = ymd(paste(year, month, "01", sep = "-"))
  ) %>%
  select(
    date, year, month, territory = territorio_number_es, variable = medidas_number_es, obs_value
  )

write_csv(adr, "data/clean/adr.csv")



# Canary Islands geo data
canary_islands_geo_raw <- read_csv("data/raw/can_islands_geo_raw.csv")

can_geo_map <- canary_islands_geo_raw %>%
  filter(NUTS_NAME %in% c("Lanzarote", "Fuerteventura", "Gran Canaria", "Tenerife",
                          "La Gomera", "La Palma", "El Hierro")) %>%
  filter(NUTS_NAME %in% c("La Gomera", "El Hierro")) %>%
  reframe(NAME_LATN = "La Gomera & El Hierro",
          CNTR_CODE = "ES",
          NUTS_NAME = "La Gomera & El Hierro",
          geometry = st_union(geometry)) %>%
  bind_rows(canary_islands_map %>%
              filter(!NUTS_NAME %in% c("La Gomera", "El Hierro"))) %>%
  select(island = NAME_LATN, geometry)

write_csv(can_geo_map, "data/clean/can_geo_map.csv")



# --- PART II ---

# Impact of the tourist sector on the GDP
gdp_impact_raw <- read_xlsx("data/raw/Datos-IMPACTUR-2023_raw.xlsx")

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

write_csv(gdp_impact, "data/clean/gdp_impact_spa.csv")



#tourism gdp in the Canary Islands
gdp_can_raw <- read_csv2("data/raw/GDP tourism Canaries.csv")

gdp_can <- gdp_can %>%
  clean_names() %>%
  select(year, gdp_millions, gdp_percent, empl_thousands, empl_percent)


write_csv(gdp_can, "data/clean/gdp_can.csv")



# average salary in Spain
avg_salary_raw <- read_csv("data/raw/avg_salary_raw.csv")

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

write_csv(avg_salary, "data/clean/avg_salary.csv")



# unemployment
unemployment_raw <- read_csv("data/raw/unemployment_raw.csv")

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

write_csv(unemployment, "data/clean/unemployment.csv")



# poverty & social exclusion rate (AROPE)
poverty_raw <- read_csv("data/raw/poverty_raw.csv")

poverty <- label_eurostat(poverty_raw, code = "geo", fix_duplicated = TRUE) %>%
  rename(
    date = TIME_PERIOD,
    poverty_rate = values
  )

write_csv(poverty, "data/clean/poverty.csv")



# income inequality (S80/S20)
s80s20_raw <- read_csv("data/raw/s80s20_raw.csv")

s80s20 <- label_eurostat(
  s80s20_raw, code = "geo", fix_duplicated = TRUE
) %>%
rename(
  date = TIME_PERIOD,
  s80s20 = values
)

write_csv(s80s20, "data/clean/s80s20.csv")



# Inability to afford a meal with meat, chicken, fish (or vegetarian equivalent) every second day
inab_meal_raw <- read_csv("data/raw/inab_meal_raw.csv")

inab_meal <- label_eurostat(inab_meal_raw, code = "geo", fix_duplicated = TRUE) %>%
  rename(
    date = TIME_PERIOD,
    perc = values
  )

write_csv(inab_meal, "data/clean/inab_meal.csv")



# Severe material and social deprivation
mat_soc_depr_raw <- read_csv("data/raw/mat_soc_depr_raw.csv")

mat_soc_depr <- label_eurostat(mat_soc_depr_raw, code = "geo", fix_duplicated = TRUE) %>%
  rename(
    date = TIME_PERIOD,
    perc = values
  )

write_csv(mat_soc_depr, "data/clean/mat_soc_depr.csv")