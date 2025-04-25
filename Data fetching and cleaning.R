
library(tidyverse)
library(eurostat)
library(janitor)
library(rjstat)
# library(scales)
# library(ineapir)



# Nights spent by EU region in 2023
eu_nights_spent_raw <- get_eurostat(id = "tour_occ_nin2m", type = "code", stringsAsFactors = TRUE)
#write_csv(eu_nights_spent_raw, "Data/eu_nights_spent_raw.csv")


head(eu_nights_spent_raw)


glimpse(eu_nights_spent_raw)



eu_nights_spent_23 <- eu_nights_spent_raw %>%
  filter(nchar(as.character(geo)) == 4) %>%
  label_eurostat(fix_duplicated = TRUE) %>%
  filter(c_resid == "Total",
         month == "Total",
         unit %in% c("Number", "Per square kilometre", "Per thousand inhabitants"),
         year(TIME_PERIOD) == 2023) %>%
  select(-freq, -c_resid, -nace_r2, -month)


head(eu_nights_spent_23)

glimpse(eu_nights_spent_23)


eu_nights_spent_23 %>%
  summarise_all(~sum(is.na(.)))


summary(eu_nights_spent_23)

#write_csv(eu_nights_spent_23, "Data/eu_nights_spent_23.csv")


# total number of tourists arrived to the Canary Islands (ISTAC)

tourists_can_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/E16028B_000003/~latest.csv")
#write_csv(tourists_can_raw, "Data/tourists_can_raw.csv")

head(tourists_can_raw)


glimpse(tourists_can_raw)


tourists_can <- tourists_can_raw %>%
  clean_names() %>%
  filter(medidas_number_es == "Turistas") %>%
  separate(time_period_number_es, into = c("month", "year"), sep = "/") %>%
  mutate(across(c(month, year), ~as.numeric(.)),
         date = ym(paste(year, month, sep = "-"))) %>%
  distinct() %>%
  select(date, year, month, country = lugar_residencia_number_es, tourists = obs_value)



tourists_can %>%
  summarise_all(~sum(is.na(.)))


tourists_can %>%
  filter(is.na(tourists)) %>%
  print(n = 40)


summary(tourists_can)

#write_csv(tourists_can, "Data/tourists_can.csv")


# number of tourists arrived by island and place of residence
tour_by_island_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/E16028B_000011/~latest.csv")
#write_csv(tour_by_island_raw, "Data/tour_by_island_raw.csv")

head(tour_by_island_raw)

glimpse(tour_by_island_raw)


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


head(tour_by_island)

sample_n(tour_by_island, size = 15)

glimpse(tour_by_island)


tour_by_island %>%
  summarise_all(~sum(is.na(.)))


tour_by_island %>%
  filter(is.na(tourists)) %>%
  arrange(date)


tour_by_island %>%
  filter(is.na(tourists)) %>%
  arrange(time_period) %>%
  tail()


tour_by_island %>%
  filter(is.na(tourists)) %>%
  group_by(year) %>%
  summarise(missing_values = n())


tour_by_island %>%
  filter(is.na(tourists),
         year == 2020) %>%
  arrange(date)


tour_by_island %>%
  filter(is.na(tourists),
         country == "Total") %>%
  group_by(year) %>%
  summarise(missing_values = n())


tour_by_island %>%
  filter(is.na(tourists),
         country == "Total",
         year == 2020) %>%
  print(n = 10)



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



lg_eh %>%
  filter(lg_eh_tourists < 0)


lg_eh_neg <- lg_eh %>%
  filter(lg_eh_tourists < 0)


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

#write_csv(tour_by_island2, "Data/tour_by_island.csv")


# average stay
average_stay_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00065A_000036/1.46.csv")
#write_csv(average_stay_raw, "Data/average_stay_raw.csv")


head(average_stay_raw)

glimpse(average_stay_raw)


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

head(average_stay)

sample_n(average_stay, size = 15)

average_stay %>%
  summarise_all(~sum(is.na(.)))


average_stay %>%
  arrange(date) %>%
  filter(is.na(stays))

average_stay %>%
  arrange(date) %>%
  filter(is.na(stays),
         island == "Canarias")


average_stay %>%
  arrange(date) %>%
  filter(is.na(stays),
         country == "Total")


#write_csv(average_stay, "Data/average_stay.csv")



# daily average expenditure per tourist
daily_exp_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00028A_000011/~latest.csv")
#write_csv(daily_exp_raw, "Data/daily_expenditure_raw.csv")

head(daily_exp_raw)

glimpse(daily_exp_raw)


daily_exp <- daily_exp_raw %>%
  clean_names() %>%
  filter(medidas_number_es == "Gasto por turista y día") %>%
  separate(col = time_period_code, into = c("year", "quarter"), sep = "-") %>%
  mutate(year = as.integer(year),
         quarter = str_extract(quarter, "\\d") %>%
           as.integer()) %>%
  select(year, quarter, concept = conceptos_gastos_turisticos_number_es,
         country = pais_residencia_number_es, avg_exp = obs_value)



daily_exp %>%
  summarise_all(~sum(is.na(.)))


daily_exp %>%
  filter(is.na(avg_exp)) %>%
  print(n = 50)
  

#write_csv(daily_exp, "Data/average_expenditure_by_tour_day.csv")



# average daily rate
adr_raw <- read_csv("https://datos.canarias.es/api/estadisticas/statistical-resources/v1.0/datasets/ISTAC/C00065A_000033/~latest.csv")
#write_csv(adr_raw, "Data/adr_raw.csv")



head(adr_raw)

glimpse(adr_raw)


adr <- adr_raw %>%
  clean_names() %>%
  filter(medidas_number_es %in% c("Ingresos totales", "Tarifa media diaria")) %>%
  separate(time_period_code, into = c("year", "month"), sep = "-") %>%
  mutate(month = parse_number(month),
         year = as.integer(year)) %>%
  select(year, month, territory = territorio_number_es, variable = medidas_number_es,
         obs_value)


adr %>%
  summarise_all(~sum(is.na(.)))


adr %>%
  filter(is.na(obs_value))


#write_csv(adr, "Data/adr.csv")




































































































