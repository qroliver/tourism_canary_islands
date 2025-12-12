library(tidyverse)
library(janitor)
library(ggtext)


gdp_impact <- read_csv("Data/gdp_impact_spa.csv")
gdp_can <- read_csv("Data/gdp_can.csv")
avg_salary <- read_csv("Data/avg_salary.csv")
unemployment <- read_csv("Data/unemployment.csv")
poverty <- read_csv("Data/poverty.csv")
s80s20 <- read_csv("Data/s80s20.csv")
inab_meal <- read_csv("Data/inab_meal.csv")
mat_soc_depr <- read_csv("Data/mat_soc_depr.csv")


# mapping vector for region names
region_names_mapping <- c("illes_balears" = "Balearic Islands",
                          "canarias" = "<span style='color:blue;'>Canary Islands</span>",
                          "comunitat_valenciana" = "Valencian Community",
                          "andalucia" = "Andalusia",
                          "espana" = "Spain",
                          "cantabria" = "Cantabria",
                          "galicia" = "Galicia",
                          "catalonia" = "Catalonia",
                          "la_rioja" = "La Rioja",
                          "region_de_murcia" = "Region of Murcia",
                          "comunidad_de_madrid" = "Community of Madrid",
                          "castilla_la_mancha" = "Castilla—La Mancha")

gdp_can %>%
  ggplot(aes(x = year, y = gdp_percent)) +
  geom_rect(aes(xmin = 2010, xmax = 2013, ymin = -Inf, ymax = Inf), fill = "#dee5ec", alpha = 0.05) +
  geom_rect(aes(xmin = 2010, xmax = 2011, ymin = -Inf, ymax = Inf), fill = "#bfd7d7", alpha = 0.05) +
  geom_rect(aes(xmin = 2020, xmax = 2023, ymin = -Inf, ymax = Inf), fill = "#c4d3df", alpha = 0.025) +
  annotate(geom = "segment", x = 2010, xend = 2013, y = 15, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  annotate(geom = "text", x = 2011.5, y = 15.8, colour = "black", size = 3.5, hjust = 0.5,
           label = "Global financial crisis") +
  annotate(geom = "segment", x = 2010, xend = 2011, y = 44.8, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2010.5, y = 43.25, colour = "black", size = 3.5, hjust = 0.5,
           label = "Arab\nSpring") +
  annotate(geom = "segment", x = 2020, xend = 2023, y = 15, colour = "grey45", 
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2021.5, y = 15.8, colour = "black", size = 3.5, hjust = 0.5,
           label = "COVID-19") +
  geom_line(colour = "blue3", linewidth = 1.2) +
  geom_point(shape = 21, colour = "white", fill = "blue3", size = 4) +
  annotate(
    geom = "text", x = 2024, y = 38, colour = "blue3", fontface = "bold", size = 4,
    label = paste0(
      gdp_can %>% 
        filter(year == 2024) %>% 
        pull(gdp_percent),
      "%"
    ),
    hjust = 0.5
  ) +
  scale_x_continuous(breaks = 2010:2024,
                     limits = c(2010, 2024),
                     expand = c(0.02, 0.02)) +
  scale_y_continuous(breaks = seq(15, 45, 5),
                     limits = c(15, 45),
                     expand = c(0.01, 0.01)) +
  labs(title = "The economy of the Canary Islands has become increasingly reliant on tourism",
       subtitle = "In 2024, tourism accounted for 36.8% of the regional GDP — up from 25% in 2010.",
       x = NULL,
       y = "% GDP",
       caption = "The graph shows the direct contribution of tourism to regional GDP.\nData: IMPACTUR | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Roboto") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey95"),
        plot.title = element_textbox_simple(colour = "#1c3d5a", family = "Poppins", 
                                            face = "bold", size = 24,
                                            margin = ggplot2::margin(b = 5)),
        plot.subtitle = element_textbox_simple(colour = "#51606f", size = 18, family = "Poppins",
                                     margin = ggplot2::margin(t = 5, b = 20)),
        plot.caption = element_text(
          family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5, margin = ggplot2::margin(t = 10)
        ),
        axis.text = element_text(colour = "#999999", face = "bold", size = 12),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        axis.ticks = element_line(colour = "grey95"),
        axis.title.y = element_text(size = 14, family = "Poppins", colour = "#666666",
                                    face = "bold", margin = ggplot2::margin(r = 10)),
        plot.margin = ggplot2::margin(10, 10, 10, 10))



# impact of tourism on employment over time — Canary Islands
gdp_can %>%
  filter(!is.na(empl_percent)) %>%
  ggplot(aes(x = year, y = empl_percent)) +
  geom_rect(aes(xmin = 2010, xmax = 2013, ymin = -Inf, ymax = Inf), fill = "#dee5ec", alpha = 0.05) +
  geom_rect(aes(xmin = 2010, xmax = 2011, ymin = -Inf, ymax = Inf), fill = "#bfd7d7", alpha = 0.05) +
  geom_rect(aes(xmin = 2020, xmax = 2023, ymin = -Inf, ymax = Inf), fill = "#c4d3df", alpha = 0.025) +
  annotate(geom = "segment", x = 2010, xend = 2013, y = 15, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  annotate(geom = "text", x = 2011.5, y = 15.8, colour = "black", size = 3.5, hjust = 0.5,
           label = "Global financial crisis") +
  annotate(geom = "segment", x = 2010, xend = 2011, y = 49.8, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2010.5, y = 48.25, colour = "black", size = 3.5, hjust = 0.5,
           label = "Arab\nSpring") +
  annotate(geom = "segment", x = 2020, xend = 2023, y = 15, colour = "grey45", 
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2021.5, y = 15.8, colour = "black", size = 3.5, hjust = 0.5,
           label = "COVID-19") +
  geom_line(colour = "blue3", linewidth = 1.2) +
  geom_point(shape = 21, colour = "white", fill = "blue3", size = 4) +
  annotate(
    geom = "text", x = 2023, y = 41, colour = "blue3", fontface = "bold", size = 4,
    label = paste0(
      round(
        gdp_can %>% 
          filter(year == 2023) %>% 
          pull(empl_percent),
        2
      ),
      "%"
    ),
    hjust = 0.75
  ) +
  scale_x_continuous(breaks = 2010:2023,
                     limits = c(2010, 2023),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(15, 50, 5),
                     limits = c(15, 50),
                     expand = c(0.01, 0.01)) +
  labs(title = "The share of jobs generated by the tourism sector has steadily increased over time",
       subtitle = "In 2023, nearly 40% of jobs in the Canary Islands were directly generated by the tourism sector — up from around 30% in 2010.",
       x = NULL,
       y = "% GDP",
       caption = "The graph shows the direct contribution of tourism to total employment.\nData: IMPACTUR | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Roboto") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey95"),
        plot.title = element_textbox_simple(colour = "#1c3d5a", family = "Poppins", 
                                            face = "bold", size = 24,
                                            margin = ggplot2::margin(b = 5)),
        plot.subtitle = element_textbox_simple(colour = "#51606f", size = 18, family = "Poppins",
                                               margin = ggplot2::margin(t = 5, b = 20)),
        plot.caption = element_text(
          family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5, margin = ggplot2::margin(t = 10)
        ),
        axis.text = element_text(colour = "#999999", face = "bold", size = 12),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        axis.ticks = element_line(colour = "grey95"),
        axis.title.y = element_text(size = 14, family = "Poppins", colour = "#666666",
                                    face = "bold", margin = ggplot2::margin(r = 10)),
        plot.margin = ggplot2::margin(10, 10, 10, 10))



# region mapping vector
region_names <- c(
  "Balears, Illes" = "Balearic Islands",
  "Canarias" = "<span style='color:blue;'>Canary Islands</span>",
  "País Vasco" = "Basque Country",
  "Andalucía" = "Andalusia",
  "Total Nacional" = "Spain",
  "Cantabria" = "Cantabria",
  "Galicia" = "Galicia",
  "Cataluña" = "Catalonia",
  "Rioja, La" = "La Rioja",
  "Murcia, Región de" = "Region of Murcia",
  "Madrid, Comunidad de" = "Community of Madrid",
  "Castilla - La Mancha" = "Castilla—La Mancha",
  "Navarra, Comunidad Foral de" = "Navarre",
  "Castilla y León" = "Castile and León", 
  "Aragón" = "Aragón",
  "Extremadura" = "Extremadura",
  "Asturias, Principado de" = "Asturias",
  "Comunitat Valenciana" = "Valencian Community"
)

# average salary per region
avg_salary %>%
  filter(
    sector == "Industria, construcción y servicios",
    component == "Coste salarial total",
    date == max(date)
  ) %>%
  select(region, euros) %>%
  ggplot(aes(x = euros, y = fct_reorder(region, euros))) +
  geom_col(
    aes(fill = euros),
    colour = "white"
  ) +
  geom_col(
    data = ~.x %>%
      filter(region == "Canarias"),
    fill = "#3e5c76",
    colour = "white"
  ) +
  scale_fill_gradient(low = "#c0d6d3", high = "#4a6f6a") +
  geom_text(
    aes(label = paste0("€", euros)),
    colour = "white",
    fontface = "bold",
    hjust = 1.2
  ) +
  scale_x_continuous(breaks = seq(0, 3000, 500),
                     limits = c(0, 3000),
                     expand = c(0.01, 0.01)) +
  scale_y_discrete(
    labels = region_names,
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "On average, the salary in the Canary Islands is the lowest in Spain",
    subtitle = "Workers in the Canary Islands earned around €1000 less a month than workers in 
    Madrid — the region with the highest average salary — in the second quarter of 2025.",
    x = "Monthly average salary (euros)",
    y = NULL,
    caption = glue::glue(
      "The graph shows average salary for Q{quarter(max(avg_salary$date))} {year(max(avg_salary$date))}.\nData: Instituto Nacional de Estadística (INE) | Made by Oliver Q.R."
    )
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey95"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(colour = "#1c3d5a", family = "Poppins", face = "bold", size = 24,
                                        margin = ggplot2::margin(b = 5)),
    plot.subtitle = element_textbox_simple(colour = "#51606f", family = "Poppins", size = 18,
                                           margin = ggplot2::margin(t = 5, b = 15)),
    plot.caption = element_text(family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5,
                                margin = ggplot2::margin(t = 10)),
    axis.text = element_text(colour = "#999999", face = "bold", size = 11),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.text.y.left = element_markdown(colour = "#4e5a66", size = 16, margin = ggplot2::margin(r = 5)),
    axis.title.x = element_text(colour = "#666666", family = "Poppins", face = "bold", size = 13,
                                margin = ggplot2::margin(t = 10)),
    axis.ticks.x = element_line(colour = "grey95"),
    legend.position = "none",
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )



# average salary over time
avg_salary %>%
  filter(
    sector == "Industria, construcción y servicios",
    component == "Coste salarial total"
  ) %>%
  select(date, region, euros, year) %>%
  mutate(
    region = if_else(region == "Canarias", "Canary Islands", recode(region, !!!region_names))
  ) %>%
  ggplot(
    aes(x = date, y = euros, group = region)
  ) +
  geom_line(
    data = ~.x %>%
      filter(!region %in% c("Canary Islands", "Spain", "Balears, Illes")),
    colour = "#d9d9d9",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_line(
    data = ~.x %>%
      filter(region == "Spain"),
    colour = "#4e4e4e",
    linewidth = 0.8
  ) +
  geom_line(
    data = ~.x %>%
      filter(region == "Balearic Islands"),
    colour = "#6aa6a6",
    linewidth = 0.8
  ) +
  geom_line(
    data = ~.x %>%
      filter(region == "Canary Islands"),
    colour = "blue3",
    linewidth = 2
  ) +
  scale_x_date(
    date_breaks = "6 months",
    labels = function(x) glue::glue("Q{lubridate::quarter(x)}—{lubridate::year(x)}"),
    limits = c(min(avg_salary$date), max(avg_salary$date)),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(750, 3500, 250),
    limits = c(750, 3500),
    expand = c(0.01, 0.01)
  ) +
  annotate(
    geom = "textbox", x = as.Date("2017-09-01"), y = 1050, colour = "black", size = 3.5, halign = 0.5, 
    hjust = 0.75, fill = "white", box.colour = "grey90", width = unit(0.225, "npc"),
    label = "COVID-19 had a big impact on salaries in both the <span style='color:blue3;'>Canary Islands</span> and the <span style='color:#6aa6a6;'>Balearic Islands</span>."
  ) +
  annotate(
    geom = "curve", x = as.Date("2018-10-01"), xend = as.Date("2020-01-01"), y = 1050, yend = 1375, 
    colour = "grey45", curvature = 0.1,
    arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "open")
  ) +
  labs(
    title = "Over the past years, the average salary in the Canary Islands has consistently been among the lowest in Spain",
    subtitle = "The average salary in the <span style='color:blue3;'>**Canary Islands**</span> has been 
      significantly below both the <span style='color:#4e4e4e;'>**Spain's**</span> national average and that of the 
      <span style='color:#6aa6a6;'>**Balearic Islands**</span>. It was also one of the regions most 
      affected by the COVID-19 pandemic.",
    x = NULL,
    y = "Average monthly salary (euros)",
    caption = "The graph shows the average salary quarterly for each region.\nData: Instituto Nacional de Estadística (INE) | Made by Oliver Q.R."
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey95"),
    plot.title = element_textbox_simple(colour = "#1c3d5a", family = "Poppins", 
                                        face = "bold", size = 24,
                                        margin = ggplot2::margin(b = 5)),
    plot.subtitle = element_textbox_simple(colour = "#51606f", size = 18, family = "Poppins",
                                            margin = ggplot2::margin(t = 5, b = 20)),
    plot.caption = element_text(
      family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5, margin = ggplot2::margin(t = 10)
    ),
    axis.text = element_text(colour = "#999999", face = "bold", size = 12),
    axis.text.x.bottom = element_text(margin = ggplot2::margin(t = 5), size = 10, angle = 45, hjust = 1, vjust = 1),
    axis.text.y.left = element_text(margin = ggplot2::margin(r = 5)),
    axis.ticks = element_line(colour = "grey95"),
    axis.title.y = element_text(size = 14, family = "Poppins", colour = "#666666",
                                face = "bold", margin = ggplot2::margin(r = 10)),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )



# unemployment
unemployment %>%
  filter(
    date == max(date),
    age == "Todas las edades"
  ) %>%
  mutate(
    region_en = recode(region, !!!region_names)
  ) %>%
  ggplot(
    aes(x = unemp_rate, y = fct_reorder(region_en, unemp_rate))
  ) +
  geom_col(
    aes(fill = unemp_rate),
    colour = "white"
  ) +
  scale_fill_gradient(low = "#fdae6b", high = "#7f2704") +
  geom_text(
    aes(label = paste0(round(unemp_rate, 2), "%")),
    colour = "white",
    fontface = "bold",
    hjust = 1.2
  ) +
  scale_x_continuous(
    breaks = seq(0, 35, 5),
    limits = c(0, 35),
    expand = c(0.01, 0.01)
  ) +
  scale_y_discrete(
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "The Canary Islands are among the regions with the highest unemployment",
    subtitle = glue::glue(
      "Only the autonomous cities of Ceuta and Melilla, and Andalusia have a higher unemployment 
      rate than the Canary Islands, where there are roughly 2.5 times more people unemployed than in the 
      Balearic Islands — the region with the lowest unemployment in Q{quarter(max(unemployment$date))} 
      {year(max(unemployment$date))}."
    ),
    x = "Unemployment rate (%)",
    y = NULL,
    caption = glue::glue(
      "The graph shows the unemployment rate for Q{quarter(max(unemployment$date))} {year(max(unemployment$date))}\nData: Instituto Nacional de Estadística (INE) | Made by Oliver Q.R."
    )
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey97"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      colour = "#1c3d5a", face = "bold", size = 24, margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = "#51606f", size = 18, margin = ggplot2::margin(t = 5, b = 15)
    ),
    plot.caption = element_text(
      family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5,
      margin = ggplot2::margin(t = 10)
    ),
    axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.text.y.left = element_markdown(colour = "#4e5a66", size = 16, margin = ggplot2::margin(r = 5)),
    axis.ticks.y = element_line(colour = "grey97"),
    legend.position = "none",
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )



# unemployment over time
unemployment %>%
  filter(
    age == "Todas las edades"
  ) %>%
  mutate(
    region = if_else(region == "Canarias", "Canary Islands", recode(region, !!!region_names))
  ) %>%
  ggplot(
    aes(x = date, y = unemp_rate, group = region)
  ) +
  geom_line(
    data = ~.x %>%
      filter(!region %in% c("Canary Islands", "Spain", "Balears, Illes")),
    colour = "#d9d9d9",
    linewidth = 0.5,
    alpha = 0.5
  ) +
  geom_line(
    data = ~.x %>%
      filter(region == "Spain"),
    colour = "#4e4e4e",
    linewidth = 0.8
  ) +
  geom_line(
    data = ~.x %>%
      filter(region == "Balearic Islands"),
    colour = "#6aa6a6",
    linewidth = 0.8
  ) +
  geom_line(
    data = ~.x %>%
      filter(region == "Canary Islands"),
    colour = "blue3",
    linewidth = 2
  ) +
  annotate(
    geom = "curve", x = as.Date("2006-07-01"), xend = as.Date("2008-04-01"), y = 31, yend = 19, colour = "grey45",
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"), curvature = 0.1
  ) +
  annotate(
    geom = "textbox", x = as.Date("2006-04-01"), y = 32.5, colour = "black", size = 3.5, halign = 0.5,
    hjust = 0.75, fill = "white", box.colour = "grey90",
    label = "Financial crisis 2007-8"
  ) +
  annotate(
    geom = "curve", x = as.Date("2018-12-01"), xend = as.Date("2020-03-01"), y = 35, yend = 24, colour = "grey45",
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"), curvature = -0.1
  ) +
  annotate(
    geom = "textbox", x = as.Date("2018-12-01"), y = 36.5, colour = "black", size = 3.5, halign = 0.5,
    hjust = 0.5, fill = "white", box.colour = "grey90", 
    label = "COVID-19 pandemic"
  ) +
  scale_x_date(
    date_breaks = "6 months",
    labels = function(x) glue::glue("Q{lubridate::quarter(x)}—{lubridate::year(x)}"),
    limits = c(min(unemployment$date), max(unemployment$date)),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    breaks = seq(0, 40, 5),
    limits = c(0, 40),
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "The Canary Islands are consistently one of the regions with the highest unemployment in Spain",
    subtitle = "The unemployment rate in the <span style='color:blue3;'>**Canary Islands**</span> is usually 
    higher than both the <span style='color:#4e4e4e;'>**Spain's**</span> national average and the rate in the 
    <span style='color:#6aa6a6;'>**Balearic Islands**</span>. In addition, it is particularly sensitive to 
    global disruptive shocks such as the 2007-8 financial crisis and the COVID-19 pandemic.",
    x = NULL,
    y = "Unemployment rate (%)",
    caption = "The graph shows the quarterly unemployment rate.\nData: Instituto Nacional de Estadística (INE) | Made by Oliver Q.R."
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey95"),
    plot.title = element_textbox_simple(
      colour = "#1c3d5a", family = "Poppins", face = "bold", size = 24, margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = "#51606f", size = 18, family = "Poppins", margin = ggplot2::margin(t = 5, b = 20)
    ),
    plot.caption = element_text(
      family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5, margin = ggplot2::margin(t = 10)
    ),
    axis.text = element_text(colour = "#999999", face = "bold", size = 12),
    axis.text.x = element_text(
      margin = ggplot2::margin(t = 5), size = 9, angle = 45, hjust = 1, vjust = 0.9
    ),
    axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
    axis.ticks = element_line(colour = "grey95"),
    axis.title.y = element_text(
      size = 14, family = "Poppins", colour = "#666666", face = "bold", margin = ggplot2::margin(r = 10)
    ),
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )



# What's the living standard in the Canary Islands?

poverty_es <- poverty %>%
  filter(
    str_starts(geo_code, "ES(?=\\d{2})")
  ) %>%
  select(date, geo_code, geo, poverty_rate) %>%
  bind_rows(
    poverty %>%
      filter(geo == "Spain") %>%
      select(date, geo_code, geo, poverty_rate)
  )


inab_meal_es <- inab_meal %>%
  filter(
    str_starts(geo_code, "ES(?=\\d{2})")
  ) %>%
  select(date, geo_code, geo, inab_meal = perc) %>%
  bind_rows(
    inab_meal %>%
      filter(geo == "Spain") %>%
      select(date, geo_code, geo, inab_meal = perc)
  )


mat_soc_depr_es <- mat_soc_depr %>%
  filter(
    str_starts(geo_code, "ES(?=\\d{2})")
  ) %>%
  select(date, geo_code, geo, mat_soc_depr = perc) %>%
  bind_rows(
    mat_soc_depr %>%
      filter(geo == "Spain") %>%
      select(date, geo_code, geo, mat_soc_depr = perc)
  )




living_std <- left_join(
    x = poverty_es,
    y = inab_meal_es,
    by = c("date", "geo_code", "geo")
  ) %>%
  left_join(
    y = mat_soc_depr_es,
    by = c("date", "geo_code", "geo")
  ) %>%
  mutate(
    region_label = case_when(
      str_detect(geo, "Canarias") ~ "CI",
      geo == "Illes Balears" ~ "BI",
      geo == "Spain" ~ "ES",
      .default = NA
    )
  )



living_std %>%
  filter(date == max(date)) %>%
  pivot_longer(
    cols = c(poverty_rate, inab_meal, mat_soc_depr),
    names_to = "variable",
    values_to = "perc"
  ) %>% mutate(
    variable = factor(variable),
    y_num = as.numeric(variable)
  ) %>%
  ggplot(
    aes(x = perc, y = variable)
  ) +
  geom_segment(
    data = ~.x %>%
      group_by(variable) %>%
      summarise(
        var_min = min(perc),
        var_max = max(perc),
        .groups = "drop"
      ),
    aes(x = var_min, xend = var_max, y = variable),
    colour = "#1c3d5a",
    lineend = "round",
    linewidth = 1
  ) +
  geom_point(
    data = ~.x %>%
      filter(!region_label %in% c("CI", "BI", "ES")),
    shape = 21,
    size = 5,
    fill = "#c7d3d9",
    colour = "white"
  ) +
  geom_segment(
    data = ~.x %>%
      filter(region_label == "ES"),
    aes(x = perc, xend = perc, y = y_num, yend = y_num + 0.2),
    colour = "grey65"
  ) +
  geom_label(
    data = ~.x %>%
      filter(region_label == "ES"),
    aes(label = glue::glue("ES: {perc}%")),
    text.colour = "#4e4e4e",
    border.colour = "grey80",
    nudge_x = 0,
    nudge_y = 0.2
  ) +
  geom_point(
    data = ~.x %>%
      filter(region_label == "ES"),
    shape = 23,
    size = 6,
    fill = "#4e4e4e",
    colour = "white"
  ) +
  geom_segment(
    data = ~.x %>%
      filter(region_label == "BI"),
    aes(x = perc, xend = perc + 0.5, y = y_num, yend = y_num - 0.2),
    colour = "grey65"
  ) +
  geom_label(
    data = ~.x %>%
      filter(region_label == "BI"),
    aes(label = glue::glue("BI: {perc}%")),
    text.colour = "#6aa6a6",
    border.colour = "grey80",
    nudge_x = 1,
    nudge_y = -0.2
  ) +
  geom_point(
    data = ~.x %>%
      filter(region_label == "BI"),
    shape = 22,
    size = 6,
    fill = "#6aa6a6",
    colour = "white"
  ) +
  geom_segment(
    data = ~.x %>%
      filter(region_label == "CI"),
    aes(x = perc, xend = perc + 1.5, y = y_num, yend = y_num - 0.25),
    colour = "grey65"
  ) +
  geom_label(
    data = ~.x %>%
      filter(region_label == "CI"),
    aes(label = glue::glue("CI: {perc}%"), fontface = "bold"),
    size = 4.5,
    text.colour = "blue3",
    border.colour = "grey80",
    nudge_x = 3,
    nudge_y = -0.25
  ) +
  geom_point(
    data = ~.x %>%
      filter(region_label == "CI"),
    shape = 21,
    size = 7,
    fill = "blue3",
    colour = "white"
  ) +
  scale_x_continuous(
    breaks = seq(0, 45, 5),
    limits = c(0, 45),
    expand = c(0.01, 0.01)
  ) +
  scale_y_discrete(
    labels = c(
      "poverty_rate" = "At risk of poverty",
      "mat_soc_depr" = "Severe social and<br>material deprivation",
      "inab_meal" = "Can't afford a protein-rich<br>meal every second day"
    )
  ) +
  labs(
    title = "Poverty and deprivation: the Canary Islands rank among Spain's worst performers",
    subtitle = "Poverty, social and material deprivation, and difficulty affording protein-rich meals are all 
    notably higher in the <span style='color:blue3;'>**Canary Islands (CI)**</span> than in the 
    <span style='color:#6AA6A6;'>**Balearic Islands (BI)**</span> and 
    <span style='color:#4e4e4e;'>**Spain's national average (ES)**</span>.",
    x = "% population",
    y = NULL,
    caption = paste(
      glue::glue("Values for the year {year(max(living_std$date))}"),
      "Data: Eurostat | Made by Oliver Q.R.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey97"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      colour = "#1c3d5a", face = "bold", size = 24, margin = ggplot2::margin(b = 5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = "#51606f", size = 18, margin = ggplot2::margin(t = 5, b = 15)
    ),
    plot.caption = element_text(
      family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5,
      margin = ggplot2::margin(t = 10)
    ),
    axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)
    ),
    axis.text.y.left = element_markdown(
      colour = "#4e5a66", size = 16, margin = ggplot2::margin(r = 10), hjust = 0.5
    ),
    axis.title.x = element_text(colour = "#666666", family = "Poppins", face = "bold", size = 13,
                                margin = ggplot2::margin(t = 10)),
    axis.ticks.y = element_line(colour = "grey97"),
    legend.position = "none",
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )



living_std %>%
  filter(
    region_label %in% c("CI", "BI", "ES")
  ) %>%
  pivot_longer(
    cols = c(poverty_rate, inab_meal, mat_soc_depr),
    names_to = "variable",
    values_to = "perc"
  ) %>%
  filter(!is.na(perc)) %>%
  group_by(variable, geo) %>%
  filter(
    date == min(date) | date == max(date)
  ) %>%
  arrange(geo, variable, date) %>%
  mutate(
    change = perc - lag(perc)
  ) %>%
  ungroup()




# income inequality (S80/S20)
s80s20_es <- s80s20 %>%
  filter(
    str_starts(geo_code, "ES(?=\\d{2})")
  ) %>%
  select(date, geo_code, geo, s80s20) %>%
  bind_rows(
    s80s20 %>%
      filter(geo == "Spain") %>%
      select(date, geo_code, geo, s80s20)
  )


eurostat_names_en <- c(
  "Ciudad de Melilla" = "Melilla",
  "Ciudad de Ceuta" = "Ceuta",
  "Comunitat Valenciana" = "Valencian Community",
  "ES70 Canarias" = "<span style='color:blue3;'>**Canary Islands**</span>",
  "ES30 Comunidad de Madrid" = "Madrid",
  "Andalucía" = "Andalusia",
  "Spain" = "<span style='color:#4e4e4e;'>**Spain**</span>",
  "Comunidad Foral de Navarra" = "Navarre",
  "Castilla y León" = "Castile and León",
  "Región de Murcia" = "Murcia",
  "Castilla-La Mancha" = "Castilla—La Mancha",
  "País Vasco" = "Basque Country",
  "La Rioja" = "La Rioja",
  "Cataluña" = "Catalonia",
  "Cantabria" = "Cantabria",
  "Principado de Asturias" = "Asturias",
  "Extremadura" = "Extremadura",
  "Illes Balears" = "<span style='color:#6aa6a6;'>**Balearic Islands**</span>",
  "Aragón" = "Aragon",
  "Galicia" = "Galicia"
)


s80s20_es %>%
  mutate(
    region_en = eurostat_names_en[geo],
    region_label = case_when(
      str_detect(geo, "Canarias") ~ "CI",
      geo == "Illes Balears" ~ "BI",
      geo == "Spain" ~ "ES",
      .default = NA
    )
  ) %>%
  filter(date == max(date)) %>%
  ggplot(
    aes(x = s80s20, y = fct_reorder(region_en, s80s20))
  ) +
  geom_segment(
    aes(x = 0, xend = s80s20),
    colour = "#dfe6ea",
    linewidth = 1,
    lineend = "round"
  ) +
  geom_point(
    data = ~.x %>%
      filter(!region_label %in% c("CI", "BI", "ES")),
    shape = 21,
    size = 4,
    fill = "#c7d3d9",
    colour = "#1c3d5a",
    alpha = 0.8
  ) +
  geom_point(
    data = ~.x %>%
      filter(region_label == "ES"),
    shape = 21,
    size = 6,
    fill = "#4e4e4e",
    colour = "white"
  ) +
  geom_point(
    data = ~.x %>%
      filter(region_label == "BI"),
    shape = 21,
    size = 6,
    fill = "#6aa6a6",
    colour = "white"
  ) +
  geom_point(
    data = ~.x %>%
      filter(region_label == "CI"),
    shape = 21,
    size = 7,
    fill = "blue3",
    colour = "white"
  ) +
  geom_label(
    data = ~.x %>%
      filter(region_label == "ES"),
    aes(label = glue::glue("{region_label}: {s80s20}")),
    size = 3.5,
    hjust = -0.3,
    text.colour = "#4e4e4e",
    border.colour = "white",
    label.r = unit(0.2, "lines"),
    label.padding = unit(c(0.1, 0.2), "lines")
  ) +
  geom_label(
    data = ~.x %>%
      filter(region_label == "BI"),
    aes(label = glue::glue("{region_label}: {s80s20}")),
    size = 3.5,
    hjust = -0.3,
    text.colour = "#6aa6a6",
    border.colour = "white",
    label.r = unit(0.2, "lines"),
    label.padding = unit(c(0.1, 0.2), "lines")
  ) +
  geom_label(
    data = ~.x %>%
      filter(region_label == "CI"),
    aes(label = glue::glue("{region_label}: {s80s20}"), fontface = "bold"),
    size = 4.5,
    hjust = -0.3,
    text.colour = "blue3",
    border.colour = "white",
    label.r = unit(0.2, "lines"),
    label.padding = unit(c(0.1, 0.2), "lines")
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, 2.5),
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "Income inequality in the Canary Islands ranks among the highest in Spain",
    subtitle = "In 2024, income inequality in the <span style='color:blue3;'>**Canary Islands**</span> 
      was notably higher than in the <span style='color:#6aa6a6;'>**Balearic Islands**</span> and slightly 
      above <span style='color:#4e4e4e;'>Spain's national average</span>.",
    x = "S80/S20 ratio",
    y = NULL,
    caption = paste(
      glue::glue("The graph shows the S80/S20 income quintile share ratio for the year {year(max(s80s20_es$date))}"),
      "Data: Eurostat | Made by Oliver Q.R.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = "grey95"),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(colour = "#1c3d5a", family = "Poppins", face = "bold", size = 24,
                                        margin = ggplot2::margin(b = 5)),
    plot.subtitle = element_textbox_simple(colour = "#51606f", family = "Poppins", size = 18,
                                           margin = ggplot2::margin(t = 5, b = 15)),
    plot.caption = element_text(family = "Poppins", colour = "#6d7b8d", face = "italic", size = 10, lineheight = 1.5,
                                margin = ggplot2::margin(t = 10)),
    axis.text = element_text(colour = "#999999", face = "bold", size = 11),
    axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
    axis.text.y.left = element_markdown(colour = "#4e5a66", size = 16, margin = ggplot2::margin(r = 5)),
    axis.title.x = element_text(colour = "#666666", family = "Poppins", face = "bold", size = 13,
                                margin = ggplot2::margin(t = 10)),
    axis.ticks.x = element_line(colour = "grey95"),
    legend.position = "none",
    plot.margin = ggplot2::margin(10, 10, 10, 10)
  )
