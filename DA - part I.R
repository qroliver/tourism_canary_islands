# loading packages
library(tidyverse)
library(gt)
library(gtExtras)
library(ggtext)
library(giscoR)
library(sf)
library(patchwork)
library(ggiraph)
library(scales)
library(htmltools)


# importing data
eu_nights_spent_23 <- read_csv("Data/eu_nights_spent_23.csv")
tourists_can <- read_csv("Data/tourists_can.csv")
tourists_by_island <- read_csv("Data/tour_by_island.csv")
average_stay <- read_csv("Data/average_stay.csv")
daily_exp <- read_csv("Data/average_expenditure_by_tour_day.csv")
adr <- read_csv("Data/adr.csv")




# Top EU regions with the most tourist nights in 2023
eu_nights_spent_23 %>%
  filter(unit == "Number",
         geo != "Euro area – 20 countries (from 2023)") %>%
  mutate(geo = case_when(str_detect(geo, "Canarias") ~ "Canary Islands",
                         geo == "Jadranska Hrvatska" ~ "Adriatic Croatia", 
                         geo == "Cataluña" ~ "Catalonia",
                         str_detect(geo, "FR10") ~ "Île-de-France",
                         geo == "Andalucía" ~ "Andalusia",
                         geo == "Illes Balears" ~ "Balearic Islands",
                         str_detect(geo, "FRL0") ~ "Provence-Alpes-Côte d'Azur",
                         geo == "Comunitat Valenciana" ~ "Valencian Community",
                         geo == "Toscana" ~ "Tuscany",
                         geo == "Oberbayern" ~ "Upper Bavaria",
                         .default = geo),
         label = if_else(geo == "Canary Islands",
                         paste0("<span style='color:blue;'>", geo, "</span>"),
                         geo)) %>%
  arrange(desc(values)) %>%
  head(15) %>%
  ggplot(aes(x = values / 1e6, y = fct_reorder(label, values), fill = values)) +
  geom_col() +
  scale_fill_gradient(low = "#fdae6b", high = "#7f2704") +
  scale_x_continuous(breaks = seq(0, 100, 20),
                     limits = c(0, 100),
                     expand = c(0, 1)) +
  labs(title = "Top EU regions with the most tourist nights in 2023",
       subtitle = "Nights spent at hotels, holiday and other short-stay accommodation, 
       camping grounds, recreational vehicle parks and trailer parks",
       x = "Million nights",
       y = element_blank(),
       caption = "Data: Eurostat | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey97"),
        plot.title = element_text(colour = "#1c3d5a", face = "bold", size = 24),
        plot.subtitle = element_textbox_simple(colour = "#6d7b8d", size = 18,
                                               margin = ggplot2::margin(t = 2.5, b = 15)),
        plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_markdown(colour = "#666666", size = 16, margin = ggplot2::margin(r = 5)),
        axis.ticks.y = element_line(colour = "grey97"),
        legend.position = "none",
        plot.margin = ggplot2::margin(10, 10, 10, 10))



# Tourist arrivals per year
tourists_can_2005_9 <- read_csv2("Data/yearly tourists Canary Islands 2005-9.csv")

tourists_can %>%
  filter(country == "Total",
         year < 2025) %>%
  group_by(year) %>%
  summarise(tourists = sum(tourists, na.rm = TRUE) / 1000000, .groups = "drop") %>%
  bind_rows(tourists_can_2005_9 %>%
              rename(year = Year) %>%
              mutate(tourists = tourists / 1000000)) %>%
  ggplot(aes(x = year, y = tourists)) +
  geom_rect(aes(xmin = 2007, xmax = 2013, ymin = -Inf, ymax = Inf), fill = "#dee5ec", alpha = 0.05) +
  geom_rect(aes(xmin = 2010, xmax = 2011, ymin = -Inf, ymax = Inf), fill = "#bfd7d7", alpha = 0.05) +
  geom_rect(aes(xmin = 2020, xmax = 2023, ymin = -Inf, ymax = Inf), fill = "#c4d3df", alpha = 0.025) +
  geom_line(linewidth = 1.2, colour = "blue") +
  geom_point(size = 4, shape = 21, colour = "white", fill = "blue") +
  scale_x_continuous(breaks = seq(2005, 2024, 1),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(0, 20, 2),
                     labels = c("", seq(2, 20, 2)),
                     limits = c(0, 20),
                     expand = c(0, 0)) +
  labs(title = "More tourists visited the Canary Islands each year after COVID.",
       subtitle = "After a dramatic drop due to the pandemic, there was a quick recovery, with 2024 setting \nan all-time high in the number of tourists visiting the islands.",
       x = element_blank(),
       y = "Tourists (millions)",
       caption = "Data: FRONTUR & Instituto Canario de Estadística (ISTAC). | Made by Oliver Q.R.") +
  annotate(geom = "segment", x = 2007, xend = 2013, y = 0.3, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2010, y = 0.8, colour = "black", size = 3.5, hjust = 0.5,
           label = "Global financial crisis") +
  annotate(geom = "segment", x = 2010, xend = 2011, y = 19.5, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2010.5, y = 18.7, colour = "black", size = 3.5, hjust = 0.5,
           label = "Arab\nSpring") +
  annotate(geom = "segment", x = 2020, xend = 2023, y = 0.3, colour = "grey45", 
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2021.5, y = 1.2, colour = "black", size = 3.5, hjust = 0.5,
           label = "COVID-19 public health emergency\nof international concern") +
  annotate(geom = "curve", x = 2022.65, xend = 2023.9, y = 19, yend = 18, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"), curvature = -0.1) +
  annotate(geom = "text", x = 2021.8, y = 19.1, colour = "black", size = 4, fontface = "bold",
           label = "all-time high") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey97"),
        plot.title = element_text(colour = "#1c3d5a", face = "bold", size = 24),
        plot.subtitle = element_text(colour = "#6d7b8d", size = 18,
                                     margin = ggplot2::margin(t = 2.5, b = 15)),
        plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 12),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        axis.ticks = element_line(colour = "grey97"),
        axis.title.y = element_text(size = 14, family = "Cabin", colour = "#555555",
                                    margin = ggplot2::margin(r = 10)),
        plot.margin = ggplot2::margin(10, 10, 10, 10))



# Tourists by island (2024)
# tourists arrived by island in 2024
tour_by_island_2024 <- tourists_by_island %>%
  filter(country == "Total",
         year == 2024) %>%
  group_by(year, island) %>%
  summarise(tourists = sum(tourists, na.rm = TRUE)) %>%
  mutate(percent = 100 * tourists / sum(tourists)) %>%
  ungroup() %>%
  select(-year)


tour_by_island_2024 %>%
  ggplot(aes(x = percent, y = fct_reorder(island, percent), fill = island)) +
  geom_col(width = 0.7) +
  scale_x_continuous(limits = c(0, 50),
                     expand = c(0.01, 0.01)) +
  scale_fill_manual(values = c("Lanzarote" = "#d73027",
                               "Fuerteventura" = "#efc000",
                               "Gran Canaria" = "#1f78b4",
                               "Tenerife" = "#08306b",
                               "La Palma" = "#238b45",
                               "La Gomera & El Hierro" = "#8b5a2b")) +
  labs(title = "Tenerife was the most popular destination in 2024",
       subtitle = "Two out of five tourists visiting the Canary Islands in 2024 chose Tenerife for 
       their holiday. Gran Canaria had the second highest number of arrivals, followed by Lanzarote 
       and Fuerteventura. In contrast, La Gomera, La Palma and El Hierro together accounted for just 2.25% 
       of tourist arrivals last year.",
       x = element_blank(),
       y = element_blank(),
       caption = "Data: Instituto Canario de Estadística (ISTAC). | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey97"),
        plot.title = element_text(colour = "#1c3d5a", face = "bold", size = 24),
        plot.subtitle = element_textbox_simple(colour = "#6d7b8d", size = 18,
                                     margin = ggplot2::margin(t = 2.5, b = 15)),
        plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(colour = "#666666", size = 16, margin = ggplot2::margin(r = 5)),
        axis.ticks.y = element_line(colour = "grey97"),
        legend.position = "none",
        plot.margin = ggplot2::margin(10, 10, 10, 10))


# table
tour_by_island_2024 %>%
  arrange(desc(tourists)) %>%
  mutate(percent = round(percent, 2)) %>%
  gt() %>%
  tab_header(title = "Tourists arrived in the Canary Islands in 2024") %>%
  tab_style(style = list(
    cell_text(color = "white", weight = "bold", size = "x-large"),
    cell_fill(color = "#1c3d5a")
  ),
  locations = cells_title(groups = "title")) %>%
  cols_label(island = "Island",
             tourists = "Tourists",
             percent = "%") %>%
  tab_style(style = list(cell_text(color = "white", weight = "bold", size = "large"),
                         cell_fill(color = "#1c3d5a")),
            locations = cells_column_labels()) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_column_labels(columns = c(tourists, percent))) %>%
  tab_style(style = cell_text(align = "center"),
            locations = cells_body(columns = c(tourists, percent))) %>%
  tab_style(style = cell_borders(color = "#bbc5ce", sides = "bottom"),
            locations = cells_body(rows = island != "La Palma")) %>%
  tab_source_note(source_note = "Data: Instituto Canario de Estadística (ISTAC) | Made by Oliver Q.R.") %>%
  tab_style(style = cell_text(color = "#6d7b8d", style = "italic", size = "small"),
            locations = cells_source_notes()) %>%
  tab_style(style = cell_text(size = "large"),
            locations = cells_body()) %>%
  fmt_number(columns = tourists,
             decimals = 0) %>%
  tab_options(table.font.names = "Poppins",
              heading.padding.horizontal = 10,
              column_labels.padding.horizontal = 10,
              data_row.padding.horizontal = 10,
              table.border.top.style = "none",
              table.border.bottom.style = "none",
              column_labels.border.top.color = "white",
              column_labels.border.bottom.style = "none",
              table_body.border.bottom.color = "#1c3d5a")



# Yearly tourist arrivals per island
# Canary Islands map
# downloading NUTS boundaries for the Canary Islands at the island level
canary_islands_map <- gisco_get_nuts(resolution = "03",
                                     country = "ES",
                                     year = "2024",
                                     nuts_level = "3") %>%
  filter(NUTS_NAME %in% c("Lanzarote", "Fuerteventura", "Gran Canaria", "Tenerife",
                          "La Gomera", "La Palma", "El Hierro"))


can_map <- canary_islands_map %>%
  filter(NUTS_NAME %in% c("La Gomera", "El Hierro")) %>%
  reframe(NAME_LATN = "La Gomera & El Hierro",
          CNTR_CODE = "ES",
          NUTS_NAME = "La Gomera & El Hierro",
          geometry = st_union(geometry)) %>%
  bind_rows(canary_islands_map %>%
              filter(!NUTS_NAME %in% c("La Gomera", "El Hierro"))) %>%
  select(island = NAME_LATN, geometry)


map <- can_map %>%
  ggplot() +
  geom_sf_interactive(aes(geometry = geometry, fill = island, data_id = island, tooltip = island)) +
  scale_fill_manual(values = c("Lanzarote" = "#d73027",
                               "Fuerteventura" = "#efc000",
                               "Gran Canaria" = "#1f78b4",
                               "Tenerife" = "#08306b",
                               "La Palma" = "#238b45",
                               "La Gomera & El Hierro" = "#8b5a2b")) +
  scale_x_continuous(breaks = -14:-18,
                     labels = paste0(abs(-14:-18), "°W")) +
  scale_y_continuous(breaks = seq(28, 29.5, 0.5),
                     labels = paste0(seq(28, 29.5, 0.5), "°N")) +
  theme_bw(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey97"),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        legend.position = "none",
        plot.margin = ggplot2::margin(10, 10, 10, 10))

#map


line_plot <- tourists_by_island %>%
  filter(country == "Total",
         year < 2025) %>%
  group_by(year, island) %>%
  summarise(tourists = sum(tourists, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = tourists, colour = island)) +
  geom_line(linewidth = 1.2) +
  geom_point_interactive(aes(fill = island, data_id = island, 
                             tooltip = paste0("<div style=text-align:center;>", island, "<br>", round(tourists/1e6, 1),"M</div>")), 
                         size = 3, shape = 21, colour = "white") +
  scale_colour_manual(values = c("Lanzarote" = "#d73027",
                                 "Fuerteventura" = "#efc000",
                                 "Gran Canaria" = "#1f78b4",
                                 "Tenerife" = "#08306b",
                                 "La Palma" = "#238b45",
                                 "La Gomera & El Hierro" = "#8b5a2b")) +
  scale_fill_manual(values = c("Lanzarote" = "#d73027",
                               "Fuerteventura" = "#efc000",
                               "Gran Canaria" = "#1f78b4",
                               "Tenerife" = "#08306b",
                               "La Palma" = "#238b45",
                               "La Gomera & El Hierro" = "#8b5a2b")) +
  scale_x_continuous(breaks = seq(2010, 2025, 2),
                     limits = c(2010, 2025),
                     expand = c(0.05, 0.01)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8),
                     limits = c(0, 8e6), 
                     labels = label_number(scale = 1e-6, suffix = "M"),
                     expand = c(0.01, 0.01)) +
  labs(subtitle = "Tourists per year",
       x = element_blank(),
       y = "Tourists (millions)") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey97"),
        plot.subtitle = element_text(colour = "#6d7b8d", size = 16,
                                     margin = ggplot2::margin(t = 2.5, b = 15)),
        axis.title.y = element_text(size = 14, family = "Cabin", colour = "#555555",
                                    margin = ggplot2::margin(r = 10)),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        axis.ticks.y = element_line(colour = "grey97"),
        legend.position = "none",
        plot.margin = ggplot2::margin(10, 10, 10, 20))

#line_plot


bar_chart <- tourists_by_island %>%
  filter(country == "Total") %>%
  group_by(year, island) %>%
  summarise(tourists = sum(tourists, na.rm = TRUE), .groups = "drop") %>%
  filter(year %in% c(2010, 2024)) %>%
  pivot_wider(names_from = year,
              values_from = tourists,
              names_prefix = "tour_") %>%
  mutate(pct_change = 100 * (tour_2024 - tour_2010) / tour_2010,
         label = if_else(pct_change > 0,
                         paste0("+", round(pct_change, 1), "%"),
                         paste0(round(pct_change, 1), "%"))) %>%
  ggplot(aes(x = pct_change, y = fct_reorder(island, pct_change))) +
  geom_vline(xintercept = 0, colour = "#999999") +
  geom_col_interactive(aes(fill = island, data_id = island, tooltip = label)) +
  scale_fill_manual(values = c("Lanzarote" = "#d73027",
                               "Fuerteventura" = "#efc000",
                               "Gran Canaria" = "#1f78b4",
                               "Tenerife" = "#08306b",
                               "La Palma" = "#238b45",
                               "La Gomera & El Hierro" = "#8b5a2b")) +
  scale_x_continuous(breaks = c(-5, seq(0, 100, 10)),
                     limits = c(-7, 100),
                     expand = c(0.05, 0.1)) +
  labs(title = element_blank(),
       subtitle = "Change between 2010 - 2024",
       x = "%",
       y = element_blank()) +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey97"),
        plot.subtitle = element_text(colour = "#6d7b8d", size = 16,
                                     margin = ggplot2::margin(t = 2.5, b = 15)),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(colour = "#555555", size = 12, margin = ggplot2::margin(r = 5)),
        axis.ticks.y = element_line(colour = "grey97"),
        legend.position = "none",
        plot.margin = ggplot2::margin(10, 10, 20, 10))

#bar_chart


yearly_per_island_plot <- (bar_chart + line_plot) / map +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(title = "Yearly tourist arrivals by island",
                  caption = "Data: Instituto Canario de Estadística (ISTAC). | Made by Oliver Q.R.",
                  theme = theme(plot.title = element_text(colour = "#1c3d5a", face = "bold", size = 24, hjust = 0.5),
                                plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
                                plot.margin = ggplot2::margin(10, 10, 10, 10)))


interactive_plot <- girafe(ggobj = yearly_per_island_plot,
                           width_svg = 10,
                           height_svg = 7) %>%
  girafe_options(opts_hover(css = "fill:rgb(60, 175, 194);stroke:black;"))

interactive_plot

#save_html(interactive_plot, "Graphs/yearly_per_island.html")




# tourists by country of origin
tourists_can %>%
  filter(!country %in% c("Total", "Mundo (excluida España)")) %>%
  mutate(country = case_when(country == "Alemania" ~ "Germany",
                             country == "Bélgica" ~ "Belgium",
                             country == "Dinamarca" ~ "Denmark",
                             country == "España (excluida Canarias)" ~ "Spain",
                             country == "Finlandia" ~ "Finland",
                             country == "Francia" ~ "France",
                             country == "Irlanda" ~ "Ireland",
                             country == "Italia" ~ "Italy",
                             country == "Noruega" ~ "Norway",
                             str_detect(country, "Otros países") ~ "Other countries",
                             country == "Países Bajos" ~ "Netherlands",
                             country == "Reino Unido" ~ "United Kingdom",
                             country == "Suecia" ~ "Sweden",
                             country == "Suiza" ~ "Switzerland",
                             .default = country)) %>%
  group_by(year, country) %>%
  summarise(tourists = sum(tourists, na.rm = TRUE) / 1000000, .groups = "drop") %>%
  filter(year %in% c(2024, 2010)) %>%
  pivot_wider(names_from = year,
              values_from = tourists,
              names_prefix = "y") %>%
  mutate(change_percent = 100 * (y2024 - y2010) / y2010) %>%
  ggplot(aes(x = y2024, y = fct_reorder(country, y2024))) +
  geom_segment(aes(x = y2010, xend = y2024), colour = "#777777", linewidth = 1) +
  geom_point(aes(x = y2010, fill = "2010"), size = 5, shape = 21, colour = "white") +
  geom_point(aes(fill = "2024"), size = 5, shape = 21, colour = "white") +
  scale_fill_manual(values = c("2024" = "#1f78b4", "2010" = "#ef8a62"),
                      name = "Year") +
  scale_x_continuous(breaks = seq(0, 8, 1)) +
  labs(title = "British tourists are the largest nationality group visiting the Canary Islands",
       subtitle = "They are also the group with the largest increase over the last 15 years. Germans rank second 
       in number of visitors, followed by mainland Spaniards.",
       x = "Tourists (millions)",
       y = element_blank(),
       caption = "Data: Instituto Canario de Estadística (ISTAC). | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey97"),
        plot.title = element_textbox_simple(colour = "#1c3d5a", face = "bold", size = 24),
        plot.subtitle = element_textbox_simple(colour = "#6d7b8d", size = 18,
                                         margin = ggplot2::margin(t = 5, b = 12.5)),
        plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 11),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(colour = "#666666", size = 16, margin = ggplot2::margin(r = 5)),
        axis.title.x = element_text(colour = "#666666", family = "Cabin", face = "bold", size = 13,
                                    margin = ggplot2::margin(t = 5)),
        axis.ticks = element_line(colour = "grey97"),
        legend.position = "inside",
        legend.position.inside = c(0.95, 0.1),
        legend.background = element_rect(linewidth = 0.2, fill = "white"),
        legend.title = element_text(face = "bold", hjust = 0.5),
        plot.margin = ggplot2::margin(10, 10, 10, 10))




# average daily expenditure (tourist and day)
daily_exp %>%
  filter(is.na(quarter),
         concept == "Total",
         country == "Total") %>%
  ggplot(aes(x = year, y = avg_exp)) +
  annotate(geom = "segment", x = 2010, xend = 2024, y = 177, 
           colour = "grey80", linetype = "dotted") +
  annotate(geom = "segment", x = 2019, xend = 2024, y = 137, 
           colour = "grey80", linetype = "dotted") +
  geom_line(linewidth = 1.2, colour = "blue") +
  geom_point(size = 4, shape = 21, colour = "white", fill = "blue") +
  geom_segment(data = ~.x %>%
                 filter(year %in% c(2010, 2024)) %>%
                 summarise(x = min(year),
                           y = avg_exp[year == min(year)] + 2,
                           yend = avg_exp[year == max(year)] - 1),
               aes(x = x, y = y, yend = yend),
               inherit.aes = FALSE,
               colour = "grey75",
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  annotate(geom = "text", x = 2010.2, y = 145.5, label = "+55.26%", colour = "grey30", 
           fontface = "bold", angle = 90) +
  geom_segment(data = ~.x %>%
                 filter(year %in% c(2019, 2024)) %>%
                 summarise(x = max(year),
                           y = avg_exp[year == min(year)] + 1,
                           yend = avg_exp[year == max(year)] - 2),
               aes(x = x, y = y, yend = yend),
               inherit.aes = FALSE,
               colour = "grey75",
               arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  annotate(geom = "text", x = 2023.8, y = 157, label = "+29.2%", colour = "grey30", 
           fontface = "bold", angle = 90) +
  scale_x_continuous(breaks = 2010:2024,
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(100, 180, 10),
                     limits = c(100, 180)) +
  labs(title = "Tourists visiting the Canary Islands spend more each year",
       subtitle = "Average daily expenditure has risen significantly after COVID, with 
       the past three years setting all-time highs.",
       x = element_blank(),
       y = "Daily average expenditure per tourist (€)",
       caption = "Data: Instituto Canario de Estadística (ISTAC). | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey97"),
        plot.title = element_text(colour = "#1c3d5a", face = "bold", size = 24),
        plot.subtitle = element_textbox_simple(colour = "#6d7b8d", size = 18,
                                     margin = ggplot2::margin(t = 2.5, b = 15)),
        plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 12),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        axis.title.y = element_text(size = 14, family = "Cabin", colour = "#555555",
                                    margin = ggplot2::margin(r = 10)),
        plot.margin = ggplot2::margin(10, 10, 10, 10))




# Total income by the tourist sector
adr %>%
  filter(is.na(month),
         territory == "Canarias",
         variable == "Ingresos totales") %>%
  ggplot(aes(x = year, y = obs_value / 1000000)) +
  geom_rect(aes(xmin = 2009, xmax = 2013, ymin = -Inf, ymax = Inf), fill = "#dee5ec", alpha = 0.05) +
  geom_rect(aes(xmin = 2010, xmax = 2011, ymin = -Inf, ymax = Inf), fill = "#bfd7d7", alpha = 0.05) +
  geom_rect(aes(xmin = 2020, xmax = 2023, ymin = -Inf, ymax = Inf), fill = "#c4d3df", alpha = 0.025) +
  annotate(geom = "segment", x = 2009, xend = 2013, y = 1000, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "last")) +
  annotate(geom = "text", x = 2011, y = 1200, colour = "black", size = 3.5, hjust = 0.5,
           label = "Global financial crisis") +
  annotate(geom = "segment", x = 2010, xend = 2011, y = 6000, colour = "grey45",
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2010.5, y = 5700, colour = "black", size = 3.5, hjust = 0.5,
           label = "Arab\nSpring") +
  annotate(geom = "segment", x = 2020, xend = 2023, y = 6000, colour = "grey45", 
           arrow = arrow(length = unit(0.2, "cm"), type = "open", ends = "both")) +
  annotate(geom = "text", x = 2021.5, y = 5700, colour = "black", size = 3.5, hjust = 0.5,
           label = "COVID-19 public health emergency\nof international concern") +
  geom_line(linewidth = 1.2, colour = "blue") +
  geom_point(size = 3, colour = "blue") +
  scale_x_continuous(breaks = 2009:2024,
                     limits = c(2009, 2024),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = pretty_breaks(n = 6),
                     limits = c(1000, 6000)) +
  labs(title = "Tourism revenue in the Canary Islands keeps breaking all-time records post-COVID",
       subtitle = "Total tourism revenue has reached unprecedented levels in the years post-COVID. 
       In 2024, for the first time ever, total revenue in the Canary Islands surpassed the €5,000 
       million mark. If this post-COVID trend continues, the €6,000 million mark could be exceeded 
       in 2025.",
       x = element_blank(),
       y = "Total revenue (million €)",
       caption = "Data: Exceltur. | Made by Oliver Q.R.") +
  theme_minimal(base_family = "Poppins") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "grey97"),
        #plot.title.position = "plot",
        plot.title = element_textbox_simple(colour = "#1c3d5a", face = "bold", size = 24,
                                            margin = ggplot2::margin(b = 5)),
        plot.subtitle = element_textbox_simple(colour = "#6d7b8d", size = 18,
                                               margin = ggplot2::margin(t = 5, b = 20)),
        plot.caption = element_text(colour = "#6d7b8d", face = "italic", size = 10),
        axis.text = element_text(colour = "#999999", family = "Cabin", face = "bold", size = 12),
        axis.text.x = element_text(margin = ggplot2::margin(t = 5)),
        axis.text.y = element_text(margin = ggplot2::margin(r = 5)),
        axis.title.y = element_text(size = 14, family = "Cabin", colour = "#555555",
                                    margin = ggplot2::margin(r = 10)),
        plot.margin = ggplot2::margin(10, 10, 10, 10))






























































































