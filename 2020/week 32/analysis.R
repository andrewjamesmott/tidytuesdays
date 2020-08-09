library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(extrafont)
library(ragg)
library(ggtext)
library(glue)


# Load Data

energy_types <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

# Match country names

energy_types$country_name[energy_types$country == "UK"] <- "United Kingdom"
energy_types$country_name[energy_types$country_name == "Bosnia & Herzegovina"] <- "Bosnia and Herzegovina"
energy_types$country_name[energy_types$country_name == "North Macedonia"] <- "Macedonia"
energy_types$country_name[energy_types$country_name == "Czechia"] <- "Czech Republic"


# Get Map data

map_data <- ne_countries(continent = "europe",
                         scale = 50,
                         returnclass = "sf") %>%
            filter(!name %in% c("Russia", "Iceland"))

map_data_2 <- ne_countries(country = c("Georgia",
                                       "Cyprus",
                                       "Turkey"), 
                           scale = 50 , returnclass = "sf")

map_data <-  bind_rows(map_data, map_data_2)

rm(map_data_2)


# calculate total, proportion and change in proportion of clean energy

energy_group <- energy_types %>%
  mutate(type_2 = ifelse(type=="Conventional thermal",
                               "not_clean", "clean")) %>%
  group_by(country_name,type_2) %>%
  summarise(total_2016 = sum(`2016`),
            total_2018 = sum(`2018`)
            ) %>%
  pivot_wider(names_from = type_2,
              values_from = c(total_2016,
                              total_2018)
  ) %>%
  mutate(prop_clean_2016 = total_2016_clean/(total_2016_clean + total_2016_not_clean),
         prop_clean_2018 = total_2018_clean/(total_2018_clean + total_2018_not_clean),
         change = round((prop_clean_2018 - prop_clean_2016) * 100,2),
         prop_map = prop_clean_2018 *100
        )




# Join map data and energy data


energy_map <- full_join(map_data,
                        energy_group, 
                        by = c("name_long" ="country_name")
                        )

# labels 

# Select top 10 and bottom 10

energy_map_high <- energy_map %>%
                    slice_max(change,n = 5)

energy_map_low <- energy_map %>%
                    slice_min(change,n = 5)

energy_map_2 <- bind_rows(energy_map_low,energy_map_high)

rm(energy_map_low, energy_map_high)




# Label locations

energy_map_2$nudge_x <- 0
energy_map_2$nudge_y <- 0

energy_map_2$nudge_x[energy_map_2$name %in% c("Portugal",
                                            "Ireland",
                                            "Belgium")] <-  -11

energy_map_2$nudge_y[energy_map_2$name %in% c("Belgium")] <-  -4



energy_map_2$nudge_x[energy_map_2$name %in% c("Germany")] <-  -6

energy_map_2$nudge_x[energy_map_2$name %in% c("Malta")] <-  -8

energy_map_2$nudge_y[energy_map_2$name %in% c("Germany")] <-  6

energy_map_2$nudge_y[energy_map_2$name %in% c("Malta")] <-  1

energy_map_2$nudge_x[energy_map_2$name %in% c("Finland",
                                              "Lithuania",
                                              "Latvia",
                                              "Bosnia and Herz.")] <-  14

energy_map_2$nudge_x[energy_map_2$name %in% c("Austria")] <-  30

# Title

map_title <- glue("<span style='font-size:25pt;color:white'>",
                  "What proportion of each country's <br> energy was clean in 2018?",
                  "</span><br><br>",
                  "<span style='font-size:15pt;color:white'>",
                  "Biggest percentage increases and <br> decreases in clean energy from 2016 labelled",
                  "<br> <br>",
                  "**Data:** Eurostat<br>",
                  "**Visualisation:** @AndrewJamesMott<br>",
                  "#TidyTuesday | Week 32",
                  "</span>")

# map 

map_plot <- ggplot(energy_map) +
  
  geom_sf(aes(fill = prop_map))+
  coord_sf(xlim = c(-18, 50), ylim = c(35, 69),
           crs = st_crs(3857), expand = TRUE, clip = "off") +
  
  geom_sf_label(data = energy_map_2, aes(label = change),
               nudge_x = energy_map_2$nudge_x,
               nudge_y = energy_map_2$nudge_y,
               fill = if_else(energy_map_2$change > 0,"green", "red"),
               family = "Century Gothic",
               color = "white"
               ) +
  
  geom_segment(data = energy_map_2, 
    aes(geometry = geometry, 
                   xend = after_stat(x) ,
                   yend = after_stat(y),
                   x = after_stat(x) + energy_map_2$nudge_x *0.9,
                   y = after_stat(y) + energy_map_2$nudge_y * 0.85),
               stat = "sf_coordinates",
               arrow = arrow(length = unit(0.2, "cm")),
              inherit.aes = FALSE,
              size = 1,
              color = "white") +
  
  scale_fill_gradient(low = "red", high = "green", na.value = "grey",
                      guide = guide_legend( direction = "horizontal",
                        title.position = "top",
                        title.hjust = 0.5,
                        size = 30
                        )) +
  
  labs(fill = "Percentage green energy") +
  
  geom_richtext(aes(x = -3 , y = 66),fill = NA, label = map_title, 
                label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt")) +

  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_rect(fill = "lightblue"),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "lightblue"),
        legend.position = "bottom",
        legend.background = element_rect(fill = "lightblue"),
        text = element_text(family = "Century Gothic", 
                            colour = "white")
        )

ggsave("Week 32.png",plot = map_plot, path = here("2020","week 32"),
       device = agg_png(width = 800 , height = 750, units = "px"))  
  