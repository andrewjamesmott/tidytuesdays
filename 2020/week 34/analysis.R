library(here)
library(tidyverse)
library(ggforce)
library(statebins)
library(ragg)
library(patchwork)


actions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')

plants_group <- actions %>%
  mutate(x = case_when(
              action_type == "Land & Water Protection" ~ -5,
              action_type == "Species Management" ~ 3,
              action_type == "Law & Policy" ~ -3,
              action_type == "Research & Monitoring" ~ 5,
              action_type == "Education & Awareness" ~ -3,
              action_type == "Unknown" ~ 3),
                
         y = case_when(
           action_type == "Land & Water Protection" ~ 0,
           action_type == "Species Management" ~ 5,
           action_type == "Law & Policy" ~ 5,
           action_type == "Research & Monitoring" ~ 0,
           action_type == "Education & Awareness" ~ -5,
           action_type == "Unknown" ~ -5) ,
      
         angle = case_when(
           action_type == "Land & Water Protection" ~ 0,
           action_type == "Species Management" ~ 60,
           action_type == "Law & Policy" ~ 120,
           action_type == "Research & Monitoring" ~ 180,
           action_type == "Education & Awareness" ~ 240,
           action_type == "Unknown" ~ 300)
  ) %>%
  group_by(group, action_type) %>%
  mutate(n = length(unique(binomial_name)),
         freq = sum(action_taken),
         action_prop = freq/n) %>%
  ungroup()



# Plot

p1 <- ggplot(plants_group) +
  statebins:::geom_rrect(mapping = aes(xmin = -19, xmax = 19,
                                       ymin = -19 , ymax = 19),
                         color = "white") +
  geom_ellipse(aes(x0 = x, y0 = y, a = 6,
                   b = 2, angle = angle*pi/180,
                   m1 = 1.5, alpha = action_prop,
                   color = action_type, fill = action_type)) +
  geom_point(x = 0, y = 0, color = "yellow", size = 4) +
  geom_text(x = -15, y = 0, aes(label = group), color ="white") +
  ylim(-20, 20) +
  xlim(-20,20) +
  coord_flip() +
  facet_wrap(~group) +
  theme(aspect.ratio =1, 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())


# Key 

annotate_plants <- tibble(x1 = c(-11,-1, 12, -1, -11, -17),
                          x2 = c(-6, 4, 11, 4, -6, -10),
                          y1 = c(13, 16, 10, -13,-13, -5),
                          y2 = c(9, 9, 0.5, -9, -9,-1),
                          group = c("b","b","b", "a", "a","a"),
                          label = c("Law & Policy",
                                    "Species Management",
                                    "Research & Monitoring",
                                    "Unknown",
                                    "Education & Awareness",
                                    "Land & Water Management"))



p2 <- ggplot(plants_group) +
  geom_ellipse(aes(x0 = x, y0 = y, a = 6,
                   b = 2, angle = angle*pi/180,
                   m1 = 1.5,
                   color = action_type, fill = action_type)) +
  geom_point(x = 0, y = 0, color = "yellow", size = 8) +
  
  geom_curve(data = filter(annotate_plants,group == "a"),
             aes(x = x1, y = y1, 
                 xend = x2, yend = y2),
                 curvature = -0.3,
             arrow = arrow(angle = 20, length = unit(0.04, "npc")),
             color = "white") +
  
  geom_curve(data = filter(annotate_plants,group == "b"),
             aes(x = x1, y = y1, 
                 xend = x2, yend = y2),
             curvature = 0.3,
             arrow = arrow(angle = 20, length = unit(0.04, "npc")),
             color = "white") + 
  
  geom_text(data = annotate_plants, aes(x = x1-0.8, y = y1, label = label),
            color = "white") +
  ylim(-20, 20) +
  xlim(-20,20) +
  coord_flip(clip = 'off') +
  theme(aspect.ratio = 1,
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = "None")

  

# layout


plant_plot <- p1 + p2 +
  plot_annotation(
   title = "Actions taken for different types of plant on the IUCN Red List",
   subtitle = "The more opaque the petal the larger proportion of plants in that group had action taken",
   caption = "Data: IUCN | Visual: @AndrewJamesMott | #TidyTuesday Week 34") &
theme(plot.background = element_rect(fill = "gray32", color = "gray32"),
      text = element_text(color = "white"))


ggsave("Week 34.png",plot = plant_plot, path = here("2020","week 34"),
       device = agg_png(width = 800 , height = 400, units = "px")) 