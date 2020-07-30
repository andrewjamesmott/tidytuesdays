library(here)
library(tidyverse)
library(magick)
library(ggtext)
library(extrafont)
library(glue)
library(ggridges)


#### Load Data

penguins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')


# Colours

fill_values <- c("#ff6b00","#c95dca", "#067576")

# subttitle

penguin_subtitle = glue("A look at the distribution of mass and flipper length in",
                        "<span style= color:{fill_values[3]}> Gentoo, </span>",
                        "<span style= color:{fill_values[2]}> Chinstrap, </span>",
                        "and",
                        "<span style= color:{fill_values[1]}> Adelie, </span>",
                        "Penguins studied on the Palmer Archipelago, Antarctica"
                        )

# Plot 

plot <- ggplot(penguins, aes(x = body_mass_g, y = species, fill = species)) +
  geom_density_ridges(jittered_points = TRUE,
                      position = position_raincloud(height = 0.6),
                      point_shape = '*',
                      point_color = "white",
                      scale = 0.4,
                      color = "white",
                      aes(point_size = flipper_length_mm)) +
  scale_point_size_continuous(range = c(1, 40)) +
  scale_x_continuous(position = "top") +
  expand_limits(y = 0) +
  xlab("Body Mass (g)")+
  scale_fill_manual(values = alpha(fill_values, 0.7)) +
  annotate("text", x = 2800, y = 3.0,
           label = "The larger the point\n the longer the flippers",
           family = "Kristen ITC",
           color = "white",
           size = 15
  ) +
  annotate("curve", x = 2800, y = 2.8, xend = 3400, yend = 1.65,
           curvature = 0.4, arrow = arrow(length = unit(5, "mm")), 
           color = "white", size = 2) +
  theme(text = element_text(aes(family = "Kristen ITC"),
                            color = "white"),
        #background
        plot.background = element_rect(fill = "slateblue"),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # axes
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "white", 
                                   size = 25),
        axis.title.x = element_text(size = 30),
        panel.grid.minor.x = element_line(color = (alpha("white", 0.7))),
        #Tidy
        legend.position = "none",
        plot.subtitle = element_markdown(margin = margin(0, 0, 50, 0),
                                         size = 35),
        plot.caption = element_markdown(lineheight = 1,  
                                        padding = margin(10, 10, 10, 10),  
                                        halign = 0.5,  
                                        valign = 0.5,
                                        hjust = 0.5,
                                        size = 35),
        plot.title = element_text(size = 50),
 
       
        
  ) +
  labs(
    title = "Palmer Penguins",
    subtitle = penguin_subtitle,
    caption = "**Data**: Dr Kristen Gorman & the Palmer Station LTER | **Graphic**: @AndrewJamesMott | **Artwork**: Allison Horst | #TidyTuesday Week 31" 
  )




# Artwork


penguins_art <- image_read(here("2020","week 31","penguins.png"))
                
image <- image_fill(penguins_art, 'none') %>%
         as.raster(image)

pic_plot <- plot + annotation_raster(image, 5000, 6500, 0, 2)

# Save


ggsave("Week 31.png",plot = pic_plot, path = here("2020","week 31"),
       device = "png", width = 1200, height = 600, units = "mm")

