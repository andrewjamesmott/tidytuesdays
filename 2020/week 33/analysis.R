library(here)
library(tidyverse)
library(extrafont)
library(ggforce)
library(ggtext)
library(ragg)


avatar <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

mapping_info <- read_csv(here("2020", "week 33", "map_info","map_info_grouped.csv"))

map_locations <- read_csv(here("2020", "week 33", "map_info","locations.csv"))

# search words 

map_locations$value <- numeric(length = length(map_locations$location_name))
map_locations$spec_text <- str_replace_all(map_locations$spec_text,", ", "|")

for(i in seq_along(map_locations$location_name)){
  if(is.na(map_locations$spec_text[i]) && is.na(map_locations$alt_text[i])){
    
    map_locations$value[i] <- sum(str_detect(avatar$full_text, 
                                         regex(map_locations$location_name[i],
                                              ignore_case = TRUE)))

  }else{
    if(is.na(map_locations$spec_text[i])){
      map_locations$value[i] <- sum(str_detect(avatar$full_text, 
                                           regex(map_locations$alt_text[i],
                                                 ignore_case = TRUE)))

    }else{
      
     avatar_2 <- avatar %>%
       filter(book_num == map_locations$book[i] & chapter_num == map_locations$chapter[i])
      
     
     map_locations$value[i] <- sum(str_detect(avatar_2$full_text, 
                                              regex(map_locations$spec_text[i],
                                                 ignore_case = TRUE)))
      
      
    }
    
    
  }
  
  
  
}



avatar_map <- ggplot(mapping_info, aes(x,y)) +
  geom_bspline_closed(aes(group = area, fill = tribe)) +
  scale_fill_manual(values = c(
                    "fire" = "orangered3",
                    "water" = "turquoise 2" ,
                    "air" = "gray84",
                    "earth" = "burlywood4")) +
  ylim(200,3360) +
  geom_segment(data = map_locations,
               aes(x = x, yend = y, xend = x, y = y + value*10),
               size = 2, color = "dodgerblue", 
               lineend = "round",
               arrow = arrow(type = "closed", length = unit(0.3, "cm")))  +
  labs(
    caption ="Data: Appa package | Original Map and Locations: Avatar Fandom Wiki | Visual: @AndrewJamesMott | #TidyTuesday week 33"
  ) +
  annotate("text", x = 1650, y = 3360,
           label = "How often are different locations talked about in Avatar: The Last Airbender?",
           family = "Herculanum", size = 5) +
  annotate("text", x = 1550, y = 3260,
           label = "Each arrow is proportional to the number of mentions,with two cities Ba Sing Se and Omashu having the most mentions",
           family = "Herculanum", size = 3) +
  theme(legend.position = "none",
        text = element_text(family = "Herculanum", size = 15),
        panel.background = element_rect(fill = "wheat"),
        plot.background = element_rect(fill = "wheat"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


ggsave("Week 33.png",plot = avatar_map, path = here("2020","week 33"),
       device = agg_png(width = 800 , height = 600, units = "px")) 
