library(tidyverse)
library(ggfittext)
library(extrafont)
library(ragg)
library(here)

chopped <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

chopped <- chopped %>%
  filter(season<=5) %>%
  group_by(season) %>%
  mutate(avg_rate = mean(episode_rating),
         more_less = ifelse(episode_rating>avg_rate, "more", "less"),
         season_start = min(series_episode),
         season_end = max(series_episode),
         season_word = case_when(
           season == 1 ~ "One",
           season == 2 ~ "Two",
           season == 3 ~ "Three",
           season == 4 ~ "Four",
           season == 5 ~ "Five"
         ))

chopped_title <- "Chopped: IMDB ratings by season"
chopped_caption <-"Data: Kaggle & IMDB | Visual: @AndrewJamesMott | #TidyTuesday Week 35 2020"

chopped_plot <- ggplot(chopped, aes(x = series_episode, y = episode_rating)) +
   geom_rect(aes(xmin = season_start -0.5,
                xmax = season_end +1,
                ymin = 7.5,
                ymax = 10,
                fill = season)) + 
  geom_rect(aes(xmin = -5,
                xmax = 1.1,
                ymin = 7.5,
                ymax = 10),
                fill = "#132B43")+
  geom_line(aes(x = series_episode, y = avg_rate, 
                group = season), color = "white")+
  geom_segment(aes(x = series_episode,
                   xend = series_episode,
                   y = avg_rate,
                   yend = episode_rating),
               linetype = "dotted",
               color = "white") +
  geom_point(aes(color = more_less)) +
  scale_color_manual(values = c(
    more = "green",
    less = "red"
  )) +
  geom_fit_text(aes(xmin = season_start - 1,
                    xmax = season_end,
                    ymin = 7.5 , ymax = 8,
                    label = season_word),
                color = "white", size = 40,
                family = "Cooper Black") +
  xlim(-5,66)+
  scale_y_continuous(breaks = seq(8,9.4, 0.2),
                     expand = expansion(add = 1)) +
  geom_segment(x = -1 , xend =-1, y = 8.0, yend = 9.4,
               color = "white") +
  annotate("segment", x = -1.2, xend = -1.01,
           y = seq(8,9.4,0.2), yend = seq(8,9.4,0.2),
           color = "white") +
  annotate("text", x = 20, y = 9.8, label = chopped_title,
           family = "Cooper Black", color = "white",
           size = 10) +
  annotate("text", x = 25, y = 9.65, label = chopped_caption,
           family = "Cooper Black", color = "white",
           size = 5) +
   theme(legend.position = "none",
        panel.background = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(margin = margin(r = -75, l = 10), color = "white"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = grid::unit(c(-6,-1.4,-6,0), "cm"))

ggsave("Week 35.png",plot = chopped_plot, path = here("2020","week 35"),
       device = agg_png(width = 740 , height = 400, units = "px"))
