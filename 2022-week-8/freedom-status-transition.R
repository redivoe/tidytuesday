library(tidyverse)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

freedom <- freedom %>% 
  janitor::clean_names()

status_transition <- freedom %>% 
  filter(year %in% c(1995, 2020)) %>%
  mutate(status = factor(status,
                         levels = c("NF", "PF", "F"),
                         labels = c("Not Free", "Partially Free", "Free"),
                         ordered = TRUE)) %>% 
  pivot_wider(id_cols = country,
              values_from = status,
              names_from = year,
              names_glue = "year_{year}") %>% 
  na.omit() %>% 
  group_by(year_1995, year_2020) %>% 
  summarise(n = n(),
            which = list(country),
            .groups = "drop") %>% 
  complete(year_1995, year_2020)

manual_text_cols <- scales::viridis_pal(option = "G", begin = 0.3)(n = 10)[c(2, 9, 10)]
background_color <- "ghostwhite"
font <- "IBM Plex Sans"

status_transition %>% 
  ggplot(aes(x = year_1995, y = year_2020))+
  geom_tile(aes(fill = n), col = "lavender")+
  geom_text(aes(label = n, col = cut(n, breaks = 4)))+
  scale_color_manual(values = manual_text_cols)+
  scale_fill_viridis_c(option = "G",
                       direction = -1,
                       begin = 0.3, na.value = "white") +
  coord_equal()+
  labs(x = "1995", y = "2020",
       title = "Freedom in the World Index and the aggregate stagnation",
       subtitle = "Number of countries by their status in 1995 and in 2020.",
       caption = "@edoardoredivo")+
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(angle = 0, margin = margin(10, 0, 0, 0)),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = background_color,
                                       color = background_color),
        plot.title.position = "plot",
        text = element_text(family = font),
        plot.caption = element_text(hjust = 0, size = 6, color = "grey30"),
        plot.caption.position = "plot")

ggsave("status-transition.png", width = 6, height = 5, dpi = "retina")

# The two countries that "changed completely"
status_transition %>%
  filter((year_1995 == "Free" & year_2020 == "Not Free") |
         (year_1995 == "Not Free" & year_2020 == "Free")) %>% 
  unnest(which)

