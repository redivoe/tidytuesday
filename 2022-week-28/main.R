library(tidyverse)
library(lubridate)
library(colorspace)
library(gganimate)
library(gifski)
library(here)

flights <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-12/flights.csv')

data <- flights |> 
  janitor::clean_names() |> 
  filter(year >= 2018) |> 
  transmute(date = as_date(flt_date)) |> 
  arrange(date) |> 
  group_by(date) |> 
  summarise(n = n(), .groups = "drop") |> 
  mutate(x = row_number(),
         y = loess(formula = .data$n ~ .data$x, span = 0.1)$fitted,
         dist = abs((lag(y, default = y[1]) - y)^4) + abs((lag(x, default = x[1]) - x)^4),
         time = cumsum(dist),
         angi = c(0, (y[-1] - y[-n()])/y[-n()]))

background_color <- colorspace::lighten("deepskyblue", amount = 0.2)

static_plot <- data |>
  ggplot(aes(x = date, y = y))+
  geom_line(color = "white")+
  geom_text(aes(angle = atan(angi)*1e4),
            label = "\u2708", family = "Arial Unicode MS", size = 10)+
  labs(title = "Number of daily flights in the EU",
       caption = "@edoardoredivo")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill = background_color, colour = NA),
        plot.background = element_rect(fill = background_color, colour = NA),
        axis.text = element_text(color = "white"),
        text = element_text(color = "white", family = "IBM Plex Sans"),
        plot.caption = element_text(hjust = 1, size = 4, color = "grey95"),
        plot.caption.position = "panel")

dynamic_plot <- static_plot+
  transition_reveal(along = date, keep_last = FALSE)

fps <- 50
animate(dynamic_plot, width = 4, height = 3.5, res = 200,
        units = "in", duration = 13, end_pause = fps * 1, fps = fps)
anim_save(here("2022-week-28", "air-traffic-europe.gif"))
