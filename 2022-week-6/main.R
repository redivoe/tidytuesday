library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(rmapshaper)
library(tidygeocoder)
library(geofacet)
library(ggrepel)
library(showtext)
library(ggtext)
#

font_add_google("Rubik","khand")
showtext_auto()

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

airmen_geocoded <- airmen %>%
  select(hometown = military_hometown_of_record, state) %>%
  filter(!(is.na(hometown) & is.na(state)),
         !(hometown %in% c("Unk", "Unknown"))) %>%
  mutate(state = recode(state,
                        "TD" = "Trinidad and Tobago",
                        "CT" = "CN")) %>% 
  unite(hometown, state, col = "address", sep = ", ", remove = FALSE) %>% 
  rowid_to_column(var = "id") %>% 
  geocode(address, method = "osm")


# Might be worth trying after rounding
# tidygeocoder::reverse_geo()
approx_coords <- airmen_geocoded %>% 
  filter(!is.na(long),
         !is.na(lat)) %>% 
  mutate(across(.cols = c(long, lat), .fns = round)) %>% 
  group_by(long, lat) %>% 
  summarise(n = n(),
            hometown = names(sort(table(hometown), decreasing = TRUE)[1]),
            .groups = "drop")

# Finding hometowns not in the 50 US states
states_codes <- geofacet::us_state_grid1 %>% pull(code)
airmen_geocoded %>% 
  filter(!(state %in% states_codes))

approx_coords <- approx_coords %>% 
  mutate(
    hometown = recode(hometown,
                  "Port of Spain" = "Trinidad and Tobago",
                  "Port au Prince" = "Haiti",
                  "St. Croix" = "US Virgin Islands",
                  "Christiansted" = "US Virgin Islands"),
    overseas = if_else(hometown %in% c("Trinidad and Tobago", "Haiti", "US Virgin Islands"), TRUE, FALSE)
)

# not needed given that force = 0 in geom_text_repel
# set.seed(17)

world_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_transform(crs = 4326) %>% 
  rmapshaper::ms_simplify()

font_size <- 3
ggplot(world_map) +
  geom_sf(fill = NA) +
  geom_sf(data = approx_coords %>% 
            st_as_sf(.,
                     coords = c("long", "lat"),
                     crs = 4326,
                     agr = "constant"),
          aes(size = n),
          col = "darkred", alpha = 0.8)+
  coord_sf(xlim = c(-125, -60), ylim = c(15, 50))+
  geom_text_repel(data = approx_coords %>% 
              slice_max(order_by = n, n = 5),
            aes(x = long, y = lat, label = str_to_upper(paste0(hometown,": ",n))),
            size = font_size,
            arrow = arrow(length = unit(0, "lines")),
            alpha = 0.9,
            force = 0,
            nudge_x = c(10,0,10,12,8),
            nudge_y = c(1,3,0,0.5,0)
            )+
  geom_text_repel(data = approx_coords %>% 
                    filter(overseas),
                  aes(x = long, y = lat, label = str_to_upper(paste0(hometown,": ",n))),
                  size = font_size*0.7,
                  arrow = arrow(length = unit(0, "lines")),
                  alpha = 0.9,
                  force = 0,
                  nudge_x = c(2,0,0),
                  nudge_y = c(2,-1,0),
                  fontface = "italic")+
  labs(title = str_to_upper("Where did the Tuskegee Airmen come from?"),
       caption = "@redivoed")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 5*font_size),
        legend.position = "none",
        text = element_text(family = "khand"),
        plot.background = element_rect(fill = "floralwhite", color = NA),
        plot.caption = element_text(vjust = 2,hjust = 0.01, family = "khand", size = 6, color = "grey30"))

ggsave(filename = "tuskegee-airmen-hometowns.png",width = 7, height = 4.8, dpi = "retina")

