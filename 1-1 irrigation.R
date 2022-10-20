# packages ----------------------------------------------------------------

source("0 data.R")
needs(osmdata)

# data --------------------------------------------------------------------

# irrigation
irrigation_osm_usage <- crop %>% 
  opq(timeout = 25*100) %>%
  osmdata::add_osm_feature(key = "usage",
                           value = "irrigation") %>%
  osmdata_sf()
# ditches
irrigation_osm_ditch <- crop %>% 
  opq(timeout = 25*100) %>%
  osmdata::add_osm_feature(key = "waterway",
                           value = "ditch") %>%
  osmdata_sf()
# canals
irrigation_osm_canal <- crop %>% 
  opq(timeout = 25*100) %>%
  osmdata::add_osm_feature(key = "waterway",
                           value = "canal") %>%
  osmdata_sf()

# all
# irrigation_osm <- crop %>% 
#   opq(timeout = 100*100) %>%
#   osmdata::add_osm_feature(key = "usage",
#                            value = "irrigation") %>%
#   osmdata::add_osm_feature(key = "waterway",
#                            value = "ditch") %>%
#   osmdata::add_osm_feature(key = "waterway",
#                            value = "canal") %>%
#   osmdata_sf()

irrigation_lines <- bind_rows(
  irrigation_osm_usage$osm_lines,
  irrigation_osm_ditch$osm_lines,
  irrigation_osm_canal$osm_lines)
irrigation_points <- bind_rows(
  irrigation_osm_usage$osm_points,
  irrigation_osm_ditch$osm_points,
  irrigation_osm_canal$osm_points)
# irrigation_lines <- irrigation_osm$osm_lines
# irrigation_points <- irrigation_osm$osm_points

# plot --------------------------------------------------------------------

irrigation_lines %>% 
  ggplot() + 
  geom_sf(data = gisco_coast %>% 
            st_crop(crop),
          fill = NA) +
  geom_sf(data = irrigation_lines, 
          col = teal, size = 1) +
  geom_sf(data = irrigation_points,
          col = teal, size = 1) +
  coord_sf(crs = 32633,
           datum = NA) +
  labs(title = "OSM irrigation",
       subtitle = "",
       caption = "Data: ") 
# ggsave("./img/.pdf",
#        width = width_, height = height_, units = "mm",
#        useDingbats = FALSE) 
# ggsave("./img/.jpg",
#        width = width_, height = height_, units = "mm")
