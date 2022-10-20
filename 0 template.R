# packages ----------------------------------------------------------------

source("0 data.R")
# needs()

# data --------------------------------------------------------------------

 abc <- read_excel("./data/") %>% 
  print()

# plot --------------------------------------------------------------------
  labs(title = "",
       subtitle = "",
       caption = "Data: ") 
  ggsave("./img/.pdf",
         width = width_, height = height_, units = "mm",
         useDingbats = FALSE) 
  ggsave("./img/.jpg",
         width = width_, height = height_, units = "mm")

# data --------------------------------------------------------------------

raster <- raster("C:/Users/cdietrich/OneDrive - EU Institute for Security Studies/old dsktp/docs/graphics/maps/raster") %>% 
  print()


# join --------------------------------------------------------------------

_sf <- x %>% 
  mutate(name = recode(name, !!!c("Congo Republic" = "Congo",
                                  "DR Congo" = "Democratic Republic of The Congo",
                                  "Côte d'Ivoire" = "Côte D’Ivoire",
                                  "Cabo Verde" = "Cape Verde",
                                  "Tanzania" = "United Republic of Tanzania"))) %>% 
  # anti_join(countries) %>% 
  left_join(countries) %>%
  st_as_sf() %>%
  print()

# map projections ---------------------------------------------------------

# library(rgeos)
# set_do_poly_check(FALSE)

# equal earth projection
coord_sf(crs = "+proj=eqearth +wktext")

# orthographic
coord_sf(crs = "+proj=ortho +lon_0=10 +lat_0=50")

# EU map 
crs = 3035
# same as European-centric ETRS89 Lambert Azimuthal Equal-Area projection
coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")
coord_sf(crs = 3035,
         xlim = c(2500000, 7000000),
         ylim = c(1000000, 5500000),
         datum = NA)

# robinson
crs = "+proj=robin"
coord_sf(crs = crs, datum = NA) +
coord_sf(crs = "+proj=robin", datum = NA)

coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs", datum = NA) +
# without antarctica
coord_sf(crs = "+proj=robin", 
         datum = NA, 
         ylim = c(-5900000, 9000000),
         expand = FALSE)

# coastline
geom_sf(data = coast, 
	size = lwd_grid, col = col_grid,
	fill = NA)

# simplifying GADM ---------------------------------------------------------

fig14_region_gadm <- c()
foreach(i = 1:length(fig14_region$ISO3)) %do% {
  
  current <- getData("GADM", 
                     country = fig14_region$ISO3[i], 
                     level = 1,
                     path = "./data/shp/") %>% 
    st_as_sf()
  
  fig14_region_gadm <- if (i == 1) {
    current
  } else {
    rbind(fig14_region_gadm, current)
  }
  
}

# giscoR ------------------------------------------------------------------
# params epsg, resolution (03, 10, 20, 60), region, country

coast <- gisco_coastallines %>% 
  st_crop(gisco_countries %>% 
            left_join(giscoR::gisco_countrycode %>% 
                        select(ISO3_CODE, continent)) %>% 
            filter(continent == "Africa")) 

continent <- gisco_countries %>% 
  left_join(giscoR::gisco_countrycode %>% 
              select(ISO3_CODE, continent)) %>% 
  filter(continent == "Africa")

# nedownload --------------------------------------------------------------

coast <- ne_coastline(scale = 110, returnclass = "sf")

countries <- ne_countries(scale = 110, country = data$country, returnclass = "sf") %>% 
  select(admin) %>% 
  print()

bbox <- ne_download(scale = 110, type = "wgs84_bounding_box", category  = "physical", returnclass = "sf")