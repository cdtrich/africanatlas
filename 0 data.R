# sources -----------------------------------------------------------------

library(needs)
needs::toProfile()
source("C:/Users/cdietrich/OneDrive - EU Institute for Security Studies/Desktop/R/style_v6.R")
needs(euissR, rnaturalearth, sf, giscoR, crsuggest, countrycode)

# helper functions --------------------------------------------------------

source("0 helpers.R")

# crop --------------------------------------------------------------------

gisco_countries <- giscoR::gisco_countries %>% 
  print()

gisco_coast <- giscoR::gisco_coastallines %>%
  print()

crop <- rnaturalearth::ne_countries(scale = 110,
                     returnclass = "sf") %>% 
  filter(continent == "Africa") %>% 
  st_bbox()

# crs ---------------------------------------------------------------------

# crs <- gisco_coast %>%
#   st_crop(crop) %>%
#   crsuggest::suggest_crs() %>%
#   print()
# crs <- 32633
crs <- 32433

coastline_sf <- gisco_coast %>% 
  st_crop(crop) %>% 
  st_transform(crs)

# countrycodes ------------------------------------------------------------

iso <- countrycode::codelist %>% 
  filter(continent == "Africa") %>% 
  select(country = country.name.en,
         iso = iso3c)

# path to data ------------------------------------------------------------

datapath <- here() %>% 
  str_replace("/graphics",
              "/data") %>% 
  str_c("/") %>% 
  print()
