# packages ----------------------------------------------------------------

source("0 data.R")
# needs()

# to do -------------------------------------------------------------------

# data --------------------------------------------------------------------

arable <- read_csv("data/1 Prosperity/1 Food Security/FAOSTAT_Arable_Crop_Agricultural Land.csv") %>% 
  select(country = 4,
         type = Item) %>% 
  filter(type %in% c("Arable land",
                     "cropland")) %>% 
  print()

darable