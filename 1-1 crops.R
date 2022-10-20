# packages ----------------------------------------------------------------

source("0 data.R")
# needs()

# sources -----------------------------------------------------------------

# https://www.mapspam.info/data/

# to do -------------------------------------------------------------------

# raster can't handle crs
## i.e. turn coast & other sf into multi/linestring/polygon 
## or fix raster

# access ------------------------------------------------------------------

# download from url, unzip, unlink
# saves to df called 'data'
downloadunzip(spam_prod,
              "https://s3.amazonaws.com/mapspam/2010/v2.0/geotiff/spam2010v2r0_global_val_prod_agg.geotiff.zip",
              "spam2010V2r0_global_V_WHEA_agg_TA.tif",
              unlink = FALSE)

# parse -------------------------------------------------------------------

# local read
# spam_prod <- read_csv("data/1 Prosperity/1 Food Security/spam2010v2r0_global_val_prod_agg.csv/spam2010V2r0_global_V_agg_TA.csv") %>% 
#   print()

# read from temp
spam_prod <- data 
rm(data)

spam_prod_crop <- spam_prod %>%
  # select crops
  filter(iso3 %in% iso$iso) %>%
  select(iso = iso3,
         x, y,
         vp_crop_a) %>%
  sf::st_as_sf(coords = c("x", "y"),
               crs = 4326) %>%
  sf::st_transform(crs = crs) %>%
  # drop geometry for geom_raster()
  # mutate(x = st_coordinates(.)[,1],
  #        y = st_coordinates(.)[,2]) %>%
  # st_drop_geometry() %>%
  print()
# write to rdata 
write_rds(spam_prod_crop,
          "data/1 Prosperity/1 Food Security/1 processed data/spam_prod_crop.rds")
# remove raw df
rm(spam_prod)

read_csv(unz("data/1 Prosperity/1 Food Security/spam2010v2r0_global_val_prod_agg.csv.zip", 
                       "spam2010V2r0_global_V_agg_TA.csv"))


# read processed data back in  --------------------------------------------

spam_prod_crop <- read_rds("data/1 Prosperity/1 Food Security/1 processed data/spam_prod_crop.rds")

# plot --------------------------------------------------------------------

ggplot() +
  geom_sf_coast() +
  geom_sf(data = spam_prod_crop,
              aes(col = vp_crop_a),
            # shape = 19,
            # stroke = .0001,
            size = .25) +
  # geom_raster(data = spam_prod_crop,
  #             aes(x, y,
  #                 fill = vp_crop_a)) +
  scale_color_gradientn(colors = pal_seq_v(4)) +
  scale_fill_gradientn(colors = pal_seq_v(4)) +
  coord_sf(crs = crs,
           datum = NA) 
ggsave_euiss("img/1-1 crops.jpg",
             publication = "cp",
             w = "full",
             h = 1)
