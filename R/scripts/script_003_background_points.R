# Script 003: Get background points from Koppen-Geiger zones 

# -----------------------------------------------------------------------------
# Session setup
# -----------------------------------------------------------------------------

# Load required packages
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load(
  tidyverse,
  dismo,
  raster,
  here,
  corrplot,
  Hmisc,
  patchwork,
  ecospat,
  # kuenm,
  gridSVG,
  gridExtra,
  grid,
  ENMeval,
  spThin,
  viridis,
  viridisLite,
  mapdata,
  maptools,
  scales,
  geosphere,
  rgdal,
  ggtext,
  rJava,
  rgeos,
  sp,
  sf,
  ggspatial,
  ecospat,
  rnaturalearth,
  rnaturalearthdata,
  # megaSDM,
  InformationValue,
  caret, 
  terra,
  geodata,
  usdm
)


# Change ggplot theme
theme_set(
  theme_classic() +
    theme(
      panel.border = element_rect(colour = "black",
                                  fill = NA),
      axis.text = element_text(colour = "black"),
      axis.title.x = element_text(margin = unit(c(2, 0, 0, 0),
                                                "mm")),
      axis.title.y = element_text(margin = unit(c(0, 4, 0, 0),
                                                "mm")),
      legend.position = "none"
    )
)

# Set the theme for the maps
theme_opts <- list(
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = 'white', colour = NA),
    plot.background = element_rect(),
    axis.line = element_blank(),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.title.x = element_text(colour = "black"),
    axis.title.y = element_text(colour = "black"),
    plot.title = element_text(colour = "black"),
    panel.border = element_rect(fill = NA),
    legend.key = element_blank()
  )
)


# -----------------------------------------------------------------------------
# Import and crop KG shapefile 
# -----------------------------------------------------------------------------

# Load KG layer
kg_layer <- rgdal::readOGR(here::here("./data/shapefiles/koppen_geiger"), 
                      "WC05_1975H_Koppen", 
                      verbose = FALSE)


# Reproject KG-layer
geo_proj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
kg_layer <- sp::spTransform(kg_layer, geo_proj)

# Plot to make sure KG layer imported correctly 
sp::plot(kg_layer)

# Coerce focal taxon GPS records into SpatialPointsDataFrame (SPDF)
records_spatial <- sp::SpatialPointsDataFrame(
  coords = cbind(sp_africa$lon, sp_africa$lat),
  data = sp_africa,
  proj4string = CRS(
    '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  )
)

# Plot to check we can overlay points on KG map
sp::plot(kg_layer)
points(sp_africa$lon,
       sp_africa$lat,
       pch = 21,
       bg = 'mediumseagreen')

# Select KG ecoregions in which there is at least one GPS record
kg_contain <- kg_layer[records_spatial, ]

# Plot regions containing at least one record
# sp::plot(kg_contain)
sp::plot(
  kg_layer,
  add = TRUE,
  col = 'gray70'
  )
# Fill the KG zones with a khaki colour if they contain at least 1 GPS point
sp::plot(
  kg_contain, 
  add = TRUE, 
  col = 'khaki')
# Overlay GPS points 
points(sp_africa$lon, 
       sp_africa$lat, 
       pch = 21, 
       bg = 'mediumseagreen', 
       cex = 1)

# Define background area by masking WORLDCLIM layers to just the KG zones with at 
# least 1 GPS record
# - First, we have to convert the KG zones containing GPS records back into an 'sf' object
kg_contain <- sf::st_as_sf(kg_contain)
bg_area <- terra::mask(predictors, kg_contain)  

# Plot to check the mask worked
terra::plot(bg_area)

# Sample random points from the background area defined by the KG zones occupied
# - We use these background points as 'pseudo-absences' to test how well our climate
#   model can distinguish between GPS points occupied by our focal species and these 
#   'pseudo-absence' points (pretty ropey assumption to make that the focal species
#    is absent from these GPS points!!!)
set.seed(2023)
bg_points <- terra::spatSample(
  x = bg_area,        # Raster of background area to sample points from 
  size = 1000,        # How many background points do we want?
  method = "random",  # Random points
  replace = FALSE,    # Sample without replacement
  na.rm = TRUE,       # Remove background points that have NA climate data
  as.df = TRUE,       # Return background points as data.frame object
  xy = TRUE           # Return lat/lon values for each background point
  ) %>%
  # Rename lon and lat columns to be consistent with GPS data for focal species 
  dplyr::rename(
    lon = x,
    lat = y
  )
head(bg_points)

# Check background points have been drawn from the correct geographic mask
terra::plot(kg_layer)
points(bg_points$lon, 
       bg_points$lat, 
       pch = 21, 
       bg = 'mediumseagreen', 
       cex = 1)















# -----------------------------------------------------------------------------
# DON'T RUN !!! 
# -----------------------------------------------------------------------------

# Load Koppen-Geiger layer
# - Available from:
#   - Beck, H.E., N.E. Zimmermann, T.R. McVicar, N. Vergopolan, A. Berg, E.F. Wood: #     Present and future Köppen-Geiger climate classification maps at 1-km
#     resolution, Scientific Data 5:180214, doi:10.1038/sdata.2018.214 (2018).
kg_map <- terra::rast(
  here::here(
    "./data/shapefiles/koppen_geiger/Beck_KG_V1_present_0p0083.tif"
  )
)

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format - no more PROJ4 strings! 
terra::crs(kg_map) <- "epsg:4326"
terra::crs(kg_map, describe = T)

# Plot KG layer
terra::plot(kg_map)






