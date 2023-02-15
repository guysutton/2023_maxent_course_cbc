# Script 001: Download and visualise environmental layers (rasters)

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
  InformationValue,
  caret, 
  terra,
  tidyterra,
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
# Download climate raster layers 
# -----------------------------------------------------------------------------

# Download the WORLDCLIM raster layers for current time period to your PC
# - This will download and store all 19 WORLDCLIM layers to a folder
#   of your choice (given using 'path = ...' below)
# - Raster layers are stored as 'SpatRaster' so they are compatible with the 
#   'terra' R package 

# -------- Uncomment this code to download WORLDCLIM layers -----------

# # Create a directory to store climate data
# dir.create("./data/environmental_layers/current",
#            recursive = TRUE)
# 
# # Download climate layers 
# wc_current <- geodata::worldclim_global(
#   var = "bio",
#   res = 2.5,      # Minute degree resolution of raster layers
#   path = here::here("./data/environmental_layers/current/"),
#   version = "2.1"
#   )

# ----------------------------------------------------------------------

# Load the WORLDCLIM rasters layers we already have downloaded
# - We don't need to run the download code above each new R session
pred_climate <- terra::rast(list.files(
  here::here("./data/environmental_layers/current/wc2.1_2.5m/"),
  full.names = TRUE,
  pattern = '.tif'
  )
)

# Plot each of the 19 WORLDCLIM layers to check they imported correctly 
# terra::plot(pred_climate)

# Plot the first layer only (bio1)
# - 'bio1' is mean annual temperature 
terra::plot(pred_climate[[1]])

# Set the CRS projection for the current climate layers 
# - Use the correct wkt CRS format - no more PROJ4 strings! 
terra::crs(pred_climate) <- "epsg:4326"
terra::crs(pred_climate, describe = T)

# -----------------------------------------------------------------------------
# Download topographical raster layers 
# -----------------------------------------------------------------------------

# We can download/import any type of data, as last we can extract values 
# at GPS points (ideally, any information that we can coerce into a 
# raster layer can be used in the modelling process)
# - We aren't limited to just climate 

# For example, we can download quantitative or qualitative data for 
# other topographical variables

# # Create a directory to store climate data
dir.create("./data/topographical_layers/current",
           recursive = TRUE)

###################################
# - Download total nitrogen in soil 
###################################

# Download total nitrogen (N) in the soil
# - We set the depth at 5cm into the soil
# pred_totaln <- geodata::soil_world(
#   var = "nitrogen",
#   # Which variable do we want?
#   depth = 5,
#   # Soil depth (cm)
#   stat = "mean",
#   # Return mean values (could get variance, CI's, ect...)
#   path = here::here("./data/topographical_layers/current/")
# )

# Load the totaln layer (if already downloaded)
pred_totaln <- terra::rast(x = here::here(
  "./data/topographical_layers/current/nitrogen_0-5cm_mean_30s.tif"
  )
)

# Set the CRS projection for the total N layer
terra::crs(pred_totaln) <- "epsg:4326"
terra::crs(pred_totaln, describe = T)

# Plot 'Total Nitrogen' raster layer
terra::plot(pred_totaln)

###################################
# - Download grassland cover ------ 
###################################

# Download grassland cover 
# pred_grass <- geodata::landcover(
#   var = "grassland", # Which variable do we want? 
#   path = here::here("./data/topographical_layers/current/")
# )

# Load the grassland layer (if already downloaded)
pred_grass <- terra::rast(x = here::here(
  "./data/topographical_layers/current/WorldCover_grassland_30s.tif"
  )
)

# Set the CRS projection for the grassland cover layer
terra::crs(pred_grass) <- "epsg:4326"
terra::crs(pred_grass, describe = T)

# Plot 'grassland cover' raster layer
terra::plot(pred_grass)

# -----------------------------------------------------------------------------
# Combine all climate and topographic rasters into one variable 
# -----------------------------------------------------------------------------

# Check if all the rasters are in the same spatial resolution
# - We downloaded climate at 2.5 minute resolution and topographical
#   layers at 0.5 minute (30 second) resolution 
terra::res(pred_climate)
terra::res(pred_totaln)
terra::res(pred_grass)

# Convert resolutions to 2.5 minute
# - Why? 
pred_totaln <- terra::aggregate(pred_totaln, 5, mean)
pred_totaln <- terra::resample(pred_totaln, pred_climate)
terra::plot(pred_totaln)

pred_grass <- terra::aggregate(pred_grass, 5, mean)
pred_grass <- terra::resample(pred_grass, pred_climate)
terra::plot(pred_grass)

# Check if all the rasters are in the same resolution
terra::res(pred_climate)
terra::res(pred_totaln)
terra::res(pred_grass)

# Combine rasters into a single variable 
# - We have to store the rasters in a list first and then
#   pass the list to get rasterised 
raster_list <- list(
  pred_climate,
  pred_totaln,
  pred_grass
  )
predictors <- terra::rast(raster_list) 

# Check the variable contains all 21 variables 
terra::nlyr(predictors)
terra::plot(predictors)








