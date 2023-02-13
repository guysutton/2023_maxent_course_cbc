# Script 002: Download or import species GPS data 

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
# Download species GPS data from GBIF 
# -----------------------------------------------------------------------------

# Below, we will download GPS data from GBIF for Senecio madagascariensis,
# which is a small herb that is native to South Africa and Madgascar, 
# but which has become invasive in Australia and Hawaii
# - We can download records from GBIF, or import GPS records from a .csv 
#   file that we have stored on our PC somewhere 
# - Pick the option that works for you 

# Option #1: Download species occurrences (GPS) from GBIF 
# set.seed(2012)
# sp_gps <- geodata::sp_occurrence(
#   genus = "Senecio",
#   species = "madagascariensis",
#   download = TRUE,
#   geo = TRUE,
#   removeZeros = TRUE,
#   nrecs = 2000    # Only download 2000 GPS - remove this for a proper analysis
# )
# head(sp_gps)

# Option #2: Alternatively, we could import a csv file containing GPS data 
sp_gps <- readr::read_csv("./data/gps/senecio_madagascariensis_world.csv")
head(sp_gps)

# It doesn't matter what option we chose above, the code below works for 
# GBIF gps data or csv GPS data (as long as your .csv file is formatted 
# like mine)

# Let's just keep the columns of data that we will use going forward 
sp_data <- sp_gps %>%
  dplyr::select(
    species,
    lon,
    lat, 
    country
  )
head(sp_data)

# Remove duplicate GPS data 
sp_data <- sp_data %>%
  dplyr::distinct(lon, lat, .keep_all= TRUE)
head(sp_data)

# Get world map 
world_map <- rnaturalearth::ne_countries(
  scale = "medium", 
  returnclass = "sf"
) 

# Plot GPS points on world map to check our locality data is correct 
ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map) +
  # Add GPS points 
  geom_point(
    data = sp_data, 
    aes(
      x = lon, 
      y = lat
    )
  )  +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  )

# Let's just keep the GPS records from South Africa and Madagascar to build 
# our climate model 
sp_africa <- sp_data %>%
  dplyr::filter(
    country %in% c("South Africa", "Madagascar")
  )

# Replot the GPS points on world map to see that we kept the right GPS data 
ggplot() +
  # Add raster layer of world map 
  geom_sf(data = world_map) +
  # Add GPS points 
  geom_point(
    data = sp_africa, 
    aes(
      x = lon, 
      y = lat
    )
  )  +
  # Set world map CRS 
  coord_sf(
    crs = 4326,
    expand = FALSE
  )




























