# Script 005: Spatial autocorrelation

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
# Setup data to run spatial autocorrelation analysis 
# -----------------------------------------------------------------------------

# Extract climate data at focal taxon study points 
gps_climate <- terra::extract(
    x = reduced_pred,          # SpatRast containing reduced WORLDCLIM layers
    y = sp_gps,                # SpatVect or data.frame containing GPS of study taxon (lon, lat)
    xy = TRUE                  # Return lon and lat columns for each GPS point 
  )
head(gps_climate)

# -----------------------------------------------------------------------------
# Run spatial autocorrelation analysis 
# -----------------------------------------------------------------------------

# At what distance does spatial autocorrelation occur?
# - The x-axis represents meters 
# - The citation for this method:
#   - Legendre, P. and M.J. Fortin. 1989. Spatial pattern and ecological analysis. 
#     Vegetation, 80, 107-138.
spatial_corr <- ecospat::ecospat.mantel.correlogram(
  dfvar = gps_climate,       # Data frame with environmental variables
  colxy = 7:8,               # Columns containing lat/long
  n = 500,                   # Number of random occurrences
  colvar = 1:6,              # Columns containing climate variables
  max = 30000,               # Computes autocorrelation up to 30km (30000m)
  nclass = 30,               # How many points to compute correlation at 
  nperm = 100
)
plot(spatial_corr)

# The first white dot represents 1km (1000m), and shows that there is no spatial autocorrelation
# at this small distance between points. 
# - Nothing to worry about here 


# Thin by spatial autocorrelation value
#speciesThinned <- spThin::thin(
#  loc.data = species,
#  lat.col = "lat",
#  long.col = "lon",
#  spec.col = "species",
#  # Km unit of correlogram
#  thin.par = 0.2,
#  reps = 100,
#  max.files = 1,
#  out.dir = here::here("./data/data_clean/")
#)

# # Import the thinned GPS records
# speciesThinned <-
#   readr::read_csv(here::here("./data/data_clean/dasi_rubi_native_thinned.csv"))
# head(speciesThinned)
# 
# # How many records were removed?
# nrow(species)
# nrow(speciesThinned)
# 
# # Just for ease, make species_thinned = species
# species <- speciesThinned






















