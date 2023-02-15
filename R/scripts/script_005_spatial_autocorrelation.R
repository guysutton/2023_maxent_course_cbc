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
    x = reduced_pred,          # SpatRast containing reduced clim and topo layers
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
  colxy = 10:11,             # Columns containing lat/long
  n = 500,                   # Number of random occurrences
  colvar = 2:9,              # Columns containing climate variables
  max = 30000,               # Computes autocorrelation up to 30km (30000m)
  nclass = 30,               # How many points to compute correlation at (n = 30)
  nperm = 100
)
plot(spatial_corr)

# The first black dot represents 1km (1000m), and shows that there is 
# significnat spatial autocorrelation at this small distance between points. 
# - The white dots indicate no significant spatial autocorrelation when
#   points are more than 1km apart. 

# Before we spatially thin GPS records, we need to process the data
# - We need to add a column giving the species name and remove the 
#   ID column (which would otherwise be treated as a climate layer)
gps_spatial <- gps_climate %>%
  dplyr::select(-c(ID)) %>%
  dplyr::mutate(species = "Senecio madagascariensis")
head(gps_spatial)

# Thin by spatial autocorrelation value
# - Here, we are going to remove any points that are less than 1km apart 
set.seed(2012)
speciesThinned <- spThin::thin(
  loc.data = gps_spatial,  # Data.frame of lon/lat (and can have species names)
  lat.col = "y",           # Name of latitude column in `loc.data`
  long.col = "x",          # Name of longitude column in `loc.data`
  spec.col = "species",    # Name of species column in `loc.data`
  thin.par = 1,            # Remove points up to 1km apart 
  reps = 100,              # Number of times to repeat thinning (don't edit)
  max.files = 1,           # Number of CSV files to produce (don't edit)
  out.base = "gps_thinned_senecio_madagascariensis",  # Name of .csv file
  out.dir = here::here("./data/gps/spatial_thin/")    # Where to store .csv file
)

# Import the thinned GPS records
sp_gps <-
  readr::read_csv(
    here::here(
      "./data/gps/spatial_thin/gps_thinned_senecio_madagascariensis_thin1.csv"
      )
    )
head(sp_gps)

# # How many records were removed?
nrow(gps_spatial)   # No. of records before spatial thinning 
nrow(sp_gps)        # No. of records after spatial thinning 























