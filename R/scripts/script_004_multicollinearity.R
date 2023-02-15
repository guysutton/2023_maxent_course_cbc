# Script 004: Multicollinearity of climate layers 

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
# Extract climate and topographical values at focal taxon GPS points 
# -----------------------------------------------------------------------------

# Focal taxon points are stored in `sp_africa`
head(sp_africa)

# Let's store just the lon and lat columns from our GPS dataset
sp_gps <- sp_africa %>%
  dplyr::select(lon, lat)
head(sp_gps)

# Extract climate at these points 
clim_sp <- terra::extract(
  x = predictors,          # SpatRast containing climate and topo layers
  y = sp_gps               # SpatVect or data.frame containing GPS of study taxon (lon, lat)
  ) %>%
  # Remove rows where no climate or topo data is available 
  tidyr::drop_na() %>%
  # Clean the column headers 
  janitor::clean_names()
head(clim_sp)

# -----------------------------------------------------------------------------
# Remove climate variables based on R2 and VIF 
# -----------------------------------------------------------------------------

# Identify collinear variables that should be excluded (r2 > 0.7)
var_corr <- usdm::vifcor(clim_sp, th = 0.7)
var_corr

# Exclude the collinear variables that were identified in 
# the previous step using R2 > 0.7 to remove variables 
re1 <- usdm::exclude(clim_sp, var_corr) 

# Identify collinear variables that should be excluded (VIF > 10)
var_step <- usdm::vifstep(re1, th = 10)
var_step

# Subset predictors to the set of uncorrelated predictors identified above 
reduced_pred <- terra::subset(
  x = predictors,               # SpatRast containing WORLDCLIM/topo layers 
  subset = c(                   # Provide names of predictors to keep
    "wc2.1_2.5m_bio_12",
    "wc2.1_2.5m_bio_15",
    "wc2.1_2.5m_bio_3",
    "wc2.1_2.5m_bio_4",
    "wc2.1_2.5m_bio_8",
    "wc2.1_2.5m_bio_9",
    "nitrogen_0-5cm",
    "grassland"
    )
)

# Plot to make sure we kept only the uncorrelated predictors 
terra::plot(reduced_pred)














