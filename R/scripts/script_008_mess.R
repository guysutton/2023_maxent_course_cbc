# Script 008: MESS

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
# Setup data to run MESS analysis
# -----------------------------------------------------------------------------

# We need a Raster layer object containing the reduced predictors
# - These are already available in 'reduced_pred', but needs to be
#   convered into a RasterStack
pred_layers <- raster::stack(reduced_pred)
class(pred_layers)
nlayers(pred_layers)

# We also need a data.frame containing only the predictor data for the 
# GPS points used to calibrate the model 
# - This is already available in 'clim_species'
ref_pts <- clim_species %>%
  dplyr::select(-c(ID))
head(ref_pts)

# We need to make sure that the number of layers in the rasterstack 
# is equal to the number of columns in the reference points 
nlayers(pred_layers) == ncol(ref_pts)

# -----------------------------------------------------------------------------
# Run MESS analysis
# -----------------------------------------------------------------------------

# Run MESS analysis
mss <- dismo::mess(
  x = pred_layers,
  v = ref_pts,
  full = FALSE
  )

# Check MESS output 
terra::plot(mss)


# -----------------------------------------------------------------------------
# Plot MESS map
# -----------------------------------------------------------------------------

# Convert mss to spatRast
mess_spat <- terra::rast(mss)
terra::plot(mess_spat)

# Reclassify raster values into categories
m <- c(
  -1000, 0, 0,    # Values between -1000 and -1 become 0
  0, 1000, 1      # Values between 0 and 1000 become 1
  )
rclmat <- matrix(m, ncol=3, byrow = TRUE)
mess_spat <- terra::classify(mess_spat, rclmat, include.lowest = TRUE)
mess_spat

# Plot MESS layer
# - MESS < 0 indicates extrapolation
# - MESS > 0 indicates interpolation
# - We must be cautious about interpretating predictions 
#   in extrapolation 
ggplot() +
  # Plot Australia boundary
  geom_sf(data = aus_ext, fill = NA) +
  # Plot MaxEnt prediction raster
  geom_spatraster(
    data = mess_spat,
    maxcell = 5e+7      # Change to maxcell = Inf for publication-quality
  ) +
  # Control raster colour and legend
  scale_fill_whitebox_c(
    palette = "muted"
  ) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "MESS"
  ) +
  # Crops map to just the geographic extent of Australia
  coord_sf(
    xlim = c(110, 155),
    ylim = c(-45, -8),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  theme(legend.position = "right") +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE)
  )

# Save figure to PC
ggsave(
  "./models/optimal_model_senecio_madagascariensis/mess_prediction.png",
  dpi = 600,
  height = 6,
  width = 6
)










