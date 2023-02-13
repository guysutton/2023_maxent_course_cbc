# Script 007: Fit MaxEnt model 

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
# Setup data to fit MaxEnt models 
# -----------------------------------------------------------------------------



# We need a data.frame with columns containing climate data for each study species
# GPS point 
clim_species <- terra::extract(
  x = reduced_pred,          # SpatRast containing reduced WORLDCLIM layers
  y = sp_gps,                # SpatVect or data.frame containing GPS of background points (lon, lat)
  xy = FALSE                 # Don't need lon and lat columns for each GPS point 
)
head(clim_species)
  

# We need a data.frame with columns containing climate data for each background point
clim_bg <- terra::extract(
  x = reduced_pred,          # SpatRast containing reduced WORLDCLIM layers
  y = bg_pts,                # SpatVect or data.frame containing GPS of background points (lon, lat)
  xy = FALSE                 # Don't need lon and lat columns for each GPS point 
)
head(clim_bg)

# Combine the climate data for the focal species and background points 
data <- dplyr::bind_rows(
  clim_species,
  clim_bg
  ) %>%
  dplyr::select(-c(ID))
head(data)

# Provide a vector containing 0 (indicating background points) and 1 (indicating 
# presence points)
p_vector <- c(
  replicate(nrow(clim_species), "1"),
  replicate(nrow(clim_bg), "0")
) 

# -----------------------------------------------------------------------------
# Fit MaxEnt model 
# -----------------------------------------------------------------------------

# Fit MaxEnt model with optimal model settings configurations 
mod1 <- dismo::maxent(
  x = data,
  p = p_vector,
  path = here::here("./models/optimal_model_senecio_madgascariensis"),
  replicates = 10,
  args = c(
    'betamultiplier=1.0',
    'linear=true',
    'quadratic=true',
    'product=true',
    'threshold=false',
    'hinge=true',
    'threads=2',
    #'doclamp=true',
    'fadebyclamping=true',
    'responsecurves=true',
    'jackknife=true',
    'askoverwrite=false',
    'responsecurves=true',
    'writemess=true',
    'writeplotdata=true',
    'writebackgroundpredictions=true'
  )
)

# -----------------------------------------------------------------------------
# Plot map - Default settings
# -----------------------------------------------------------------------------

# Get map of Australia to project our model over
aus_ext <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf") %>%
  dplyr::filter(name == "Australia")

# Mask reduced set of WORLDCLIM layers to the extent of Australia
aus_map <- terra::mask(reduced_pred, aus_ext)

# Extract MaxEnt predictions for Australia 
predict_maxent <- terra::predict(mod1, aus_map)
terra::plot(predict_maxent)

# MaxEnt scores are in a raster layer above, but we need the MaxEnt scores in a dataframe 
# - Below, create data.frame of MaxEnt model projection/scores
df <- terra::as.data.frame(predict_maxent, xy = TRUE) %>%
  dplyr::select(
    lon = x,
    lat = y,
    maxent_score = maxent
  )
head(df)

ggplot() +
  # Plot the shapefile of Australia
  geom_sf(data = aus_ext, fill = NA) +
  # Plot MaxEnt scores
  geom_tile(data = df, aes(x = lon,
                           y = lat,
                           fill = maxent_score)) +
  # Colour each grid cell according the MaxEnt scores
  # Scores close to 1 = High climatic suitability (good)
  # Scores close to 0 = Low climatic suitability (bad)
  scale_fill_gradientn(
    colours = c("white", "blue", "lightgreen",
                "yellow", "orange", "red"),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
    limits = c(0, 1)
  ) +
  # Crops map to just the geographic extent of Australia
  coord_sf(
    xlim = c(110, 155),
    ylim = c(-45, -8),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "Climatic similarity"
    ) +
  # Add scale bar to bottom-right of map
  annotation_scale(
    location = "bl",          # 'bl' = bottom left
    style = "ticks",
    width_hint = 0.2
    ) +
  # Add north arrow
  annotation_north_arrow(
    location = "bl",
    which_north = "true",
    pad_x = unit(0.325, "in"),
    pad_y = unit(0.3, "in"),
    style = north_arrow_fancy_orienteering
  ) +
  # Apply the theme for the map we defined above at the start of the script 
  theme_opts +
  theme(legend.position = "right") +
  # Change appearance of the legend
  guides(
    fill = guide_colorbar(ticks = FALSE),
    colour = guide_legend(order = 2)
    )

# Save a high quality map to your PC
ggsave(
  "./models/optimal_model_senecio_madgascariensis/figures/map_aus_senecio_madgascariensis.png",
  width = 8,
  height = 6,
  dpi = 600
)











