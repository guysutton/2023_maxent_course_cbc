


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

# Run MESS analysis
mss <- dismo::mess(
  x = pred_layers,
  v = ref_pts,
  full = FALSE
  )

# Plot MESS layer
# - MESS < 0 indicates extrapolation
# - MESS > 0 indicates interpolation
# - We must be cautious about interpretating predictions 
#   in extrapolation 
terra::plot(mss)
plot(mss)

# Convert mss to spatRast
mess_spat <- terra::rast(mss)
terra::plot(mess_spat)

# Plot MESS map   
ggplot() +
  # Plot Australia boundary
  geom_sf(data = aus_ext, fill = NA) +
  # Plot MaxEnt prediction raster
  geom_spatraster(
    data = mess_spat,
    maxcell = 5e+6         # maxcell = Inf
  ) +
  # Control raster colour and legend
  scale_fill_whitebox_c(
    palette = "muted",
    breaks = seq(-600, 100, 100),
    limits = c(-600, 100)
  ) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Crops map to just the geographic extent of Australia
  coord_sf(
    xlim = c(110, 155),
    ylim = c(-45, -8),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  theme(legend.position = "right")

# Convert mss to spatRast
mess_spat <- mess_spat > 0
terra::plot(mess_spat)

factor <- mess_spat %>% 
  dplyr::mutate(mess = cut(
  mess,
  breaks = c(0),
  labels = c("Extrapolation", "Interpolation"")
             


# Plot MESS map (binary)  
ggplot() +
  # Plot Australia boundary
  geom_sf(data = aus_ext, fill = NA) +
  # Plot MaxEnt prediction raster
  geom_spatraster(
    data = mess_spat,
    maxcell = 5e+6         # maxcell = Inf
  ) +
  # Control raster colour and legend
  scale_fill_whitebox_d(
    palette = "muted",
    breaks = c(FALSE, TRUE),
    labels = c("Extrapolation", "Interpolation")
  ) +
  # Control axis and legend labels 
  labs(
    x = "Longitude",
    y = "Latitude",
    fill = "P(suitability)"
  ) +
  # Crops map to just the geographic extent of Australia
  coord_sf(
    xlim = c(110, 155),
    ylim = c(-45, -8),
    crs = 4326,
    expand = FALSE
  ) +
  # Create title for the legend
  theme(legend.position = "right")













