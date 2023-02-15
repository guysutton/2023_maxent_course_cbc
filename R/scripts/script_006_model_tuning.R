# Script 006: Tune model settings 

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

# Increase system memory otherwise 'ENMevaluate' will throw and error 
# - Only works for R < 4.2.0
memory.limit(memory.limit()*2^30)

# -----------------------------------------------------------------------------
# Setup data to run model tuning experiments 
# -----------------------------------------------------------------------------

# We need a data.frame of the lon and lat (in that order) for our 
# focal taxon's GPS records
sp_gps <- sp_gps %>%
  dplyr::select(
    lon = x,
    lat = y
  )
head(sp_gps)

# We need a 'RasterStack' containing our reduced set of environmental predictors 
# - We already have this data stored in 'reduced_pred'
terra::plot(reduced_pred)

# We also need a data.frame of the lon and lat (in that order) for our background points 
bg_pts <- bg_points %>%
  dplyr::select(
    lon, 
    lat
  )
head(bg_pts)

# We need a list of the feature class (fc) and regularisation multipliers (rm) to test
list_settings <- list(
  fc = c("L","Q"),    # Should test more (e.g. c("L", "H", "Q", "LH", "LQ", "LQH"))
  rm = 1:2            # Should test more (e.g. 1:10)
) 

# -----------------------------------------------------------------------------
# Run model tuning experiments 
# -----------------------------------------------------------------------------

# Set reproducible seed
set.seed(2023)

# Run model tuning 
tuning_results <- 
  ENMeval::ENMevaluate(
    occs = sp_gps,             # Data.frame with only lon THEN lat columns
    envs = reduced_pred,       # 'spatRaster' of reduced clim and topo layers
    bg = bg_pts,               # Data.frame with lon then lat then bg predictors
    tune.args = list_settings, # Named list of FC and RM values to test 
    partitions = "block",      # Specify spatial vs random cross-validation 
    algorithm = "maxent.jar",  # Don't change!!! 
    doClamp = FALSE            # Don't change - we need to extrapolate!!! 
  )

# Store the model tuning results in a dataframe
tuning_df <- ENMeval::eval.results(tuning_results)
tuning_df

# Save model tuning results to PC
readr::write_csv(
  x = tuning_df,
  file = here::here("./models/model_tuning/model_tuning_results.csv")
)

# -----------------------------------------------------------------------------
# Visualise results 
# -----------------------------------------------------------------------------

# Plot the model tuning results
ENMeval::evalplot.stats(
  e = tuning_results,      # Variable containing ENMevaluate results 
  stats = c(               # Which metrics to plot?
    "auc.val",             # Make a plot for AUC
    "or.mtp",              # Make a plot for minimum training presence (MTP)
    "or.10p"               # Make a plot for 10th percentile omission rate (OR10)
    ),   
  color = "fc",            # Colours lines/bars by feature class (fc)
  x.var = "rm",            # Variable to plot on x-axis
  error.bars = FALSE       # Don't show error bars 
)


# -----------------------------------------------------------------------------
# Select the optimal model settings 
# -----------------------------------------------------------------------------

# Extract the model tuning results to a data.frame 
res <- ENMeval::eval.results(tuning_results)
head(res)

# Select the model settings (RM and FC) that optimised AICc (delta AICc == 0)
best_model_settings <- res %>% 
  dplyr::filter(delta.AICc == 0)
best_model_settings

# -----------------------------------------------------------------------------
# Evaluate the best model  
# -----------------------------------------------------------------------------

# Let's evaluate the best model 
mod_best <- eval.models(tuning_results)[[best_model_settings$tune.args]]

# Plot the marginal response curves for the predictor variables wit non-zero 
# coefficients in our model. We define the y-axis to be the cloglog transformation, 
# which is an approximation of occurrence probability (with assumptions) 
# bounded by 0 and 1 (Phillips et al. 2017).
dismo::response(eval.models(tuning_results)[[best_model_settings$tune.args]])

# You interpret these graphs to see if the relationship between your study 
# species being present at a site correlates with the environmental variables, 
# and whether the shape of the relationship makes sense for the species

# For example, if we consider bio12 (mean annual precipitation),
# - We can see that there is a relatively weak effect on our study species,
#   with the species less likely to be recorded with increasing 
#   mean annual precipitation

# Another example, if we consider bio15 (cv of seasonality precipitation),
# or how variable precipitation is between seasons,
# - We can see that the suitability for the species is very high when rainfall
#   is not very variable between seasons (x-axis between 0 and 40), 
#   and as the variation in rainfall between seasons increases 
#  (larger x-axis values), the suitability for our study species decreases.
#   - This would imply that our study species likes consistent rainfall
#     (or a lack of rainfall) throughout the year


# -----------------------------------------------------------------------------
# Null models
# -----------------------------------------------------------------------------

# We first run the null simulations with 100 iterations to get a reasonable 
# null distribution for comparisons with the empirical values
# - The null distribution represents random model performance
# - The empirical value represents the model we fit 
mod_null <-
  ENMeval::ENMnulls(
    e = tuning_results,                         # Variable containing ENMevaluate object
    mod.settings = list(fc = "Q", rm = 2),      # Optimal model settings 
    no.iter = 100
    )
mod_null

# We can make plots of the null model results as a histogram
# - We ideally want to see our empirical model performance being better 
#   than the null distribution
ENMeval::evalplot.nulls(
  mod_null, 
  stats = c(
    "auc.val",
    "or.10p" 
    ), 
  plot.type = "histogram"
  )





