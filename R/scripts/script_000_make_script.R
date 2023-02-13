# Make script to automate the running of all R scripts required to replicate the MaxEnt analysis 

# Source files to run 
source("./R/scripts/script_001_download_worldclim_layers.R")
source("./R/scripts/script_002_download_import_species_gps.R")
source("./R/scripts/script_003_background_points.R")
source("./R/scripts/script_004_multicollinearity.R")