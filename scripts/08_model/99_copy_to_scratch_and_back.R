#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to copy GAMS to scratch for running on server
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### SOURCE FUNCTIONS
source(file.path(root, "Code/general/support_functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio



### COPY FILES TO SERVER
# spam_2.0
spam_files <- list.files(spam_path, full.names = T, pattern = paste(glob2rx("*.gms"), glob2rx("*.gpr"), sep = "|" ))
input_file <- list.files(file.path(spam_path, "input"), full.names = T, pattern = glob2rx("*.gdx")) 

# List of files to be copied
on_drive <- c(spam_files, input_file)

# Create list of files and destination path
on_server <- on_drive
on_server <- gsub(spam_path, scratch_path, on_server)

# Copy files and create folder if needed
walk(seq_along(on_drive), function(x){
  message(basename(on_drive[x]))
  dir.create(dirname(on_server[x]),recursive = T, showWarnings = F)
  file.copy(on_drive[x], on_server[x], overwrite = T)
})

# Create output folder on server
spam_output_path <- file.path(scratch_path, paste0("/output"))
dir.create(spam_output_path, recursive = T, showWarnings = F)

# Clean up
rm(on_drive, on_server)


#############################################################################################################
# RUN SPAM_2.0 FROM SERVER
#############################################################################################################


### COPY FILES FROM SERVER
# We need the log and the results in the output folder
output_file <- list.files(file.path(scratch_path, "output"), full.names = T, pattern = glob2rx("*.gdx"))
log_file <- list.files(file.path(scratch_path), full.names = T, pattern = glob2rx("*.log"))

# list of files to be copied
on_server <- c(output_file, log_file)


# Create list of files
on_drive <- on_server
on_drive <- gsub(scratch_path, spam_path, on_server)

# Copy files and create path
walk(seq_along(on_server), function(x){
  base <- basename(on_server[x])
  message(base)
  file.copy(on_server[x], on_drive[x], overwrite = T)
})
