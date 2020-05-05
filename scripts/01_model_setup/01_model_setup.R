#'========================================================================================================================================
#' Project:  mapspam2globiom_mwi
#' Subject:  Setup SPAM
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SETUP R ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "here", "gdalUtils", "glue", "raster", "readxl", "tidyverse", "sf")

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### SETUP SPAMc ###############
# Set the folder where the model will be stored
# Note that R uses forward slashes even in Windows!!
spamc_path <- "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi"

# Set SPAMc parameters
param <- spam_par(spam_path = spamc_path,
                         iso3c = "MWI",
                         year = 2010,
                         res = "5min",
                         adm_level = 2,
                         solve_level = 1,
                         model = "max_score")

# Show parameters
print(param)

# Create SPAMc folder structure in the spamc_path
create_spam_folders(param)

