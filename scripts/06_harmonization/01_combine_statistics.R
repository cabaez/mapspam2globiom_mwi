#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to combine various statistics
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### LOAD DATA ###############
load_data(c("adm_list", "ha", "fs", "ci"), param)


############### PROCESS HARVESTED AREA ###############
# wide to long format
ha <- ha %>% 
  gather(crop, ha, -adm_name, -adm_code, -adm_level)

# Set -999 and empty string values
ha <- ha %>%
  mutate(ha = if_else(ha == -999, NA_real_, ha))

# filter out crops which values are all zero or NA
crop_na_0 <- ha %>%
  group_by(crop) %>%
  filter(all(ha %in% c(0, NA))) %>%
  dplyr::select(crop) %>%
  unique

ha <- ha %>%
  filter(!crop %in% crop_na_0$crop)

# Remove lower level adm data if it would somehow not be used 
ha <- ha %>%
  filter(adm_level <= param$adm_level)


########## PROCESS FARMING SYSTEM SHARES ##########
# wide to long format
fs <- fs %>% 
  gather(crop, fs, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
fs <- fs %>%
  mutate(fs = if_else(fs == -999, NA_real_, fs))

# Select relevent crops using ha
fs <- fs %>%
  filter(crop %in% unique(ha$crop))


########## CROPPING INTENSITY ##########
# wide to long format
ci <- ci %>% 
  gather(crop, ci, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
ci <- ci %>%
  mutate(ci = if_else(ci == -999, NA_real_, ci))

# Select relevent crops using ha
ci <- ci %>%
  filter(crop %in% unique(ha$crop))


########## CONSISTENCY CHECKS ##########

# Format checks, numerical values and code is character
# consistency within data: do they add up and are lower level totals not higher than higher level ADMs
# Check if artifical ADMs area is near zero.
# Do shares of farming systems add up.
# Is ADM system the same as the map
# Are crops the same in ci, fs and ha data files. Not possible to have ADM0 data and NA for fs and ci.


############### SET ADM IN LINE WITH SOLVE_SEL ###############
prepare_pa_stat(ha, fs, ci, param)
