#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Process adm shapefile
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# set name file
iso3c_shp <- "adm_2010_MWI.shp"

# load shapefile
adm_raw <- read_sf(file.path(spam_par$spam_path, glue("raw_data/adm/{iso3c_shp}")))

# plot
plot(adm_raw$geometry)


############### PROCESS ###############
# Project to standard global projection
adm <- adm_raw %>%
  st_transform(spam_par$crs)

# Check names
head(adm)
names(adm)

# Change names In order to use the country polygon as input, the column names of
# the attribute table have to be set. 
# The names of the administrative units should be set to admX_name, where X is the adm level.
# The codes of the administrative units should be set to admX_code, where X is the adm code.

# Set the original names, i.e. the ones that will be replaced. Remove adm1
# and/or adm2 entries if such data is not available.
adm0_name_orig <- "ADM0_NAME"
adm0_code_orig <- "FIPS0"
adm1_name_orig <- "ADM1_NAME"
adm1_code_orig <- "FIPS1"
adm2_name_orig <- "ADM2_NAME"
adm2_code_orig <- "FIPS2"

# Replace the names
names(adm)[names(adm) == adm0_name_orig] <- "adm0_name"
names(adm)[names(adm) == adm0_code_orig] <- "adm0_code"
names(adm)[names(adm) == adm1_name_orig] <- "adm1_name"
names(adm)[names(adm) == adm1_code_orig] <- "adm1_code"
names(adm)[names(adm) == adm2_name_orig] <- "adm2_name"
names(adm)[names(adm) == adm2_code_orig] <- "adm2_code"

# Only select relevant columns
adm <- adm %>%
  dplyr::select(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)

# Check names
head(adm)
names(adm)

# Union separate polygons that belong to the same adm    
adm <- adm %>%
  group_by(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarize() %>%
  ungroup() %>%
  mutate(adm0_name = spam_par$country,
         adm0_code = spam_par$iso3c)

par(mfrow=c(1,2))
plot(adm$geometry, main = "ADM all polygons")

# Set names of ADMs that need to be removed from the polygon. 
# These are ADMs where no crop should be allocated. Here we remove 
# Area under National Administration, which is the part of Lake Malawi that belongs to Malawi
# and Likoma, several small islands in the lake that are not covered by the statistics.
# Set the adm_name by ADM level which need to be removed. Otherwise remove the script.
adm1_to_remove <- c("Area under National Administration")
adm2_to_remove <- c("Likoma")

# Remove ADM1s
adm <- adm %>%
  filter(adm1_name != adm1_to_remove) %>%
  filter(adm2_name != adm2_to_remove)

plot(adm$geometry, main = "ADM polygons removed")
par(mfrow=c(1,1))


############### SAVE ###############
# Create adm_map_list
adm_list <- adm %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

write_csv(adm_list, file.path(spam_par$spam_path, glue("processed_data/lists/adm_list_{spam_par$year}_{spam_par$iso3c}.csv")))

# Save maps in .rds and shapefile format
saveRDS(adm, file.path(spam_par$spam_path, glue("processed_data/maps/adm/adm_{spam_par$year}_{spam_par$iso3c}.rds")))
write_sf(adm, file.path(spam_par$spam_path, glue("processed_data/maps/adm/adm_{spam_par$year}_{spam_par$iso3c}.shp")))


############### CLEAN UP ###############
rm(adm, adm_raw, adm_list, adm0_code_orig, adm0_name_orig, adm1_code_orig, adm1_name_orig,
   adm2_code_orig, adm2_name_orig, adm1_to_remove, adm2_to_remove, iso3c_shp)

