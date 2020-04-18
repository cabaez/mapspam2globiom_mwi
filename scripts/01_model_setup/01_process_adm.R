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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### USER INPUT ###############
# set name file
iso3c_shp <- "adm_2010_MWI.shp"

# Add correct header names.
# To process the shapefile the header of the columns with the adm name and adm code needs to be set. 
# Note that it is no problem if the attribute table of the shapefile contains other information that 
# the adm name and code. This will be ignored.

# Add info from adm0 to adm_sel, so if adm_sel = 1, only adm0 and adm1 info.
adm0_name_orig <- "ADM0_NAME"
adm0_code_orig <- "FIPS0"
adm1_name_orig <- "ADM1_NAME"
adm1_code_orig <- "FIPS1"
adm2_name_orig <- "ADM2_NAME"
adm2_code_orig <- "FIPS2"

# Set names of ADMs that need to be removed from the polygon. 
# These are ADMs where no crop should be allocated. Here we remove 
# Area under National Administration, which is the part of Lake Malawi that belongs to Malawi
# and Likoma, several small islands in the lake that are not covered by the statistics.
# Set the adm_name by ADM level which need to be removed. Otherwise set to NULL.
# adm1_to_remove <- NULL
# adm2_to_remove <- NULL
adm1_to_remove <- c("Area under National Administration")
adm2_to_remove <- c("Likoma")


############### LOAD DATA ###############
# load shapefile
adm_raw <- read_sf(file.path(raw_path, glue("adm/{iso3c_shp}")))

# plot
plot(adm_raw$geometry)


############### PROCESS ###############
# Project to standard global projection
crs_sel <- "+proj=longlat +datum=WGS84 +no_defs"
adm <- adm_raw %>%
  st_transform(crs_sel)

# Check names
head(adm)
names(adm)

# Correct names and union polygons
if(adm_sel == 0){
  names(adm)[names(adm) == adm0_name_orig] <- "adm0_name"
  names(adm)[names(adm) == adm0_code_orig] <- "adm0_code"

  adm <- adm %>%
    group_by(adm0_name, adm0_code) %>%
    summarize() %>%
    ungroup() %>%
    mutate(adm0 = country_sel,
           adm0_code = iso3c_sel)
  plot(adm$geometry)
  
} else if(adm_sel == 1) {
  names(adm)[names(adm) == adm0_name_orig] <- "adm0_name"
  names(adm)[names(adm) == adm0_code_orig] <- "adm0_code"
  names(adm)[names(adm) == adm1_name_orig] <- "adm1_name"
  names(adm)[names(adm) == adm1_code_orig] <- "adm1_code"
  
  adm <- adm %>%
    group_by(adm0_name, adm0_code, adm1_name, adm1_code) %>%
    summarize() %>%
    ungroup() %>%
    mutate(adm0_name = country_sel,
           adm0_code = iso3c_sel)
  plot(adm$geometry)
  
  } else if(adm_sel == 2){
    names(adm)[names(adm) == adm0_name_orig] <- "adm0_name"
    names(adm)[names(adm) == adm0_code_orig] <- "adm0_code"
    names(adm)[names(adm) == adm1_name_orig] <- "adm1_name"
    names(adm)[names(adm) == adm1_code_orig] <- "adm1_code"
    names(adm)[names(adm) == adm2_name_orig] <- "adm2_name"
    names(adm)[names(adm) == adm2_code_orig] <- "adm2_code"
    
    adm <- adm %>%
      group_by(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code) %>%
      summarize() %>%
      ungroup() %>%
      mutate(adm0_name = country_sel,
             adm0_code = iso3c_sel)
    plot(adm$geometry)
}

# check
names(adm)
head(adm)
par(mfrow=c(1,2))
plot(adm$geometry, main = "ADM all polygons")


############### REMOVE SELECTED ADMS ################
# Remove ADM1s
if(!is.null(adm1_to_remove)){
  adm <- adm %>%
    filter(adm1_name != adm1_to_remove)
  message(glue("{adm1_to_remove} is removed"))
}

if(!is.null(adm2_to_remove)){
  adm <- adm %>%
    filter(adm2_name != adm2_to_remove)
  message(glue("{adm2_to_remove} is removed"))
}

plot(adm$geometry, main = "ADM polygons removed")
par(mfrow=c(1,1))


############### SAVE ###############
adm_path <- file.path(proc_path, "/maps/adm")
dir.create(adm_path, recursive = T, showWarnings = F)

# Create adm_map_list
adm_map_list <- adm %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

# Save
temp_path <- file.path(proc_path, paste0("lists"))
dir.create(temp_path, recursive = T, showWarnings = F)

write_csv(adm_map_list, file.path(temp_path, glue("adm_map_list_{year_sel}_{iso3c_sel}.csv")))

# Save maps in .rds and shapefile format
saveRDS(adm, file.path(adm_path, glue("adm_{year_sel}_{iso3c_sel}.rds")))
write_sf(adm, file.path(adm_path, glue("adm_{year_sel}_{iso3c_sel}.shp")))


############### CLEAN UP ###############
rm(temp_path, adm, adm_path, adm_map_list, adm_raw, adm0_code_orig, adm0_name_orig, adm1_code_orig, adm1_name_orig,
   adm2_code_orig, adm2_name_orig, adm1_to_remove, adm2_to_remove, iso3c_shp)

