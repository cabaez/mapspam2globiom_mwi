#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to process FAOSTAT crops data
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "countrycode")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# Set FAOSTAT versions
faostat_crops_version <- "20200303"

# Crop production
prod_raw <- read_csv(file.path(glob_raw_path, glue("faostat/{faostat_crops_version}_faostat_crops.csv")))

# faostat2crop
faostat2crop <- read_excel(file.path(mappings_path, "mappings_spam.xlsx"), sheet = "faostat2crop") %>%
  dplyr::select(crop, faostat_crop_code) %>%
  na.omit()


############### PROCESS ###############
# Extract harvested area data
area <- prod_raw %>%
  filter(`Area Code` == fao_sel, Element ==  "Area harvested", Unit == "ha") %>%
  dplyr::select(faostat_crop_code = `Item Code`, year = Year, unit = Unit, value = Value) %>%
  left_join(., faostat2crop) %>%
  filter(!is.na(value)) %>%
  na.omit() %>%# remove rows with na values for value
  group_by(crop, unit, year) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(source = "FAOSTAT",
         adm_level = 0,
         adm_code = iso3c_sel,
         adm_name = country_sel)
summary(area)
str(area)


########## SAVE ##########
temp_path <- file.path(proc_path, paste0("agricultural_statistics"))
dir.create(temp_path, recursive = T, showWarnings = F)
write_csv(area, file.path(temp_path, glue("faostat_crops_{year_sel}_{iso3c_sel}.csv")))


########## CREATE FAOSTAT CROP LIST ##########
temp_path <- file.path(proc_path, "lists")
dir.create(temp_path, showWarnings = F, recursive = T)

faostat_crop_list <- area %>%
  dplyr::select(source, adm_code, adm_name, crop) %>%
  unique() %>%
  arrange(crop)

write_csv(faostat_crop_list, file.path(temp_path, glue("faostat_crop_list_{year_sel}_{iso3c_sel}.csv")))


########## CLEAN UP ##########  
rm(temp_path, area, faostat_crop_list, faostat2crop, prod_raw, faostat_crops_version)
