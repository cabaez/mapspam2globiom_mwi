#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to process aquastat irrigation data
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# Set aquastat version
aquastat_version <- "20200303"

# Aquastat raw
aquastat_raw <- read_excel(file.path(glob_raw_path, glue("aquastat/{aquastat_version}_aquastat_irrigation.xlsx")), sheet = "data")

# Crop mapping
aquastat2crop <-  read_excel(file.path(mappings_path, "mappings_spam.xlsx"), sheet = "aquastat2crop")


############### PROCESS ###############
# Clean up database
aquastat <- aquastat_raw %>%
  filter(`Area Id` == fao_sel) %>%
  mutate(adm_code = iso3c_sel,
         adm_name = country_sel,
         adm_level = 0) %>%
  transmute(adm_name, adm_code, adm_level, variable = `Variable Name`, variable_code = `Variable Id`, year = Year, value = Value)

# Create irrigated area df
# Note that "Total harvested irrigated crop area (full control irrigation)" (4379) is only presented if all crops are included
ir_area <- aquastat %>%
  dplyr::filter(grepl("Harvested irrigated temporary crop area", variable)|
                grepl("Harvested irrigated permanent crop area", variable)|
                variable_code %in% c(4379, 4313)) %>%
  separate(variable, c("variable", "aquastat_crop"), sep = ":") %>%
  mutate(aquastat_crop = trimws(aquastat_crop),
         aquastat_crop = ifelse(is.na(aquastat_crop), "Total", aquastat_crop),
         aquastat_crop = ifelse(aquastat_crop == "total", "Total", aquastat_crop),
         value = value * 1000) # to ha

# Map to crops
ir_area <- ir_area %>%
  left_join(aquastat2crop) %>%
  group_by(adm_name, adm_code, adm_level, variable, year, crop) %>%
  summarize(value = sum(value, na.rm = T),
            aquastat_crop = paste(aquastat_crop, collapse = ", ")) %>%
  mutate(system = "I") %>%
  filter(crop != "REMOVE") # removes fodder


########## USER INPUT ##########
# AQUASTAT uses "Other fruits" as a category, which can either be mapped to 
# tropical fruits (trof) or temperate fruits (temf) in mapspam. 
# The standard is to map it to trof. If this is fine there is no need for any changes.
# If temf is more appropriate change trof to temf below.

# Check if the Other fruits category is present.
other_fruits <- ir_area %>% 
  filter(crop %in% c("trof, temf"))
if(NROW(other_fruits) == 0) {
  message("There is no Other fruits category")
} else {
  message("There is an Other fruits category")
}

# If you want to change Other fruits to temf, change "trof" to "temf" in the statement below
ir_area <- ir_area %>%
 mutate(crop = if_else(aquastat_crop == "Other fruits", "trof", crop))


############### SAVE ###############
write_csv(ir_area, file.path(proc_path, glue("agricultural_statistics/aquastat_irrigated_crops_{year_sel}_{iso3c_sel}.csv")))
        

############### CLEAN UP ###############
rm(aquastat, aquastat_raw, aquastat_version, aquastat2crop, ir_area, other_fruits)


