#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to calculate irrigated system shares
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
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### LOAD DATA ###############
# Adm statistics
stat_raw <- read_csv(file.path(proc_path, glue("agricultural_statistics/ha_adm_{year_sel}_{iso3c_sel}.csv")))

# Faostat
faostat_raw <- read_csv(file.path(proc_path, glue("agricultural_statistics/faostat_crops_{year_sel}_{iso3c_sel}.csv")))

# Faostat
aquastat_raw <- read_csv(file.path(proc_path, glue("agricultural_statistics/aquastat_irrigated_crops_{year_sel}_{iso3c_sel}.csv")))


########## PROCESS ##########
# Prepare stat
stat <- stat_raw %>%
  gather(crop, value_ha, -adm_name, -adm_code, -adm_level) %>%
  mutate(value_ha = as.numeric(value_ha),
         value_ha = if_else(value_ha == -999, NA_real_, value_ha),
         adm_code = as.character(adm_code))

aquastat <- aquastat_raw %>%
  filter(crop != "total", 
         variable %in% c("Harvested irrigated permanent crop area", "Harvested irrigated temporary crop area")) %>%
  dplyr::select(adm_code, adm_name, adm_level, crop, ir_area = value, year)

faostat <- faostat_raw %>%
  dplyr::select(adm_name, adm_code, adm_level, crop, year, value)

# Combine
ir_share <- left_join(faostat, aquastat) %>%
  na.omit %>%
  mutate(ir_share = ir_area/value*100)

# plot
ggplot(data = ir_share, aes(x = as.factor(year), y = ir_share, fill = crop)) +
  geom_col() +
  facet_wrap(~crop, scales = "free")
