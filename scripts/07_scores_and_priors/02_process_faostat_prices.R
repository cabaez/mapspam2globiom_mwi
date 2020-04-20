#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to process FAOSTAT price data
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
faostat_prices_version <- "20200303"

# Crop production
prod_raw <- read_csv(file.path(glob_raw_path, glue("faostat/{faostat_crops_version}_faostat_crops.csv")))

# price data
price_raw <- read_csv(file.path(glob_raw_path, glue("faostat/{faostat_prices_version}_faostat_prices.csv")))

# faostat2crop
faostat2crop <- read_excel(file.path(mappings_path, "mappings_spam.xlsx"), sheet = "faostat2crop") %>%
  dplyr::select(crop, faostat_crop_code) %>%
  na.omit()


############### PROCESS ###############
# Clean up FAOSTAT
price <- price_raw %>%
  setNames(tolower(names(.))) %>%
  filter(element == "Producer Price (USD/tonne)") %>%
  mutate(iso3c = countrycode(`area code`, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  dplyr::select(iso3c, faostat_crop_code = `item code`, year, price = value)

area <- prod_raw %>%
  setNames(tolower(names(.))) %>%
  filter(element == "Area harvested") %>%
  mutate(iso3c = countrycode(`area code`, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  dplyr::select(iso3c, item, faostat_crop_code = `item code`, year, area = value)

# Combine and calculate weighted average price for crop
# We take weighted average over five years to reduce fluctuations
# We take average for continents because otherwise there are many missing data (e.g. coffee in Southern Africa)
price_iso3c <- full_join(price, area) %>%
  na.omit() %>%
  left_join(faostat2crop) %>%
  filter(!is.na(crop)) %>%
  group_by(iso3c, crop, year) %>%
  summarize(price = sum(price*area)/sum(area, na.rm = T)) %>%
  ungroup() %>%
  filter(year %in% c(year_sel-1, year_sel, year_sel+1)) %>%
  mutate(continent = countrycode(iso3c, "iso3c", "continent"),
         region = countrycode(iso3c, "iso3c", "region")) %>%
  group_by(crop, continent) %>% 
  summarize(price = mean(price, na.rm = T)) %>%
  ungroup()

# Filter out continent prices
price_iso3c <- price_iso3c %>%
  filter(continent == countrycode(iso3c_sel, "iso3c", "continent")) 

# Check missing
crop_list <- faostat2crop %>%
  dplyr::select(crop) %>%
  unique()

miss_crop <- full_join(crop_list, price_iso3c) %>%
  complete(crop, continent) %>%
  filter(is.na(price))


########## SAVE ##########
temp_path <- file.path(proc_path, paste0("agricultural_statistics"))
dir.create(temp_path, recursive = T, showWarnings = F)
write_csv(price_iso3c, file.path(temp_path, glue("faostat_crop_prices_{year_sel}_{iso3c_sel}.csv")))


########## CLEAN UP ##########
rm(temp_path, area, crop_list, faostat2crop, miss_crop, price, price_iso3c, price_raw,
   prod_raw, faostat_crops_version, faostat_prices_version)
