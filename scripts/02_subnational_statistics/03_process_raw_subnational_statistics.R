#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to process raw subnational statistics
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

# !diagnostics off


############### LOAD DATA ###############
# ifpri2crop
crop_orig2crop <- read_excel(file.path(param$spam_path,
                                       "parameters/mappings_spam.xlsx"), sheet = "crop_orig2crop")

# adm statistics
stat_raw <- read_csv(file.path(param$raw_path,
                               glue("subnational_statistics/subnational_harvested_area_{param$year}_{param$iso3c}.csv")))

# adm_map_list
adm_list <- read_csv(file.path(param$spam_path,
                               glue("processed_data/lists/adm_list_{param$year}_{param$iso3c}.csv")))

# faostat
fao_raw <- read_csv(file.path(param$spam_path,
                              glue("processed_data/agricultural_statistics/faostat_crops_{param$year}_{param$iso3c}.csv")))


############### PROCESS STATISTICS ###############
# wide to long format
stat <- stat_raw %>%
  gather(crop_orig, ha, -adm_name, -adm_code, -adm_level)

# Convert -999 and empty string values to NA
stat <- stat %>%
  mutate(ha = if_else(ha == -999, NA_real_, ha),
         ha = as.numeric(ha)) # this will transform empty string values "" into NA and throw a warning

# filter out crops which values are all zero or NA
crop_na_0 <- stat %>%
  group_by(crop_orig) %>%
  filter(all(ha %in% c(0, NA))) %>%
  dplyr::select(crop_orig) %>%
  unique

stat <- stat %>%
  filter(!crop_orig %in% crop_na_0$crop_orig)

# Remove lower level adm data if it would in the data but not used
stat <- stat %>%
  filter(adm_level <= param$adm_level)

# Round values
stat <- stat %>%
  mutate(ha = round(ha, 0))

# Recode crop names if needed
stat <- stat %>%
  #   left_join(crop_orig2crop)
  rename(crop = crop_orig)


########## CONSISTENCY CHECKS ##########

### CHECK CROP MAPPING
crop_no_label <- unique(stat$crop_orig[is.na(stat$crop)])
if(length(crop_no_label)>0) {
  message("These crops are not mapped to the SPAM crops")
  crop_no_label
} else {
  message("All crops are mapped to the SPAM crops.")
}

# Remove crop_orig column
stat$crop_orig <- NULL


### MAKE SURE THAT TOTALS AT HIGHER LEVEL ARE THE SAME AS SUBTOTALS.
# We start at the lowest level, assuming lower levels are preferred if more than one level
# of data is available and data is complete.
# TOADD. In case of inconsistencies: incomplete data at lower level but with a higher subtotal,
# data is also replaced and all data with NA at the lower level is set to 0.
stat <- rebalance_stat(stat, param)


########## HARMONIZE WITH FAOSTAT ##########
# Compare with FAO
# Process fao
fao <- fao_raw %>%
  filter(year %in% c((param$year-1): (param$year+1))) %>%
  group_by(crop) %>%
  summarize(ha = mean(value, na.rm = T)) %>%
  dplyr::select(crop, ha)

# Compare
fao_stat <- bind_rows(
  fao %>%
    mutate(source = "fao"),
  stat %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat")
)

ggplot(data = fao_stat) +
  geom_col(aes(x = source, y = ha, fill = source)) +
  facet_wrap(~crop, scales = "free")

# We scale all the data to FAOSTAT
# If the data is incomplete and the sum is lower than FAOSTAT we do no adjust.
# If the data is incomplete and the sum is higher than FAOSTAT we scale down.

# Identify crops that are present in stat but not in fao and remove them from stat.
crop_rem <- setdiff(unique(stat$crop), unique(fao$crop))
stat <- stat %>%
  filter(!crop %in% crop_rem)

# Identify crops that are present in fao but not in stat.
# We will add then to stat
crop_add <- setdiff(unique(fao$crop), unique(stat$crop))
stat <- stat %>%
  bind_rows(
    fao %>%
      filter(crop %in% crop_add) %>%
      mutate(
        fips = unique(stat$fips[stat$adm_level==0]),
        adm_level = 0,
        adm = unique(stat$adm[stat$adm_level==0])))


# Calculate scaling factor
fao_stat_sf <-bind_rows(
  fao %>%
    mutate(source = "fao"),
  stat %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat")) %>%
  dplyr::select(crop, source, ha) %>%
  spread(source, ha) %>%
  mutate(sf = fao/stat) %>%
  dplyr::select(crop, sf)

# rescale stat
stat <- stat %>%
  left_join(fao_stat_sf) %>%
  mutate(ha = ha * sf) %>%
  dplyr::select(-sf)

# Compare again
fao_stat <- bind_rows(
  fao %>%
    mutate(source = "fao"),
  stat %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat")
)

ggplot(data = fao_stat) +
  geom_col(aes(x = source, y = ha, fill = source)) +
  facet_wrap(~crop, scales = "free")


### CONSISTENCY CHECKS
check_stat(stat, param)

# To wide format
stat <- stat %>%
  spread(crop, ha, fill = -999) %>%
  arrange(adm_code, adm_name, adm_level)


########## SAVE ##########
write_csv(stat, file.path(param$spam_path, glue("processed_data/agricultural_statistics/ha_adm_{param$year}_{param$iso3c}.csv")))


