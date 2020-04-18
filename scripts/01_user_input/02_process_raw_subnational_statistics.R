#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to process raw subnational statistics
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

# !diagnostics off


### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))


############### LOAD DATA ###############
# ifpri2crop
crop_orig2crop <- read_excel(file.path(mappings_path, "mappings_spam.xlsx"), sheet = "crop_orig2crop")

# adm statistics
stat_raw <- read_csv(file.path(raw_path, glue("subnational_statistics/subnational_harvested_area_{year_sel}_{iso3c_sel}.csv")))

# adm_map_list
adm_map_list <- read_csv(file.path(proc_path, glue("lists/adm_map_list_{year_sel}_{iso3c_sel}.csv")))

# faostat
fao_raw <- read_csv(file.path(proc_path, glue("agricultural_statistics/faostat_crops_{year_sel}_{iso3c_sel}.csv")))


############### PROCESS STATISTICS ###############
# wide to long format
stat <- stat_raw %>% 
  gather(crop_orig, value_ha, -adm_name, -adm_code, -adm_level)

# Convert -999 and empty string values to NA
stat <- stat %>%
  mutate(value_ha = if_else(value_ha == -999, NA_real_, value_ha),
         value_ha = as.numeric(value_ha)) # this will transform empty string values "" into NA and throw a warning

# filter out crops which values are all zero or NA
crop_na_0 <- stat %>%
  group_by(crop_orig) %>%
  filter(all(value_ha %in% c(0, NA))) %>%
  dplyr::select(crop_orig) %>%
  unique

stat <- stat %>%
  filter(!crop_orig %in% crop_na_0$crop_orig)

# Remove lower level adm data if it would somehow not be used 
stat <- stat %>%
  filter(adm_level <= adm_sel)

# Round values
stat <- stat %>%
  mutate(value_ha = round(value_ha, 0))

# # Recode crop names
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


### ADM2
# Aggregate adm2
adm2_reag <- stat %>%
  filter(adm_level == 2) %>%
  left_join(adm_map_list %>%
              dplyr::select(adm2_name, adm2_code, adm1_name, adm1_code) %>%
              unique() %>%
              dplyr::rename(adm_name = adm2_name, adm_code = adm2_code)) %>%
  group_by(adm1_name, adm1_code, crop) %>%
  summarize(adm2_tot = plus(value_ha, na.rm = F)) %>% #NB use plus with na.rm = F because we want NA+NA = NA but NA + 0 = NA
  rename(adm_name = adm1_name, adm_code = adm1_code)

# Reveal inconsistencies in subtotal, i.e. if adm2 subtotal != adm1 total
adm1_to_replace <- stat %>%
  filter(adm_level == 1) %>%
  dplyr::rename(adm1_tot = value_ha) %>%
  left_join(adm2_reag) %>% 
   mutate(update = case_when(
    is.na(adm2_tot) ~ "N",
    adm1_tot ==  adm2_tot ~ "N",
    TRUE ~ "Y")) %>%
  filter(update == "Y") %>%
  mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

adm1_replace <- adm1_to_replace %>%
  dplyr::select(adm_code, adm_name, adm_level, value_ha = adm2_tot, crop) %>%
  mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

# update stat
stat <- bind_rows(
  stat %>%
    mutate(adm_code_crop = paste(adm_code, crop, sep = "_")) %>%
    filter(!adm_code_crop %in% adm1_to_replace$adm_code_crop) %>%
    ungroup,
  adm1_replace) %>%
  dplyr::select(-adm_code_crop)


## ADM1
# Aggregate adm1
adm1_reag <- stat %>%
  filter(adm_level == 1) %>%
  left_join(adm_map_list %>%
              dplyr::select(adm1_name, adm1_code, adm0_name, adm0_code) %>%
              unique() %>%
              dplyr::rename(adm_name = adm1_name, adm_code = adm1_code)) %>%
  group_by(adm0_name, adm0_code, crop) %>%
  summarize(adm1_tot = plus(value_ha, na.rm = F)) %>%
  rename(adm_name = adm0_name, adm_code = adm0_code)

# Reveal inconsistencies in subtotal, i.e. if adm2 subtotal != adm1 total
adm0_to_replace <- stat %>%
  filter(adm_level == 0) %>%
  dplyr::rename(adm0_tot = value_ha) %>%
  left_join(adm1_reag) %>% 
  mutate(update = case_when(
    is.na(adm1_tot) ~ "N",
    adm0_tot ==  adm1_tot ~ "N",
    TRUE ~ "Y")) %>%
  filter(update == "Y") %>%
  mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

adm0_replace <- adm0_to_replace %>%
  dplyr::select(adm_code, adm_name, adm_level, value_ha = adm1_tot, crop) %>%
  mutate(adm_code_crop = paste(adm_code, crop, sep = "_"))

# update stat
stat <- bind_rows(
  stat %>%
    mutate(adm_code_crop = paste(adm_code, crop, sep = "_")) %>%
    filter(!adm_code_crop %in% adm0_to_replace$adm_code_crop) %>%
    ungroup,
  adm0_replace) %>%
  dplyr::select(-adm_code_crop)


########## HARMONIZE WITH FAOSTAT ##########
# Compare with FAO
# Process fao
fao <- fao_raw %>%
  filter(year %in% c((year_sel-1): (year_sel+1))) %>%
  group_by(crop) %>%
  summarize(value_ha = mean(value, na.rm = T)) %>%
  dplyr::select(crop, value_ha)

# Compare
fao_stat <- bind_rows(
  fao %>%
    mutate(source = "fao"),
  stat %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat")
)

ggplot(data = fao_stat) +
  geom_col(aes(x = source, y = value_ha, fill = source)) +
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
  dplyr::select(crop, source, value_ha) %>%
  spread(source, value_ha) %>%
  mutate(sf = fao/stat) %>%
  dplyr::select(crop, sf)

# rescale stat
stat <- stat %>% 
  left_join(fao_stat_sf) %>%
  mutate(value_ha = value_ha * sf) %>%
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
  geom_col(aes(x = source, y = value_ha, fill = source)) +
  facet_wrap(~crop, scales = "free")


### CONSISTENCY CHECKS

# Compare totals at different adm levels
stat %>% filter(adm_level == 0) %>%
  summarize(value_ha = sum(value_ha, na.rm = T))

stat %>% filter(adm_level == 1) %>%
  summarize(value_ha = sum(value_ha, na.rm = T))

stat %>% filter(adm_level == 2) %>%
  summarize(value_ha = sum(value_ha, na.rm = T))

# Check how crop totals compare
# In not, there were missing values at the adm1 or adm2 level
comp_crop_tot <- left_join(
  stat %>% filter(adm_level == 0) %>%
    group_by(crop) %>%
    summarize(adm0 = plus(value_ha, na.rm = F)),
  stat %>% filter(adm_level == 1) %>%
    group_by(crop) %>%
    summarize(adm1 = plus(value_ha, na.rm = F))) %>%
  left_join(.,
            stat %>% filter(adm_level == 2) %>%
              group_by(crop) %>%
              summarize(adm2 = plus(value_ha, na.rm = F)))

# Check if values are equal at all levels. 
check <- na.omit(comp_crop_tot[c("adm0", "adm1")])
all.equal(check$adm0, check$adm1)
check <- na.omit(comp_crop_tot[c("adm0", "adm2")])
all.equal(check$adm0, check$adm2)
check <- na.omit(comp_crop_tot[c("adm1", "adm2")])
all.equal(check$adm1, check$adm2)
rm(check)

# To wide format
stat <- stat %>%
  spread(crop, value_ha, fill = -999) %>%
  arrange(adm_code, adm_name, adm_level)


########## SAVE ##########
temp_path <- file.path(proc_path, paste0("agricultural_statistics"))
dir.create(temp_path, recursive = T, showWarnings = F)
write_csv(stat, file.path(temp_path, paste0("ha_adm_", year_sel, "_", iso3c_sel, ".csv")))


########## CLEAN UP
# Remove everything in memory but not package loads to start clean
rm(list = ls())

