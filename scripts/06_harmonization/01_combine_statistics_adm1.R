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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### LOAD DATA ###############
# Harvested area
ha <- read_csv(file.path(param$spam_path, 
                         glue("processed_data/agricultural_statistics/ha_adm_{param$year}_{param$iso3c}.csv")))

# Adm list
adm_list <- read_csv(file.path(param$spam_path, 
                               glue("processed_data/lists/adm_list_{param$year}_{param$iso3c}.csv")))

# Farming system shares
fs <- read_csv(file.path(param$raw_path,
                         glue("subnational_statistics/farming_system_shares_{param$year}_{param$iso3c}.csv")))

# Cropping intensity
ci <- read_csv(file.path(param$raw_path,
                         glue("subnational_statistics/cropping_intensity_{param$year}_{param$iso3c}.csv")))


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
if(param$solve_level == 0) {
  adm_code_list <- unique(adm_list$adm0_code)
} else {
  adm_code_list <- unique(adm_list$adm1_code)
}

adm_code_sel = adm_code_list

############### COMBINE DATA AT ADM LEVEL ###############
# Function to save pa and pa_fs according to solve_sel
prep_pa <- function(adm_code_sel){
  
  message(glue("Save pa and pa_fs statistics for {adm_code_sel}"))
  
  # Select ha for top level and all lower level ADMs
  ha_adm <- bind_rows(
    ha[ha$adm_code == adm_code_sel,],
    ha[ha$adm_code %in% adm_list$adm1_code[adm_list$adm0_code == adm_code_sel],],
    ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm1_code == adm_code_sel],],
    ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm0_code == adm_code_sel],]) %>%
    unique()
  
  # Select fs and ci for top level ADM only. We apply these to lower levels.
  fs_adm <- bind_rows(
    fs[fs$adm_code == adm_code_sel,]) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level) %>%
    unique()
  
  ci_adm <- bind_rows(
    ci[ci$adm_code == adm_code_sel,]) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level) %>%
    unique()
  
  # Calculate physical area using cropping intensity information.
  pa_adm <- ha_adm %>%
    left_join(ci_adm, by = "crop")  %>%
    left_join(fs_adm, by = c("crop", "system")) %>%
    mutate(pa = ha*fs/ci) %>%
    group_by(adm_name, adm_code, crop, adm_level) %>%
    summarize(pa = plus(pa, na.rm = T)) %>%
    ungroup()
  
  # Calculate physical area broken down by farming systems
  pa_fs_adm <- pa_adm %>%
    filter(adm_code == adm_code_sel) %>%
    left_join(fs_adm, by = "crop") %>%
    mutate(pa = pa*fs) %>%
    dplyr::select(-fs) %>%
    ungroup()
  
  
  ########## CONSISTENCY CHECKS ##########
  
  
  ########## SAVE STATISTICS ##########
  pa_adm <- pa_adm %>%
    spread(crop, pa) %>%
    arrange(adm_code, adm_name, adm_level)
  
  pa_fs_adm <- pa_fs_adm %>%
    spread(crop, pa) %>%
    arrange(adm_code, adm_name, adm_level)
  
  write_csv(pa_adm, file.path(param$spam_path,
    glue("pa_{param$year}_{adm_code_sel}_{param$iso3c}.csv")))
  write_csv(pa_fs_adm, file.path(param$spam_path,
    glue("pa_fs_{param$year}_{adm_code_sel}_{param$iso3c}.csv")))
}

walk(adm_code_list, prep_pa)


########## CLEAN UP ##########
rm(temp_path, adm_list, adm_stat_list, adm_crop_list, ci, ci_raw, crop_na_0, fs, fs_raw, ha, ha_raw)



# make sure pa and pa_fs totals are the same
pa_fs_check <- left_join(
  pa_adm %>% 
    filter(adm_code == adm_code_sel) %>%
    group_by(crop) %>%
    summarize(pa = plus(pa, na.rm = F)),
  pa_fs_adm %>%
    group_by(crop) %>%
    summarize(pa_fs = plus(pa, na.rm = F))) %>%
  mutate(diff = pa_fs-pa)

all.equal(pa_fs_check$pa_fs, pa_fs_check$pa)
