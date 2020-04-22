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
if(param$solve_level == 0) {
  adm_code_list <- unique(adm_list$adm0_code)
} else {
  adm_code_list <- unique(adm_list$adm1_code)
}

# Split data for all ADMs at solve_level
walk(adm_code_list, prepare_pa_stat, ha, fs, ci, param)


########## CLEAN UP ##########
rm(adm_list, ci, crop_na_0, fs, ha)


prepare_pa_stat <- function(adm_code, ha, fs, ci, param){
  # load data
  load_data(c("adm_list"), param, local = TRUE)
  
  message(glue("Save pa and pa_fs statistics for {adm_code}"))
  
  # Select ha for top level and all lower level ADMs
  ha_adm <- bind_rows(
    ha[ha$adm_code == adm_code,],
    ha[ha$adm_code %in% adm_list$adm1_code[adm_list$adm0_code == adm_code],],
    ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm1_code == adm_code],],
    ha[ha$adm_code %in% adm_list$adm2_code[adm_list$adm0_code == adm_code],]) %>%
    unique()
  
  # Select fs and ci for top level ADM only. We apply these to lower levels.
  fs_adm <- bind_rows(
    fs[fs$adm_code == adm_code,]) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level) %>%
    unique()
  
  ci_adm <- bind_rows(
    ci[ci$adm_code == adm_code,]) %>%
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
    filter(adm_code == adm_code) %>%
    left_join(fs_adm, by = "crop") %>%
    mutate(pa = pa*fs) %>%
    dplyr::select(-fs) %>%
    ungroup()
  
  # consistency check
  compare_adm2(pa_adm, pa_fs_adm, param$solve_level)
  
  # Set adm_level
  if(param$solve_level == 0) {
    adm_code_list <- unique(adm_list$adm0_code)
  } else {
    adm_code_list <- unique(adm_list$adm1_code)
  }
  
  # Save
  purrr::walk(adm_code_list, split_stat, df, "pa", param)
  purrr::walk(adm_code_list, split_stat, df, "pa_adm", param)
 
}

split_stat <- function(adm_code, df, var, param){
  message(glue::glue("Save {var} for {adm_code}"))

  df <- df %>%
    spread(crop, pa) %>%
    arrange(adm_code, adm_name, adm_level)
  
  temp_path <- file.path(param$spam_path,
                         glue::glue("processed_data/intermediate_output/{adm_code}"))
  dir.create(temp_path, recursive = T, showWarnings = F)
  write_csv(df, file.path(temp_path,
                              glue::glue("{var}_{param$year}_{adm_code}_{param$iso3c}.csv")))
}
  


  
# save
temp_path <- file.path(param$spam_path,
                       glue::glue("processed_data/intermediate_output/{adm_code}"))
dir.create(temp_path, recursive = T, showWarnings = F)

pa_adm <- pa_adm %>%
  spread(crop, pa) %>%
  arrange(adm_code, adm_name, adm_level)

pa_fs_adm <- pa_fs_adm %>%
  spread(crop, pa) %>%
  arrange(adm_code, adm_name, adm_level)

write_csv(pa_adm, file.path(temp_path,
                            glue::glue("pa_{param$year}_{adm_code}_{param$iso3c}.csv")))
write_csv(pa_fs_adm, file.path(temp_path,
                               glue::glue("pa_fs_{param$year}_{adm_code}_{param$iso3c}.csv")))

