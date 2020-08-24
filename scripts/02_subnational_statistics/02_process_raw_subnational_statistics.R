#'========================================================================================================================================
#' Project:  mapspam2globiom
#' Subject:  Script to process raw subnational statistics
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### LOAD DATA ###############
### LOAD DATA
# Original data to SPAMc crop mapping
orig2crop <- read_csv(file.path(param$raw_path, "subnational_statistics/spam_stat2crop.csv")) %>%
  dplyr::select(-crop_full)

# raw administrative level statistics
stat_raw <- read_csv(file.path(param$raw_path, "subnational_statistics/stat_area_all.csv"), na = c("-999", ""))

# raw farming system and crop intensity statistics
sy_ci_raw <- read_csv(file.path(param$raw_path, paste0("subnational_statistics/dep_list_all.csv")), na = c("-999", ""))

# link table to reaggregate administrative units into those presented in the shapefile
link_raw <- read_csv(file.path(param$raw_path, paste0("subnational_statistics/linktable_all.csv")), na = c("-999", ""))


########## PREPARARE STAT ##########
# In the case of Malawi we are using raw data from SPAM2010 (https://www.mapspam.info)
# as source. Hence there was no need to collect data and aggregate crops. 
# Alternatively we could have started with a template file and use R or Excel to
# aggregate/split the raw statistics so they fit in the template.

# To create the templates use the following commands
ha_template <- create_statistics_template("ha", param)
fs_template <- create_statistics_template("fs", param)
ci_template <- create_statistics_template("ci", param)


# Remove columns that are not used
stat <- stat_raw %>%
  gather(crop_stat, value_ha, -stat_code, -prod_level, -name_cntr, -name_admin, -rec_type, -unit, -ar_irr, -ar_tot, -year_data, -source) %>%
  dplyr::select(-source, -year_data, -stat_code, -name_cntr, -rec_type, -unit, -ar_irr, -ar_tot) 

# In the Malawi case the raw adm2 statistics are more detailed than the
# shapefile with the location of the adm2 units. We use a linktable to aggregate
# the statistics so they are comparable with the maps.
# Create link table for adm1
link_adm1 <- link_raw %>%
  dplyr::select(region, prod_level = fips1, o_fips1, o_region) %>%
  unique()

# Reaggregate adm1
adm1_ag <- stat %>%
  filter(prod_level %in% unique(link_adm1$prod_level)) %>%
  left_join(link_adm1) %>%
  group_by(crop_stat, o_region, o_fips1) %>%
  summarize(value_ha = sum(value_ha, na.rm = F)) %>% 
  rename(name_admin = o_region, prod_level = o_fips1)

# Create link table for adm2
link_adm2 <- link_raw %>%
  dplyr::select(name_admin = admin_name, prod_level = fips2, o_fips2, o_adm_name) %>%
  unique()

# Reaggregate adm2
adm2_ag <- stat %>%
  filter(prod_level %in% unique(link_adm2$prod_level)) %>%
  left_join(link_adm2) %>%
  group_by(crop_stat, o_adm_name, o_fips2) %>%
  summarize(value_ha = sum(value_ha, na.rm = F)) %>% # 
  rename(name_admin = o_adm_name, prod_level = o_fips2)

# Combine adm0, adm1 and adm2 data
stat <- bind_rows(
  stat %>%
    filter(prod_level == unique(paste0(substring(stat$prod_level, 1, 2), "00"))),
  adm1_ag,
  adm2_ag)

# Add adm_level using adm codes and iso3c and rename
stat <- stat %>%
  mutate(n_char = nchar(prod_level),
         iso2 = substring(prod_level, 0, 2),
         adm_temp = substring(prod_level, 3, n_char),
         adm_level = ifelse(adm_temp == "00", 0, 
                            ifelse(nchar(adm_temp) == "2", 1, 2))) %>%
  dplyr::select(-adm_temp, -iso2, -n_char) %>%
  rename(adm_code = prod_level, adm_name = name_admin)

# Remove millet and coffee varietes, which are all zero and rename to SPAMc crop names
stat <- stat %>%
  filter(!crop_stat %in% c("pearlmill", "rob_coffee")) %>%
  mutate(crop_stat = if_else(crop_stat == "smallmill", "millet", crop_stat),
         crop_stat = if_else(crop_stat == "ara_coffee", "coffee", crop_stat),)

# Set adm0_name adm0_code equal to country name and iso3c code
stat <- stat %>%
  mutate(adm_name = if_else(adm_level == 0, param$country, adm_name),
         adm_code = if_else(adm_level == 0, param$iso3c, adm_code))

# Recode to standard SPAM crops Also aggregate as multiple orig crops could be
# linked to one SPAMc crop or crop group (not the case here).
# NB: use sum with na.rm = F as we want NA+NA = NA, not NA+NA = 0!
stat <- stat %>%
  left_join(orig2crop) %>%
  group_by(crop, adm_code, adm_name, adm_level) %>%
  summarize(value_ha = sum(value_ha, na.rm = F)) %>% 
  ungroup()

# Remove Area under National Administration as we also remove the polygon to 
# ensure no crops will be allocated there
stat <- stat %>%
  filter(!adm_name %in% "Area under National Administration")

# Update coff information. Secondary sources indicate coffee is grown in a
# selected number of ADM2s. We set these to -999 and let the model decide. We
# set ADM1 values informed by a pre SPAMc run where ADM1 values where NA. We
# only add this data to be able to run the model at the ADM1 level
# (param$solve_level = 1) as for this option data needs to be fully complate at
# the ADM1 level. 
coff_adm <- c("Dedza", "Ntchisi", "Chitipa", "Nkhata Bay", "Rumphi", "Mulanje", "Thyolo", 
              "Zomba")

stat <- stat %>%
  mutate(value_ha = case_when(
    adm_name %in% coff_adm & crop == "coff" ~ -999,
    adm_name == "Northern Region" & crop == "coff" ~ 540,
    adm_name == "Central Region" & crop == "coff" ~ 1452,
    adm_name == "Southern Region" & crop == "coff" ~ 1009,
    adm_name != "Malawi" & crop == "coff" ~ 0,
    TRUE ~ value_ha))

# Update bana, plnt for which ADM1 and ADM2 information is zero or NA.
# We replace using data from the pre-run model solution using the unchanged data
stat <- stat %>%
  mutate(value_ha = case_when(
    adm_code == "MI03" & crop == "bana" ~ 1656,
    adm_code == "MI04" & crop == "bana" ~ 16551.67,
    adm_code == "MI03" & crop == "plnt" ~ 29239,
    adm_code == "MI04" & crop == "plnt" ~ 6642.33,
    TRUE ~ value_ha))

# Update ofib, rest, temf, trof, vege for which ADM1 level data is missing
# completely. As this is an illustration, we use the maize ha shares to split
# the national data for these crops and impute the ADM1 level data.
crop_upd_share <- stat %>%
  dplyr::filter(adm_level %in% c(1), crop == "maiz") %>%
  mutate(tot = sum(value_ha, na.rm = T),
         share = value_ha/tot) %>%
  dplyr::select(adm_level, adm_code, adm_name, share)

crop_upd_adm0 <- stat %>%
  filter(crop %in% c("ofib", "rest", "temf", "trof", "vege"), adm_level %in% c(0)) %>% 
  dplyr::select(crop, adm0_value_ha = value_ha)

crop_upd_adm1 <- stat %>%
  filter(crop %in% c("ofib", "rest", "temf", "trof", "vege"), adm_level %in% c(1)) %>%
  left_join(crop_upd_adm0) %>%
  left_join(crop_upd_share) %>%
  mutate(value_ha = share * adm0_value_ha) %>%
  dplyr::select(adm_level, adm_code, adm_name, value_ha, crop)

stat <- bind_rows(
  stat %>% 
    filter(!(crop %in% c("ofib", "rest", "temf", "trof", "vege") & adm_level == 1)),
  crop_upd_adm1)

# Put in preferred mapspam format, adding -999 for missing values
stat_mapspam <- stat %>%
  spread(crop, value_ha, fill = -999) %>%
  arrange(adm_code, adm_code, adm_level) 


########## PROCESS SY_CI ##########
# For most models we only need adm0 level but we also select adm1 in case the model needs to be run at adm1 level
sy_ci <- sy_ci_raw %>%
  gather(crop_stat, value, -iso3, -prod_level, -rec_type, -name_cntr, -name_admin, -rec_type, -unit, -year_data, -source) %>%
  dplyr::select(-source, -year_data, -iso3, -name_cntr) %>%
  mutate(n_char = nchar(prod_level),
         iso2 = substring(prod_level, 0, 2),
         adm_temp = substring(prod_level, 3, n_char),
         adm_level = ifelse(adm_temp == "00", 0, 
                            ifelse(nchar(adm_temp) == "2", 1, 2))) %>%
  dplyr::select(-adm_temp, -iso2, -n_char) %>%
  filter(adm_level %in% c(0,1))

# Reaggregate adm1
sy_ci_adm1_ag <- sy_ci %>%
  filter(adm_level == 1) %>%
  left_join(link_adm1) %>%
  group_by(crop_stat, o_region, o_fips1, rec_type, adm_level) %>%
  summarize(value = mean(value, na.rm = T)) %>% 
  rename(name_admin = o_region, prod_level = o_fips1) %>%
  ungroup()

# Combine adm0, adm1 data
sy_ci <- bind_rows(
  sy_ci %>%
    filter(adm_level == 0),
  sy_ci_adm1_ag)

# Remove millet and coffee varietes that are not needed and rename
sy_ci <- sy_ci %>%
  filter(!crop_stat %in% c("pearlmill", "rob_coffee")) %>%
  mutate(crop_stat = if_else(crop_stat == "smallmill", "millet", crop_stat),
         crop_stat = if_else(crop_stat == "ara_coffee", "coffee", crop_stat),)

# Rename and select variables
sy_ci <- sy_ci %>%
  dplyr::select(adm_code = prod_level, adm_name = name_admin, adm_level, variable = rec_type, crop_stat, value) 

# Set adm0_name adm0_code
sy_ci <- sy_ci %>%
  mutate(adm_name = if_else(adm_level == 0, param$country, adm_name),
         adm_code = if_else(adm_level == 0, param$iso3c, adm_code))

# Recode to standard SPAM crops
sy_ci <- sy_ci %>%
  left_join(orig2crop) %>%
  dplyr::select(-crop_stat)

# Remove Area under National Administration as we also remove the polygon to 
# ensure no crops will be allocated there
sy_ci <- sy_ci %>%
  filter(!adm_name %in% "Area under National Administration")


########## PREPARE FARMING SYSTEMS SHARE ##########
# System shares
sy <- sy_ci %>%
  filter(variable %in% c("SHIRR", "SHRFH", "SHRFS")) %>%
  spread(variable, value) %>%
  rename(H = SHRFH, I = SHIRR, S = SHRFS) %>%
  mutate(L = 100-H-I-S) %>%
  dplyr::select(crop, adm_name, adm_code, adm_level, S, H, I, L) %>%
  gather(system, share, -crop, -adm_name, -adm_code, -adm_level) %>%
  mutate(share = share/100) 

# Set tea and whea to 100% irrigated in line with secondary statistics
sy <- sy %>%
  mutate(share = case_when(
    crop == "whea" & system == "I" ~ 1,
    crop == "whea" & system != "I" ~ 0,
    crop == "teas" & system == "I" ~ 1,
    crop == "teas" & system != "I" ~ 0,
    TRUE ~ share))

# Wide format
sy_mapspam <- sy %>%
  spread(crop, share, fill = -999) %>%
  arrange(adm_code, adm_code, adm_level) 


########## PREPARE CROPING INTENSITY ##########
# Cropping intensity by system
ci <- sy_ci %>%
  filter(variable %in% c("CIIRR", "CIRFH", "CIRFL")) %>%
  rename(ci = variable) %>%
  spread(ci, value) %>%
  rename(H = CIRFH, I = CIIRR, L = CIRFL) %>%
  mutate(S = L) %>%
  dplyr::select(crop, adm_name, adm_code, adm_level, S, H, I, L) %>%
  gather(system, ci, -crop, -adm_name, -adm_code, -adm_level) 

# Wide format
ci_mapspam <- ci %>%
  spread(crop, ci, fill = -999) %>%
  arrange(adm_code, adm_code, adm_level) 


############### SAVE ###############
write_csv(stat_mapspam, file.path(param$raw_path, "subnational_statistics/subnational_harvested_area_2010_MWI.csv"))
write_csv(ci_mapspam, file.path(param$raw_path, "subnational_statistics/cropping_intensity_2010_MWI.csv"))
write_csv(sy_mapspam, file.path(param$raw_path, "subnational_statistics/farming_system_shares_2010_MWI.csv"))


############### NOTE ###############
# As you probably created a lot of objects in he R memory, we recommend to
# restart R at this moment and start fresh. This can be done easily in RStudio by
# pressing CTRL/CMD + SHIFT + F10.
