#'========================================================================================================================================
#' Project:  mapspam2globiom
#' Subject:  Script to convert mapspam crop distribution maps to globiom simu input
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### AGGREGATE LAND COVER MAP TO GLOBIOM CLASSES ###############
# We take the ESACCI landcover map
lc_file <- file.path(param$spam_path,
                              glue("processed_data/maps/cropland/{param$res}/esa_raw_{param$year}_{param$iso3c}.tif"))
lc_map <- raster(lc_file)
plot(lc_map)

# Load mapping of lc classes to globiom lc classes
esacci2globiom<- read_excel(file.path(param$spam_path, 
                                     "parameters/mappings_spam.xlsx"), sheet = "esacci2globiom")

# Aggregate land cover map to GLOBIOM land cover classes
simu_lc_df <- calc_lc_area(esacci2globiom, lc_map)

# Aggregate mapspam crop distribution tif files to globiom simu
# We do this for physical area but if needed it can also be done for
# harvested area by replacing "pa", with "ha".
simu_crop_df <- mapspam2simu("pa", param)


# Rebalance lc map with mapspam crop distribution maps
# Replace CrpLnd with new values
# Replace OthAgri with rest crops
# If SimUarea - sum(CrpLnd, OthAgr, Forest, WetLnd, OthNatLnd, NotRel, Grass) > 0
# OthNatLnd = diff

# If SimUarea - sum(CrpLnd, OthAgr, Forest, WetLnd, OthNatLnd, NotRel, Grass) < 0
# 1 take from OthNatLnd
# 2 take from NotRel
# 3 take from WetLnd
# 4 take fom Forest
# 5 take from Grass


# Rebalance simu
simu_list <- unique(simu_lc_df$SimUID)
globiom_lc_upd <- map_df(simu_list, rebalance_lc, simu_lc_df, simu_crop_df)


### PREPARE COUNTRY DATA
## globiom_lc
globiom_lc <- globiom_lc_raw %>%
  filter(COUNTRY == country_sel) %>%
  dplyr::select(-COUNTRY)

# sum over AEZ, if in dataframe
globiom_lc <- globiom_lc %>%
  group_by(SimUID, LC_TYPES_EPIC) %>%
  summarize(value = sum(value, na.rm = T))

## Globiom_cd
# raw data might include some additional crops that are not used by GLOBIOM
globiom_cd <- globiom_cd_raw %>%
  filter(ALLCOUNTRY == country_sel) %>%
  dplyr::select(-ALLCOUNTRY) %>%
  mutate(system = recode(system, 
                         "SS" = "S",
                         "LI" = "L",
                         "HI" = "H",
                         "IR" = "I"))
unique(globiom_cd$globiom_crop)



# Add cropland system breakdown
spam_lc_crplnd <- spam_lc_system %>%
  filter(system != "rest") %>%
  mutate(LC_TYPES_EPIC = recode(system, 
                                "S" = "CrpLnd_S",
                                "L" = "CrpLnd_L",
                                "H" = "CrpLnd_H",
                                "I" = "CrpLnd_I")) %>%
  ungroup() %>%
  dplyr::select(SimUID, LC_TYPES_EPIC, value)

# lc
globiom_lc_final <- bind_rows(globiom_lc_upd, spam_lc_crplnd) %>%
  left_join(simu_mapping) %>%
  dplyr::select(LC_TYPES_EPIC, ALLCOUNTRY, ColRow30, AltiClass, SlpClass, SoilClass, value)
summary(globiom_lc_final)

# cd
globiom_cd_final <- spam_simu %>%
  filter(globiom_crop != "rest") %>%
  dplyr::select(SimUID, system, globiom_crop, value) %>%
  mutate(system = recode(system, 
                         "S" = "SS",
                         "L" = "LI",
                         "H" = "HI",
                         "I" = "IR")) %>%
  left_join(simu_mapping) %>%
  dplyr::select(SimUID, globiom_crop, system, ALLCOUNTRY, ColRow30, AltiClass, SlpClass, SoilClass, value)
summary(globiom_cd_final)


### SAVE
temp_path <- file.path(crop_map_path)
dir.create(temp_path, showWarnings = F, recursive = T)

saveRDS(globiom_cd_final, file.path(temp_path, paste0("crop_area_spam_", year_sel, "_", iso3c_sel, ".rds")))
saveRDS(globiom_lc_final, file.path(temp_path, paste0("landcover_", year_sel, "_", iso3c_sel, ".rds")))
