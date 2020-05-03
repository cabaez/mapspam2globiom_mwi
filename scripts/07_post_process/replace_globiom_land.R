#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to extract GAMS results and map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "mapview")
# Additional packages
p_load("WDI", "countrycode", "plotKML", "sf", "gdxrrw", "viridis")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio

### GET DATA PATH
source(file.path(root, "scripts/support/get_data_path.R"))

### LINK GAMS LIBRARIES
igdx(gams_path)


### FUNCTIONS
# source gdx functions
source(file.path(root, "scripts/support/R2GDX.r"))

# TO CHECK: where are nan values come from
# function to aggregate crop map to simu
crop_map2simu_f <- function(crop_map){
  map <- raster(crop_map)
  message(names(map))
  ag_map <- as.data.frame(zonal(map, simu_r, fun = 'sum', na.rm = T)) %>%
    setNames(c("SimUID","value"))
  crop <- strsplit(names(map), "_")[[1]][2]
  system <- strsplit(names(map), "_")[[1]][3]
  year <- strsplit(names(map), "_")[[1]][5]
  iso3c <- strsplit(names(map), "_")[[1]][6]
  ag_map <- ag_map %>%
    mutate(crop = crop,
           system = system,
           year = year,
           iso3c = iso3c)
  return(ag_map)  
}

# Function to rebalance simu
rebalance_lc_f <- function(simu, lc_rp = c("OthNatLnd", "NotRel", "WetLnd", "Forest", "Grass")){
  message(simu)
  
  df <- globiom_lc %>%
    filter(!LC_TYPES_EPIC %in% c("CrpLnd_S", "CrpLnd_L", "CrpLnd_H", "CrpLnd_I", "CrpLnd", "OthAgri")) %>%
    left_join(base_fix,.) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    bind_rows(spam_simu_ag) %>%
    ungroup() %>%
    filter(SimUID == simu) 
  
  diff <- df$value[df$LC_TYPES_EPIC %in% "SimUarea"] - sum(df$value[!df$LC_TYPES_EPIC %in% "SimUarea"])
  print(paste0("total diff is ", diff))
  
  if(diff >= 0){
    df_upd <- df
    df_upd$value[df$LC_TYPES_EPIC == "OthNatLnd"] <- df_upd$value[df$LC_TYPES_EPIC == "OthNatLnd"] +diff
    print("No rebalancing needed, diff added to OthNatLnd")
  } else {
    for (i in lc_rp){
      if(df$value[df$LC_TYPES_EPIC == i] >= abs(diff)){
        print(paste0("Final ", abs(diff), " is subtracted from lc ", i, "."))
        df$value[df$LC_TYPES_EPIC == i] <- df$value[df$LC_TYPES_EPIC == i] - abs(diff)
        diff <- 0
        break
      } else {
        if(df$value[df$LC_TYPES_EPIC == i] == 0){
          print(paste0("0 is subtracted from lc ", i, " because it has 0 area."))
        } else {
          diff <- diff + df$value[df$LC_TYPES_EPIC == i]
          print(paste0(df$value[df$LC_TYPES_EPIC == i], " is subtracted from lc ", i, "."))  
          df$value[df$LC_TYPES_EPIC == i] <- 0
        }
      }
    }
    df_upd <- df
  }
  return(df_upd)
}


### LOAD DATA
# globiom2crop
globiom2crop <- read_excel(file.path(root, "mappings/mappings.xlsx"), sheet = "globiom2crop")

# globiom landcover baseline map
globiom_lc_raw <- rgdx.param(file.path(crop_map_path, "Initial_LandCover.gdx"), "LANDCOVERALL_INIT_SIMU") %>%
  dplyr::rename(value = LANDCOVERALL_INIT_SIMU) %>%
  droplevels() %>%
  mutate(LC_TYPES_EPIC = as.character(LC_TYPES_EPIC),
         SimUID = as.numeric(as.character(SimUID)))

# globiom crop distribution baseline
globiom_cd_raw <- rgdx.param(file.path(crop_map_path, "CropArea_SPAM.gdx"), "CropArea_SPAM") %>%
  dplyr::rename(value = CROPAREA_SPAM,
                globiom_crop = SPECIES,
                system = MngSystem) %>%
  droplevels() %>%
  mutate(globiom_crop = as.character(globiom_crop),
         system = as.character(system),
         SimUID = as.numeric(as.character(SimUID)))

# simu mapping
simu_mapping <- rgdx.set(file.path(crop_map_path, "Initial_LandCover.gdx"), "SimUID_MAP") %>%
  droplevels() %>%
  mutate(SimUID = as.numeric(as.character(SimUID)))


### SET COUNTRY AND REFERENCE YEAR
# Reference year refers to the year for which the spam map is made (normally 2000 for globiom)
iso3c_sel <- "ZWE"
iso3n_sel <- countrycode(iso3c_sel, "iso3c", "iso3n")
country_sel <- countrycode(iso3c_sel, "iso3c", "country.name")
year_sel <- 2010


### CREATE SIMU RASTER
source(file.path(root, "scripts/replace_globiom_land/select_simu.R"))


### AGGREGATE TO SIMU AND GLOBIOM CROPS
# Stack and aggregate to simu per crop
crop_map_files <- list.files(file.path(crop_map_path, paste0("spam_2.0/", year_sel, "/", iso3c_sel)), full.names = T, pattern = glob2rx("*.tif"))
spam_simu <- map_dfr(crop_map_files, crop_map2simu_f) 

# Aggregate to globiom crops
# Total is all 0 for some reason: CHECK
# We filter out crop aggregates if they would have been loaded
# Divide by 1000 to get 1000 ha as in globiom
spam_simu <- spam_simu %>%
  filter(crop != "total") %>%
  left_join(globiom2crop) %>%
  group_by(SimUID, year, iso3c, system, globiom_crop) %>%
  summarize(value = sum(value, na.rm = T)/1000) %>%
  ungroup


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


### COMPARE GLOBIOM LC AND UPDATED CROP DISTRIBUTION MAP
# Compare at system level
globiom_lc_system <- globiom_lc %>%
  filter(LC_TYPES_EPIC != "CrpLnd") %>%
  mutate(system = recode(LC_TYPES_EPIC, 
                         "CrpLnd_S" = "S",
                         "CrpLnd_L" = "L",
                         "CrpLnd_H" = "H",
                         "CrpLnd_I" = "I",
                         "OthAgri" = "rest",
                         .default = "other")) %>%
  mutate(system = na_if(system, "other")) %>%
  filter(!is.na(system)) %>%
  ungroup() %>%
  dplyr::select(system, value, SimUID) %>%
  mutate(SimUID = as.numeric(as.character(SimUID)),
         source = "globiom")

spam_lc_system <- spam_simu %>%
  mutate(system = ifelse(globiom_crop == "rest", "rest", system)) %>%
  group_by(system, SimUID) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  filter(value != 0) %>%
  mutate(source = "spam")

lc_comp <- bind_rows(globiom_lc_system, spam_lc_system)

# system plot
lc_comp %>%
  spread(source, value) %>%
  ggplot()  +
  geom_point(aes(x = spam, y = globiom)) +
  facet_wrap(~system, scales = "free") +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(aes(x = spam, y = globiom), method = "lm", se = F)

# totals
lc_comp_tot <- bind_rows(
  lc_comp %>%
    group_by(system, source) %>%
    summarize(value = sum(value, na.rm = T)),
  lc_comp %>%
    group_by(source) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(system = "total"))

lc_comp_tot %>%
  ggplot()  +
  geom_col(aes(x = source, y = value, fill = source)) +
  facet_wrap(~system, scales = "free") 


### COMPARE GLOBIOM CD AND UPDATED CROP DISTRIBUTION MAP
# Compare at crop level
cd_comp <- bind_rows(
  globiom_cd %>%
    mutate(source = "globiom"),
  spam_simu %>%
    mutate(source = "spam")) %>%
  dplyr::select(source, SimUID, system, globiom_crop, value) %>%
  filter(!globiom_crop %in% c("rest", "SISA","Coff","Pain"))

cd_comp %>%
  spread(source, value) %>%
  ggplot()  +
  geom_point(aes(x = spam, y = globiom, shape = system, colour = system)) +
  facet_wrap(~ globiom_crop, scales = "free") +
  geom_abline(intercept = 0, slope = 1) +
  geom_smooth(aes(x = spam, y = globiom), method = "lm", se = F)


### UPDATE GLOBIOM LC WITH SPAM
# Replace CrpLnd (and four systems) with new values
# Replace OthAgri with rest crops
# If SimUarea - sum(CrpLnd, OthAgr, Forest, WetLnd, OthNatLnd, NotRel, Grass) > 0
# OthNatLnd = diff

# If SimUarea - sum(CrpLnd, OthAgr, Forest, WetLnd, OthNatLnd, NotRel, Grass) < 0
# 1 take from OthNatLnd
# 2 take from NotRel
# 3 take from WetLnd
# 4 take fom Forest
# 5 take from Grass

# Ensure that all SimUID x LC_TYPES_EPIC are present
base_fix <- expand.grid(SimUID = unique(globiom_lc$SimUID), LC_TYPES_EPIC = c("Forest", "WetLnd", "NotRel", "OthNatLnd", "Grass", "SimUarea"), stringsAsFactors = F)
base_flex <- expand.grid(SimUID = unique(globiom_lc$SimUID), LC_TYPES_EPIC = c("OthAgri", "CrpLnd"), stringsAsFactors = F)

# Aggregate spam to CrpLnd
# split off rest crops
spam_simu_ag <- spam_simu %>%
  mutate(LC_TYPES_EPIC = ifelse(globiom_crop == "rest", "OthAgri", "CrpLnd")) %>%
  group_by(SimUID, LC_TYPES_EPIC) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  left_join(base_flex, .) %>%
  mutate(value = ifelse(is.na(value), 0, value))


# Rebalance simu
simu_list <- unique(globiom_lc$SimUID)
globiom_lc_upd <- map_df(simu_list, rebalance_lc_f)

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
