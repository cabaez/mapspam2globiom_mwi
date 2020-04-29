#'========================================================================================================================================
#' Project:  crop map
#' Subject:  Script to calculate yield and prod
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "mapview")
# Additional packages
p_load("countrycode", "gdxrrw", "plotKML", "viridis")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


### LOAD DATA
# Spam
spam_raw <- readRDS(file.path(proc_path, paste0("spam_2.0/spam_final_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# ci
ci_raw <- readRDS(file.path(proc_path, paste0("agricultural_statistics/ci_", year_sel, "_", iso3c_sel, ".rds")))

# Harmonized system
lu_sy_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_sy_harm_", year_sel, "_", iso3c_sel, ".rds"))) 

# ifpri2crop
spam_stat2crop <- read_excel(file.path(mappings_path, "mappings_v1.1.xlsx"), sheet = "spam_stat2crop")

# lc
lc_raw <- readRDS(file.path(proc_path, paste0("harmonized/lc_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# GAEZ py
py_raw <- readRDS(file.path(proc_path, glue("harmonized/py_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# faostat
fao_raw <- readRDS(file.path(proc_path, paste0("agricultural_statistics/faostat_crops_", year_sel, "_", iso3c_sel, ".rds")))

# grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Adm_r
adm_r <- readRDS(file.path(proc_path, paste0("maps/adm/adm_r_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) 


### FUNCTIONS
# Plus function that ensures NA + NA is NA not 0 as in sum. If na.rm = F (default), NA + 0 = NA, otherwise 0, similar to sum
plus <- function(x, na.rm = F){
  if(all(is.na(x))){
    c(x[0],NA)
  } else {
    if(na.rm == T){
      sum(x, na.rm = TRUE)
    } else {
      sum(x, na.rm)
    }  
  }
} 

# Function to compare crop maps per system in a panel
view_4p_f <- function(crp, vr, sy = c("S", "L", "H", "I"), df = db){
  ext <- extent(grid)
  df <- df %>%
    filter(crop == crp, system %in% sy) %>%
    left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  st <- lapply(seq(length(st)), function(i){
    mapview(st[[i]], layer.name = paste(crp, sys[i], sep = "_"))
  })
  sync(st) 
}

# Function to compare crop maps in stack
view_st_f <- function(crp, vr, sy = c("S", "L", "H", "I"), df = db){
  ext <- extent(grid)
  df <- df %>%
    filter(crop == crp, system %in% sy) %>%
    left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  if(length(sys) >1){
    st <- stack(st)
  }else{
    st <- st[[1]]  
  }
  names(st) <- paste(crp, sys, vr, sep = "_")
  st[st==0] <- NA
  mapview(st, use.layer.names = T)
}


### PREPARE
# Only select ci for adm_sel
ci <- ci_raw %>%
  filter(adm_level == adm_sel)

# Rename fips and adm to fips_sel and adm_sel
fips_sel <- paste0("fips", adm_sel)
adm_sel2 <- paste0("adm", adm_sel)
names(ci)[names(ci) == "fips"] <- fips_sel
names(ci)[names(ci) == "adm"] <- adm_sel2

# Rename alloc into pa (physical area)
spam <- spam_raw %>%
  rename(pa = alloc)

# The model might allocate very small areas to a grid cell.
# We assume it is unrealistic that there is less than 1m2 of a crop in a 1km2 grid.
# We set all allocations that are smaller than 0.0001 = 1m2 to 0
spam <- spam %>%
  mutate(pa = if_else(pa < 0.0001, 0, pa))

# Process fao
fao <- fao_raw %>%
  filter(year %in% c((year_sel-1): (year_sel+1)), variable == "area") %>%
  group_by(crop) %>%
  summarize(value_ha = mean(value, na.rm = T)) %>%
  dplyr::select(crop, value_ha)


### CALCULATE HARVESTED AREA
# We multiply the physical area ci
spam <- spam %>%
  left_join(ci) %>%
  mutate(ha = pa*ci)

# Calculate fao scaling factor
fao_stat_sf <-bind_rows(
  fao %>%
    mutate(source = "fao"),
  spam %>%
    group_by(crop) %>%
    summarize(value_ha = sum(ha, na.rm = T)) %>%
    mutate(source = "stat")) %>%
  dplyr::select(crop, source, value_ha) %>%
  spread(source, value_ha) %>%
  mutate(sf = fao/stat) %>%
  dplyr::select(crop, sf)

# rescale stat
spam <- spam %>% 
  left_join(fao_stat_sf) %>%
  mutate(ha_scale = ha * sf) %>%
  dplyr::select(-sf)


### CALCULATE YIELD
# calculate average potential yield per crop, system and adm using allocated areas as weight

# As allocation is made to areas where potential yield is 0, 
# we impute 0 values by average at adm2, then adm1 level, then adm0 using py
py_imp_df <- left_join(py_raw, adm_r) %>%
  mutate(py = if_else(py == 0, NA_real_, py)) %>%
  group_by(adm2, fips2, crop, system, system) %>%
  mutate(py_adm2 = mean(py, na.rm = T)) %>%
  group_by(adm1, fips1, crop, system) %>%
  mutate(py_adm1 = mean(py, na.rm = T)) %>%
  group_by(crop, system) %>%
  mutate(py_adm0 = mean(py, na.rm = T)) %>%
  ungroup() %>%
  mutate(py_imp = case_when(
    is.na(py) & !is.na(py_adm2) ~ py_adm2,
    is.na(py) & is.na(py_adm2) & !is.na(py_adm1)  ~ py_adm1,
    is.na(py) & is.na(py_adm2) & is.na(py_adm1) & !is.na(py_adm0) ~ py_adm0,
    TRUE ~ py)) %>%
  dplyr::select(gridID, crop, system, adm0, fips0, adm1, fips1, adm2, fips2, py_imp)
summary(py_imp_df)

# Rename fips_sel and adm_sel to fips and adm
names(py_imp_df)[names(py_imp_df) == fips_sel] <- "fips"
names(py_imp_df)[names(py_imp_df) == adm_sel2] <- "adm"

py_imp_df <- dplyr::select(py_imp_df, gridID, crop, system, adm, fips, py_imp)

# Average potential yield in each grid, weighted using ha
av_py_df <- left_join(spam, py_imp_df) %>%
  group_by(crop, system, fips, adm) %>%
  mutate(av_py = sum(ha*py_imp, na.rm = T)/sum(ha, na.rm = T))

summary(av_py_df)
