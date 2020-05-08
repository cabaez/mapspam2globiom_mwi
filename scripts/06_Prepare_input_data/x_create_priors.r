#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Code to create priors using updated original spam approach
#' Author:   Michiel van Dijk, Yating Ru
#' Contact:  michiel.vandijk@wur.nl, y.ru@cgiar.org
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "R.utils", "mapview")

### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer=NULL)


### FUNCTIONS
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


### LOAD DATA
# stat
lu_adm_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_adm_harm_", year_sel, "_", iso3c_sel, ".rds")))

# sys
lu_sy_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_sy_harm_", year_sel, "_", iso3c_sel, ".rds")))

# grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# lc
lc_raw <- readRDS(file.path(proc_path, paste0("harmonized/lc_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# lu_ir
lu_ir_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_ir_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# lc_det
lu_det_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_det_grid_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) 
lu_det_all_raw <- readRDS(file.path(proc_path, paste0("harmonized/lu_det_all_grid_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) 

# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

# Adm_r
adm_r <- readRDS(file.path(proc_path, paste0("maps/adm/adm_r_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) 

# Population map
pop_raw <- raster(file.path(proc_path, paste0("maps/pop/pop_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")))
names(pop_raw) <- "pop"

# Urban mask
urban_mask <- readRDS(file.path(proc_path, paste0("maps/urban_mask/urban_mask_", year_sel, "_", iso3c_sel, ".rds")))
urban_mask <- as(urban_mask, "Spatial")

# Travel time map
tt_raw <- raster(file.path(proc_path, paste0("maps/travel_time/travel_time_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")))
names(tt_raw) <- "tt"

# Field size
fs_raw <- raster(file.path(proc_path, paste0("maps/field_size/field_size_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")))
names(fs_raw) <- "fs"

# GAEZ suitability 
suit_files <- list.files(file.path(proc_path, paste0("maps/gaez/", grid_sel, "/suit")), full.names = T, pattern = glob2rx("*.tif"))
suit_names <- basename(suit_files)
suit_names <- unlist(lapply(strsplit(suit_names, "_"), function(x) paste(x[1], x[2], sep="_")))
suit_raw <- stack(suit_files)
names(suit_raw) <- suit_names

# GAEZ potential yield
py_files <- list.files(file.path(paste0(proc_path, "/maps/gaez/", grid_sel, "/prod/")), full.names=TRUE, pattern = glob2rx("*.tif"))
py_names <- basename(py_files)
py_names <- unlist(lapply(strsplit(py_names, "_"), function(x) paste(x[1], x[2], sep="_")))
py_raw <- stack(py_files)
names(py_raw) <- py_names

# Continent level crop price in USD/ton
price <- readRDS(file.path(proc_path, paste0("agricultural_statistics/faostat_crop_price_", year_sel, "_", iso3c_sel, ".rds")))
  
# dm2fm comnversion for GAEZ potential yield
dm2fm <- read_excel(file.path(glob_path, "SPAM_2010/conversion_factors/GAEZ_dry_yield_convert_factor.xlsx")) 

# ifpri2crop
spam_stat2crop <- read_excel(file.path(mappings_path, "mappings.xlsx"), sheet = "spam_stat2crop")


### PROCESS DATA
## Create gridID and system combinations
crop_system <- lu_sy_raw %>%
  filter(adm_level == 0, value != 0) %>%
  mutate(crop_system = paste(crop, system , sep = "_"))

priors_base <- expand.grid(gridID = unique(lc_raw$gridID), 
                           crop_system = unique(crop_system$crop_system), stringsAsFactors = F) %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F)

# create gridID list
grid_df <- as.data.frame(rasterToPoints(grid))

## Rural population 
# Note that we normalize over adms to distribute the crops more evenly over adms. 
# If we would normalize over the whole country, crops for which we do not have adm information,
# might be pushed to a very limited area.
pop_rural <- mask(pop_raw, urban_mask, inverse = T) # Remove urban areas
pop_rural <- as.data.frame(rasterToPoints(stack(grid, pop_rural))) %>%
  dplyr::select(gridID, pop) %>%
  mutate(pop = ifelse(is.na(pop), 0, pop)) %>% # We assume zero population in case data is missing
  left_join(adm_r) %>%
  rename(fips = paste0("fips", adm_sel)) %>%
  group_by(fips) %>%
  mutate(
         pop_norm = 100*(pop-min(pop, na.rm = T))/(max(pop, na.rm = T)-min(pop, na.rm = T))) %>%
  ungroup() %>%
  mutate(pop_norm = ifelse(is.nan(pop_norm), 0, pop_norm)) %>%
  dplyr::select(gridID, pop_norm) %>%
  filter(gridID %in% unique(lc_raw$gridID)) 
summary(pop_rural)
hist(pop_rural$pop_norm, breaks = 100)

pop_rural_r <- rasterFromXYZ(
  left_join(grid_df, pop_rural) %>%
    dplyr::select(x, y, pop_norm), crs = crs(grid))
pop_rural_r[pop_rural_r == 0] <- NA
plot(pop_rural_r)

## Travel time
# NOTE that we normalize so that max = 0 and min = 1 as higher tt gives lower suitability
# NOTE that we normalize over the whole country as some cash crops are allocated at national level. 
# We expected these crops to be located a most accessible areas from a national (not adm) perspective
# Hence we do not normalize using adm_sel as a basis.
tt <- as.data.frame(rasterToPoints(stack(grid, tt_raw))) %>%
  dplyr::select(gridID, tt) %>%
  left_join(adm_r) %>%
  rename(fips = paste0("fips", adm_sel)) %>%
  mutate(
    #tt = log(tt+1),
         tt_norm = 100*(max(tt, na.rm = T)-tt)/(max(tt, na.rm = T)-min(tt, na.rm = T))) %>% 
  mutate(tt_norm = ifelse(is.nan(tt_norm), 0, tt_norm)) %>%
  dplyr::select(gridID, tt_norm)  %>%
  filter(gridID %in% unique(lc_raw$gridID)) 
summary(tt)
hist(tt$tt_norm)

tt_r <- rasterFromXYZ(
  left_join(grid_df, tt) %>%
    dplyr::select(x, y, tt_norm), crs = crs(grid))
tt_r[tt_r == 0] <- NA
plot(tt_r)


## Field size
# 3502 -Very large fields with an area of greater than 100 ha;
# 3503 - Large fields with an area between 16 ha and 100 ha;
# 3504 - Medium fields with an area between 2.56 ha and 16 ha;
# 3505 - Small fields with an area between 0.64 ha and 2.56 ha; and
# 3506 - Very small fields with an area less than 0.64 ha.

# Add labels to raster
fs_df <- data.frame(ID = c(0, 3502, 3503, 3504, 3505, 3506),
                    size = c("no field", 
                             "Very large fields with an area of greater than 100 ha",
                             "Large fields with an area between 16 ha and 100 ha",
                             "Medium fields with an area between 2.56 ha and 16 ha",
                             "Small fields with an area between 0.64 ha and 2.56 ha",
                             "Very small fields with an area less than 0.64 ha"))

# remove zero values for plotting
fs_lp <- fs_raw
fs_lp[fs_lp == 0] <- NA 
fs_lp <- ratify(fs_lp)
rat <- levels(fs_lp)[[1]]
rat <- rat %>% left_join(fs_df)
levels(fs_lp) <- rat
levelplot(fs_lp)

# Allocate field code to gridID
fs <- as.data.frame(rasterToPoints(stack(grid, fs_raw))) %>%
  dplyr::select(gridID, fs) %>%
  filter(gridID %in% unique(lc_raw$gridID)) 
summary(fs)

fs_r <- rasterFromXYZ(
  left_join(grid_df, fs) %>%
    dplyr::select(x, y, fs), crs = crs(grid))
fs_r[fs_r == 0] <- NA
plot(fs_r)

#mapview(fs_lp)

# NOTE Fields size map for MWI only shows very small and small plots which is not very informative as prior.
# We do not use this information.


## GAEZ suitability
# Select only relevant suit layers
suit <- subset(suit_raw, crop_system$crop_system)

# Create df
suit <- as.data.frame(rasterToPoints(stack(grid, suit))) %>%
  filter(gridID %in% unique(lc_raw$gridID)) 
summary(suit)

# There could be na values on the edges, related with the downscaling procedure, we inspect maiz_S
suit_na <- dplyr::select(suit, x, y, maiz_S) %>%
  mutate(check = ifelse(is.na(maiz_S), 1, NA)) %>%
  filter(!is.na(check))

# There also seem to be negative suit values, possible because of change in resolution. We set them to zero.
suit <- suit %>%
  dplyr::select(-x, -y) %>%
  gather(crop_system, suit, -gridID) %>%
  filter(crop_system %in% priors_base$crop_system, !is.na(gridID)) %>%
  mutate(suit = ifelse(is.na(suit), 0, suit),
         suit = ifelse(suit < 0, 0, suit))
summary(suit)


## CHECK FOR ZERO SUITABILITY IN GAEZ
# For some crops the suitability maps are 0 throughout. We replace this by a crop that is 'close'
suit_zero <- suit %>%
  group_by(crop_system) %>%
  summarize(sum_suit = sum(suit, na.rm = T)) %>%
  filter(sum_suit == 0)
suit_zero

# Check which replacements are available
# TO_UPDATE: NEED TO THINK ABOUT ALTERNATIVE SUIT IF CROP_SYSTEM IS NOT AVAILBLE AND STANDARDIZE!!
# THIS CAN BE TAKEN ALSO FROM GAEZ_RAW, i.e. crop_systems not in present selection!!!

# ofib_L is zero. We replace it by cott_L
suit <- bind_rows(
  suit %>% 
    filter(crop_system != "ofib_L"),
  suit %>%
    filter(crop_system == "cott_L") %>%
    mutate(crop_system = "ofib_L"))

# ooil_L is zero. We replace it by sunf_L
suit <- bind_rows(
  suit %>% 
    filter(crop_system != "ooil_L"),
  suit %>%
    filter(crop_system == "sunf_L") %>%
    mutate(crop_system = "ooil_L"))

# Check if all crop_systems are represented
identical(sort(unique(suit$crop_system)), sort(unique(crop_system$crop_system)))


## GAEZ potential yield
# Select only relevant gaez layers
py <- subset(py_raw, crop_system$crop_system)

# Create df
py <- as.data.frame(rasterToPoints(stack(grid, py))) %>%
  filter(gridID %in% unique(lc_raw$gridID)) 
summary(py)

# There could be na values on the edges, related with the downscaling procedure, we inspect maiz_S
py_na <- dplyr::select(py, gridID, x, y, maiz_S) %>%
  mutate(check = ifelse(is.na(maiz_S), 1, NA)) %>%
  filter(!is.na(check))

py_na <- rasterFromXYZ(dplyr::select(py_na, x, y, check))
plot(py_na$check)  
plot(adm, add = T)

# There also seem to be negative py values, possible because of change in resolution. We set them to zero.
py <- py %>%
  dplyr::select(-x, -y) %>%
  gather(crop_system, py, -gridID) %>%
  filter(crop_system %in% priors_base$crop_system, !is.na(gridID)) %>%
  mutate(py = ifelse(is.na(py), 0, py),
         py = ifelse(py < 0, 0, py))
summary(py)


## CHECK FOR ZERO SUITABILITY IN py
# For some crops the suitability maps are 0 throughout. We replace this by a crop that is 'close'
py_zero <- py %>%
  group_by(crop_system) %>%
  summarize(sum_py = sum(py, na.rm = T)) %>%
  filter(sum_py == 0)
py_zero

# Check which replacements are available
# TO_UPDATE: NEED TO THINK ABOUT ALTERNATIVE SUIT IF CROP_SYSTEM IS NOT AVAILBLE AND STANDARDIZE!!
# THIS CAN BE TAKEN ALSO FROM py_RAW, i.e. crop_systems not in present selection!!!

# ofib_L is zero. We replace it by cott_L
py <- bind_rows(
  py %>% 
    filter(crop_system != "ofib_L"),
  py %>%
    filter(crop_system == "cott_L") %>%
    mutate(crop_system = "ofib_L"))

# ooil_L is zero. We replace it by sunf_L
py <- bind_rows(
  py %>% 
    filter(crop_system != "ooil_L"),
  py %>%
    filter(crop_system == "sunf_L") %>%
    mutate(crop_system = "ooil_L"))

# Check if all crop_systems are represented
identical(sort(unique(py$crop_system)), sort(unique(crop_system$crop_system)))

# Convert to fm
py <- py %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F) %>%
  left_join(dm2fm) %>%
  mutate(py = py/t_factor) %>%
  dplyr::select(-t_factor)

# We calculate potential revenue by multiplying potential yield with national crop prices
rev <- py %>%
  left_join(price) %>%
  mutate(rev = py*price)
summary(rev)


# CREATE PRIORS
# Pre process
lc <- lc_raw %>%
  dplyr::select(gridID, lc = lc_harm)

lu_sy <- lu_sy_raw %>%
  filter(adm_level == 0) %>%
  dplyr::select(crop, system, crop_area = value)

lu_ir <- lu_ir_raw %>%
  mutate(system = "I")


############### PRIOR FOR EACH SYSTEM
## SUBSISTENCE
# We use the rural population share as prior but exclude areas where suitability is zero.
# We also remove adm where crops are not allocated by definition because stat indicates zero ha.

# crop_s
crop_s <- unique(lu_sy$crop[lu_sy$system == "S"])

# select adm without crop_l
fips_crop_s <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1, crop %in% crop_s, value == 0) %>%
    dplyr::select(crop, fips, adm, adm_level) %>%
    distinct(),
  lu_adm_raw %>%
    filter(adm_level == 2, crop %in% crop_s, value == 0) %>%
    dplyr::select(crop, fips, adm, adm_level) %>%
    distinct()) %>%
  mutate(fips_crop = paste(fips, crop, sep = "_"))

prior_s <- priors_base %>%
  filter(system == "S") %>%
  left_join(pop_rural) %>%
  left_join(suit) %>%
  group_by(crop) %>%
  mutate(
    pop_norm = if_else(suit == 0, 0, pop_norm),
    rur_pop_share = pop_norm/sum(pop_norm, na.rm = T),
    crop_system = paste(crop, system, sep = "_")) %>%
  ungroup() %>%
  left_join(lu_sy) %>%
  mutate(prior = rur_pop_share*crop_area) %>%
  dplyr::select(gridID, crop_system, prior)
summary(prior_s)

## IRRIGATION
# We use revenue and accessibility 
# We first remove adm where crops are not allocated by definition because stat indicates zero ha
# Then we normalize rev and acessibility over all crops so that an overal ranking is created. 
# Next we use equal weight geometric average as the final ranking.
# This means that crops with higher revenue and accessibility will get a higher score than crops with a lower rankings.
# The argument is that if there would be competition between crops, the crop with the highest score
# Will be allocated first.
# We rerank the combined rev and accessibility score again to it has the same scale as l and i priors.

# crop_i
crop_i <- unique(lu_sy$crop[lu_sy$system == "I"])

# We use geometric average of suitability and accessibility
# We select only ir gridID
score_i <- priors_base %>%
  filter(system == "I") %>%
  left_join(lu_ir_raw) %>%
  filter(!is.na(ir_area)) %>%
  left_join(rev) %>%
  left_join(tt) %>%
  ungroup() %>%
  mutate(rev_norm = 100*(rev-min(rev, na.rm = T))/(max(rev, na.rm = T)-min(rev, na.rm = T)),
         score = (rev_norm*tt_norm)^0.5,
         score = 100*(score-min(score, na.rm = T))/(max(score, na.rm = T)-min(score, na.rm = T))) %>%
  dplyr::select(gridID, crop_system, score)
summary(score_i)


### LOW INPUT
# We use suitability for only for L
# We first remove adm where crops are not allocated by definition because stat indicates zero ha
# Then we normalize over all crops so that an overal ranking is created. 
# This means that crops with higher suitability will get a higher prior than crops with a lower suitability.
# The argument is that if there would be competition between crops, the crop with the highest suitability
# Will be allocated first

# crop_l
crop_l <- unique(lu_sy$crop[lu_sy$system == "L"])

# select adm without crop_l
fips_crop_l <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1, crop %in% crop_l, value == 0) %>%
    dplyr::select(crop, fips, adm, adm_level) %>%
    distinct(),
  lu_adm_raw %>%
    filter(adm_level == 2, crop %in% crop_l, value == 0) %>%
    dplyr::select(crop, fips, adm, adm_level) %>%
    distinct()) %>%
  mutate(fips_crop = paste(fips, crop, sep = "_"))

# We use suitability only for L 
score_l <- priors_base %>%
  filter(system == "L") %>%
  left_join(adm_r) %>%
  mutate(fips1_crop = paste(fips1, crop, sep = "_"),
         fips2_crop = paste(fips2, crop, sep = "_")) %>%
  filter((!fips1_crop %in% fips_crop_l$fips_crop) &
           (!fips2_crop %in% fips_crop_l$fips_crop)) %>%
  left_join(suit) %>%
  ungroup() %>%
  mutate(score = 100*(suit-min(suit, na.rm = T))/(max(suit, na.rm = T)-min(suit, na.rm = T))) %>%
  dplyr::select(gridID, crop_system, score)
summary(score_l)  


## HIGH INPUT
# We use the same score as for I

# crop_h
crop_h <- unique(lu_sy$crop[lu_sy$system == "H"])

# select adm without crop_l
fips_crop_h <- bind_rows(
  lu_adm_raw %>%
    filter(adm_level == 1, crop %in% crop_h, value == 0) %>%
    dplyr::select(crop, fips, adm, adm_level) %>%
    distinct(),
  lu_adm_raw %>%
    filter(adm_level == 2, crop %in% crop_h, value == 0) %>%
    dplyr::select(crop, fips, adm, adm_level) %>%
    distinct()) %>%
  mutate(fips_crop = paste(fips, crop, sep = "_"))


# We use geometric average of rev and accessibility
score_h <- priors_base %>%
  filter(system == "H") %>%
  left_join(adm_r) %>%
  mutate(fips1_crop = paste(fips1, crop, sep = "_"),
         fips2_crop = paste(fips2, crop, sep = "_")) %>%
  filter((!fips1_crop %in% fips_crop_h$fips_crop) &
           (!fips2_crop %in% fips_crop_h$fips_crop)) %>%
  left_join(rev) %>%
  left_join(tt) %>%
  ungroup() %>%
  mutate(rev_norm = 100*(rev-min(rev, na.rm = T))/(max(rev, na.rm = T)-min(rev, na.rm = T)),
         score = (rev_norm*tt_norm)^0.5,
         score = 100*(score-min(score, na.rm = T))/(max(score, na.rm = T)-min(score, na.rm = T))) %>%
  dplyr::select(gridID, crop_system, score)
summary(score_h)


### CALCULATE PRIORS USING RELATIVE SHARES OF RESIDUAL AREA AFTER S
# Residual grid area
resid_area <- prior_s %>%
  group_by(gridID) %>%
  summarize(prior_s = sum(prior, na.rm = T)) %>%
  ungroup() %>%
  left_join(lc,.) %>%
  mutate(
    prior_s = if_else(is.na(prior_s), 0, prior_s),
    resid = if_else(lc-prior_s < 0, 0, lc-prior_s)) %>%
  dplyr::select(gridID, resid)
summary(resid_area)

# Distribute residual area over I, L, H systems using score as weight.
prior_i_l_h <-bind_rows(score_i, score_l, score_h) %>%
  spread(crop_system, score, fill = 0) %>% # add 0 as fill
  gather(crop_system, score, -gridID) %>%
  group_by(gridID) %>%
  mutate(score_share = score/sum(score, na.rm = T)) %>%
  ungroup() %>%
  full_join(resid_area) %>%
  mutate(prior = resid * score_share) %>%
  dplyr::select(gridID, crop_system, prior) %>%
  filter(prior > 0)


############### COMBINE ###############

# combine with prior_s
prior_df <- bind_rows(prior_s, prior_i_l_h) %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F) %>%
  filter(prior > 0)
summary(prior_df)

# view_4p_f("acof", "prior", df = prior_df)
# view_4p_f("maiz", "prior", df = prior_df)
# view_4p_f("rice", "prior", df = prior_df)
# view_4p_f("vege", "prior", df = prior_df)

# Use number of lc grid cells as scaling factor
scalelp <- NROW(filter(lc_raw))

# calculate shares
prior_df <- prior_df %>%
  group_by(crop_system) %>%
  mutate(prior_share = prior/sum(prior,na.rm = T),
         prior_scaled = prior_share * scalelp) %>%
  ungroup()

# save
saveRDS(prior_df, file.path(proc_path, paste0("harmonized/prior_harm_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

