#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to harmonize physical area, cropland and irrigated area information
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster", "mapview", "gdxrrw")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))


############### LINK GAMS LIBRARIES ###############
# R will automatically find the location of GAMS if this is stored in your system (see documentation).
# If for some reason this does not work you can set it specifically, e.g. igdx("C:/Program Files/GAMS/win64/24.6")
igdx("")


############### SET ADM IN LINE WITH SOLVE_SEL ###############
# Adm list
adm_list <- read_csv(file.path(proc_path, glue("lists/adm_map_list_{year_sel}_{iso3c_sel}.csv")))

if(solve_sel == 0) {
  adm_code_list <- unique(adm_list$adm0_code)
} else {
  adm_code_list <- unique(adm_list$adm1_code)
}


############### PREPARE MODEL INPUT AT ADM LEVEL ###############
# Function to prepare model input in line with solve_sel
prep_model_input <- function(adm_code_sel){
  
  message(glue("Prepare model input for {adm_code_sel}"))
  
  
  ############### LOAD DATA ###############
  # Physical area
  pa_raw <- read_csv(file.path(proc_path, glue("harmonized/{adm_code_sel}/pa_{year_sel}_{adm_code_sel}_{iso3c_sel}.csv"))) 
  
  # Farming system
  pa_fs_raw <- read_csv(file.path(proc_path, glue("harmonized/{adm_code_sel}/pa_fs_{year_sel}_{adm_code_sel}_{iso3c_sel}.csv"))) 
  
  # Synergy cropland
  cl <- readRDS(file.path(proc_path, glue("harmonized/{adm_code_sel}/cl_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  
  # Synergy irrigated area
  cl_ir <- readRDS(file.path(proc_path, glue("harmonized/{adm_code_sel}/cl_ir_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  
  # rur_pop_s
  rur_pop_share <- readRDS(file.path(proc_path, glue("harmonized/{adm_code_sel}/rur_pop_share_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  
  # Score
  score_raw <- readRDS(file.path(proc_path, glue("harmonized/{adm_code_sel}/score_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.rds")))
  
  # prior
  #prior_raw <- readRDS(file.path(proc_path, paste0("harmonized/prior_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))
  
  # Adm list
  adm_list_raw <- read_csv(file.path(proc_path, glue("lists/adm_map_list_{year_sel}_{iso3c_sel}.csv")))
  
  
  ############### PREPARATIONS ###############
  # only select adm_code
  adm_code_list <- adm_list_raw %>%
    dplyr::select(adm0_code, adm1_code, adm2_code)
  
  # create adm long list
  adm_list_long <- pa_raw %>%
    gather(crop, pa, -adm_code, -adm_name, -adm_level) %>%
    dplyr::select(adm_code, adm_name, adm_level) %>%
    unique()
  
  # Put statistics in long format and filter out crops where pa = 0
  # These crops create artificial adms, which created conflicts
  pa <- pa_raw %>%
    gather(crop, pa, -adm_code, -adm_name, -adm_level) %>% 
  filter(pa != 0)
  
  pa_fs <- pa_fs_raw %>%
    gather(crop, pa, -adm_code, -adm_name, -adm_level, -system) %>% 
    filter(pa != 0)
  
  
  ### DETERMINE ADM CONSTRAINTS AND CREATE ARTIFICIAL ADMS
  
  #####################################################################################################################
  #'
  #' From a theoretical perspective there are two ways to deal with incomplete adm information in the GAMS code. 
  #' (1) Use a nested approach where constraints are added for adm2, adm1 and if adm1 is not complete, adm0 levels per crop.
  #' (2) Introduce artificial adms at adm2 level per crop that union adms with missing information. 
  #' 
  #'  We use the second approach that is also used by SPAM because the first approach seems difficult to implement in GAMS.
  #'  Problem is that the first approach requires that we introduce explicit constraints for adm1 crop combinations where 
  #'  know that the area is zero. As GAMS does not allow for zeros in data files, this is quite cumbersome. 
  #'  THe only way would be to filter out all gridID for adms where area is zero. This requires a lot of coding. 
  #'  Hence it seems easier to introduce the artificial adms. 
  #'  
  #'  The approach is dependent on adm_sel. For now we simply add code for adm 0, 1 and 3
  #'  In the future this might be generalized for all possible adms (although max is probably 2)
  #'  
  #####################################################################################################################
  
  
  
  
  ### ARTIFICIAL ADM FOR adm_sel = 0
  
  if(adm_sel == 0){
    
    # Create all adm combinations at adm_sel
    adm_base <- adm_list_long %>%
      filter(adm_level == adm_sel)
    
    base <- expand.grid(adm0_code = unique(adm_base$adm_code), crop = unique(pa$crop), stringsAsFactors = F) %>%
      mutate(adm_level = adm_sel) %>%
      left_join(adm_code_list)
    
    adm0_pa <- pa %>%
      filter(adm_level == 0) %>%
      rename(adm0_code = adm_code, pa_adm0 = pa) %>%
      dplyr::select(-adm_name, -adm_level) 
    
    adm0_1_2 <- left_join(base, adm0_pa) %>%
      dplyr::select(crop, adm0_code, pa_adm0) %>%
      unique
    
    # Prepare artificial adm. With only adm0 level data there are none.
    adm_art <- adm0_1_2 %>%
      dplyr::select(adm_code = adm0_code, crop = crop, pa = pa_adm0)
    
    adm_art_map <- data.frame(adm_code = unique(adm0_1_2$adm0_code), fips_art = unique(adm0_1_2$adm0_code))
    
  }
  
  
  ### ARTIFICIAL ADM FOR adm_sel = 1
  
  if(adm_sel == 1){
    
    ### PREPARE DATA
    # Create all adm combinations at adm_sel
    adm_base <- adm_list_long %>%
      filter(adm_level == adm_sel)
    
    base <- expand.grid(adm1_code = unique(adm_base$adm_code), crop = unique(pa$crop), stringsAsFactors = F) %>%
      mutate(adm_level = adm_sel) %>%
      left_join(adm_code_list)
    
    adm0_pa <- pa %>%
      filter(adm_level == 0) %>%
      rename(adm0_code = adm_code, pa_adm0 = pa) %>%
      dplyr::select(-adm_name, -adm_level) 
    
    adm1_pa <- pa %>%
      filter(adm_level == 1) %>%
      rename(adm1_code = adm_code, pa_adm1 = pa) %>%
      dplyr::select(-adm_name, -adm_level)
    
    ### ADM0_1 ARTIFICIAL UNITS
    # Combine adm 0_1 data
    adm0_1 <- left_join(base, adm1_pa) %>%
      left_join(adm0_pa) %>%
      dplyr::select(crop, adm0_code, adm1_code, pa_adm0, pa_adm1) %>%
      unique
    
    # Prepare artificial adm combining adm0 and adm1
    adm1_art <- adm0_1 %>%
      group_by(adm0_code, crop) %>%
      mutate(adm1_av = sum(pa_adm1, na.rm = T),
             imp_adm1 = ifelse(is.na(pa_adm1), unique(pa_adm0) - adm1_av, pa_adm1)) %>%
      ungroup() %>%
      mutate(adm1_code_art = ifelse(is.na(pa_adm1), paste(adm0_code, "ART1", crop, sep = "_"), adm1_code)) %>%
      dplyr::select(adm1_code_art, adm1_code, crop, imp_adm1) %>%
      unique
    
    adm_art <- adm1_art %>%
      dplyr::select(crop, imp_adm1, adm1_code_art) %>%
      unique %>%
      rename(adm_code = adm1_code_art, pa = imp_adm1)
    
    # artificial adm mapping
    adm_art_map <- adm1_art %>%
      dplyr::select(adm_code_art = adm1_code_art, adm_code = adm1_code) %>%
      unique
  }
  
  
  ### ARTIFICIAL ADM FOR adm_sel = 2 and solve_sel = 0
  
  if(adm_sel == 2 & solve_sel == 0){
    
    ### PREPARE DATA
    # Create all adm combinations at adm_sel
    adm_base <- adm_list_long %>%
      filter(adm_level == adm_sel)
    
    base <- expand.grid(adm2_code = unique(adm_base$adm_code), crop = unique(pa$crop), stringsAsFactors = F) %>%
      mutate(adm_level = adm_sel) %>%
      left_join(adm_code_list)
    
    adm0_pa <- pa %>%
      filter(adm_level == 0) %>%
      rename(adm0_code = adm_code, pa_adm0 = pa) %>%
      dplyr::select(-adm_name, -adm_level) 
    
    adm1_pa <- pa %>%
      filter(adm_level == 1) %>%
      rename(adm1_code = adm_code, pa_adm1 = pa) %>%
      dplyr::select(-adm_name, -adm_level)
    
    adm2_pa <- pa %>%
      filter(adm_level == 2) %>%
      rename(adm2_code = adm_code, pa_adm2 = pa) %>%
      dplyr::select(-adm_name, -adm_level) 
    
    
    ### ADM0_1 ARTIFICIAL UNITS
    # Combine adm 0_1 data
    adm0_1 <- left_join(base, adm1_pa) %>%
      left_join(adm0_pa) %>%
      dplyr::select(crop, adm0_code, adm1_code, pa_adm0, pa_adm1) %>%
      unique
    
    # Prepare artificial adm combining adm0 and adm1
    adm1_art <- adm0_1 %>%
      group_by(adm0_code, crop) %>%
      mutate(adm1_av = sum(pa_adm1, na.rm = T),
             imp_adm1 = ifelse(is.na(pa_adm1), unique(pa_adm0) - adm1_av, pa_adm1)) %>%
      ungroup() %>%
      mutate(adm1_code_art = ifelse(is.na(pa_adm1), paste(adm0_code, "ART1", crop, sep = "_"), adm1_code)) %>%
      dplyr::select(adm1_code_art, adm1_code, crop, imp_adm1) %>%
      unique
    
    
    ### ADM1_2 ARTIFICIAL UNITS
    # We take the new adm1 list including artificial adms as basis
    
    # Combine adm 1_2 data
    adm1_2 <- left_join(base, adm2_pa) %>%
      left_join(adm1_pa) %>%
      dplyr::select(crop, adm1_code, adm2_code, pa_adm2) %>%
      unique
    
    # Prepare artificial adm combining adm0 and adm1
    adm2_art <- adm1_2 %>%
      left_join(adm1_art) %>%
      group_by(adm1_code_art, crop) %>%
      mutate(adm2_av = sum(pa_adm2, na.rm = T),
             imp_adm2 = ifelse(is.na(pa_adm2), unique(imp_adm1) - adm2_av, pa_adm2)) %>%
      ungroup() %>%
      mutate(adm2_code_art = ifelse(is.na(pa_adm2), paste(adm1_code_art, "ART2", crop, sep = "_"), adm2_code)) %>%
      unique() 
    
    adm_art <- adm2_art %>%
      dplyr::select(crop, imp_adm2, adm2_code_art) %>%
      unique %>%
      rename(adm_code = adm2_code_art, pa = imp_adm2)
    
    # artificial adm mapping
    adm_art_map <- adm2_art %>%
      dplyr::select(adm_code_art = adm2_code_art, adm_code = adm2_code) %>%
      unique
  }
  
  ### ARTIFICIAL ADM FOR adm_sel = 2 and adm_solve = 1
  
  if(adm_sel == 2 & solve_sel == 1){
    
    ### PREPARE DATA
    # Create all adm combinations at adm_sel
    adm_base <- adm_list_long %>%
      filter(adm_level == adm_sel)
    
    base <- expand.grid(adm2_code = unique(adm_base$adm_code), crop = unique(pa$crop), stringsAsFactors = F) %>%
      mutate(adm_level = adm_sel) %>%
      left_join(adm_code_list)
    
    adm1_pa <- pa %>%
      filter(adm_level == 1) %>%
      rename(adm1_code = adm_code, pa_adm1 = pa) %>%
      dplyr::select(-adm_name, -adm_level)
    
    adm2_pa <- pa %>%
      filter(adm_level == 2) %>%
      rename(adm2_code = adm_code, pa_adm2 = pa) %>%
      dplyr::select(-adm_name, -adm_level) 
    
    
    ### ADM1_2 ARTIFICIAL UNITS
    # We take the new adm1 list including artificial adms as basis
    
    # Combine adm 1_2 data
    adm1_2 <- left_join(base, adm2_pa) %>%
      left_join(adm1_pa) %>%
      dplyr::select(crop, adm1_code, adm2_code, pa_adm1, pa_adm2) %>%
      unique
    
    # Prepare artificial adm combining adm0 and adm1
    adm2_art <- adm1_2 %>%
      group_by(adm1_code, crop) %>%
      mutate(adm2_av = sum(pa_adm2, na.rm = T),
             imp_adm2 = ifelse(is.na(pa_adm2), unique(pa_adm1) - adm2_av, pa_adm2)) %>%
      ungroup() %>%
      mutate(adm2_code_art = ifelse(is.na(pa_adm2), paste(adm1_code, "ART2", crop, sep = "_"), adm2_code)) %>%
      unique() 
    
    adm_art <- adm2_art %>%
      dplyr::select(crop, imp_adm2, adm2_code_art) %>%
      unique %>%
      rename(adm_code = adm2_code_art, pa = imp_adm2)
    
    # artificial adm mapping
    adm_art_map <- adm2_art %>%
      dplyr::select(adm_code_art = adm2_code_art, adm_code = adm2_code) %>%
      unique
  }
  
  # Check if totals add up
  all.equal(sum(adm_art$pa), sum(pa$pa[pa$adm_code == adm_code_sel])) 
  
  # Replace very small negative numbers which might occur because of rounding by 0
  adm_art <- adm_art %>%
    mutate(pa = if_else(pa < 0, 0, pa))
  unique(adm_art$adm_code)
  
  
  ############### CREATE GAMS PARAMETERS ###############
  # adm_area(k,s): Land use per lowest level adm, including artificial adms (k) and crop (s). 
  adm_area <- adm_art %>%
    dplyr::select(adm_code, crop, pa)
  
  adm_area_gdx <- para_gdx(adm_area, c("adm_code", "crop"), "adm_area", "Crop area per adm")
  
  
  # lc(i): Crop cover for each gridcell (i)
  cl_m <- cl %>%
    dplyr::select(gridID, cl)
  
  cl_gdx <- para_gdx(cl_m, c("gridID"), "cl", "Cropland per grid cell")
  
  
  # crop_area(j): Total area per crop system (j)
  crop_area <- pa_fs %>%
    filter(adm_code == adm_code_sel) %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    ungroup() %>%
    dplyr::select(crop_system, pa)
  
  crop_area_gdx <- para_gdx(crop_area, c("crop_system"), "crop_area", "Total area per crop")
  
  
  # ir_area(i): Irrigated area per grid cell (i)
  ir_area <- cl_ir %>%
    filter(gridID %in% unique(cl$gridID)) %>%
    dplyr::select(gridID, cl_ir)
  
  ir_area_gdx <- para_gdx(ir_area, c("gridID"), "ir_area", "Irrigated area per grid cell")
  
  
  # ir_crop(j): Total irrigated area per crop system (j)
  ir_crop <- pa_fs %>%
    filter(adm_code == adm_code_sel) %>%
    filter(system == "I") %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    ungroup() %>%
    dplyr::select(crop_system, pa)
  
  ir_crop_gdx <- para_gdx(ir_crop, c("crop_system"), "ir_crop", "Total irrigated area per crop")
  
  
  # score(i,j): Score per grid cell and crop_system
  score <- score_raw %>%
    dplyr::select(gridID, crop_system, score)
  
  score_gdx <- para_gdx(score, c("gridID", "crop_system"), "score", "score per grid cell and crop_system")
  
  # # prior(i,j): prior per grid cell and crop_system
  # prior <- prior_raw %>%
  #   dplyr::select(gridID, crop_system, prior_scaled)
  # 
  # prior_gdx <- para_gdx(prior, c("gridID", "crop_system"), "prior", "scaled prior per grid cell and crop_system")
  
  
  # rur_pop_s(i,j): Rural population share per grid cell
  rur_pop_share_gdx <- para_gdx(rur_pop_share, c("gridID", "crop_system"), "rur_pop_share", "Rural population shares")
  
  
  ### CREATE GAMS SETS
  # Grid cells (i)
  grid_s <- cl %>%
    dplyr::select(gridID) %>%
    unique()
  
  grid_s_gdx <- set_gdx(grid_s, c("gridID"), "i", "Grid cells")
  
  
  # Crops system combinations (j)
  crop_system_s <- pa_fs %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    filter(adm_code == adm_code_sel) %>%
    ungroup() %>%
    dplyr::select(crop_system) %>%
    unique() 
  
  crop_system_s_gdx <- set_gdx(crop_system_s, c("crop_system"), "j", "Crop systems")
  
  
  # Crop (s)
  crop_s <- adm_art %>%
    dplyr::select(crop) %>%
    unique() 
  
  crop_s_gdx <- set_gdx(crop_s, c("crop"), "s", "Crops")
  
  # Subsistence system
  s_system_s <- pa_fs %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    filter(adm_code == adm_code_sel, system == "S") %>%
    ungroup() %>%
    dplyr::select(crop_system) %>%
    unique() 
  
  s_system_s_gdx <- set_gdx(s_system_s, c("crop_system"), "j_s", "Subsistence system combinations")
  
  
  # Adms with statistics (k) 
  adm_s <- adm_area %>%
    dplyr::select(adm_code) %>%
    unique()
  
  adm_s_gdx <- set_gdx(adm_s, c("adm_code"), "k", "Administrative regions")
  
  
  # Crops with corresponding crop system combinations (s,j)  
  crop_crop_system_s <- pa_fs %>%
    mutate(crop_system = paste(crop, system, sep = "_")) %>%
    filter(adm_code == adm_code_sel) %>%
    ungroup() %>%
    dplyr::select(crop, crop_system) %>%
    unique()
  
  crop_crop_system_s_gdx <- set_gdx(crop_crop_system_s, c("crop","crop_system"), "n", "Crops with corresponding system combinations")
  
  
  # Administrative regions with corresponding grid cells (k,i)   
  adm_grid_s <- cl %>% 
    left_join(adm_art_map) %>%
    dplyr::select(adm_code = adm_code_art, gridID) %>%
    unique() 
  
  adm_grid_s_gdx <- set_gdx(adm_grid_s, c("adm_code","gridID"), "l", "adm with corresponding grid cells")
  
  
  # Administrative regions with corresponding crops
  adm_crop_s <- adm_area %>%
    dplyr::select(adm_code, crop) %>%
    unique() 
  
  adm_crop_s_gdx <- set_gdx(adm_crop_s, c("adm_code", "crop"), "m", "adm with corresponding crops")
  
  
  ############### CREATE GAMS SCALARS ###############
  # scalef: number of grid cells to scale optimization so numbers do not get too small
  scalef <- nrow(grid_s)
  scalef_gdx <- scalar_gdx(scalef, "scalef", "Scaling factor")
  
  
  ############### SAVE ###############
  temp_path <- file.path(proc_path, glue("harmonized/{adm_code_sel}"))
  dir.create(temp_path, recursive = T, showWarnings = F)
  
  # GDX
  wgdx(file.path(temp_path, glue("input_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.gdx")),
       cl_gdx, 
       adm_area_gdx, 
       ir_crop_gdx, 
       ir_area_gdx, 
       crop_area_gdx,
       s_system_s_gdx,
       score_gdx,
       grid_s_gdx, crop_system_s_gdx, 
       adm_s_gdx, 
       crop_crop_system_s_gdx, 
       adm_grid_s_gdx,
       adm_crop_s_gdx, crop_s_gdx, 
       rur_pop_share_gdx,
       scalef_gdx)
  
}

walk(adm_code_list, prep_model_input)

############### CLEAN UP ###############
rm(adm_area, adm_art, adm_art_map, adm_base, adm_code_list, adm_crop_s, adm_grid_s, adm_list_long, adm_list_raw, adm_s,
   adm0_1, adm0_pa, adm1_2, adm1_art, adm2_art, adm2_pa, base, cl, cl_ir, cl_m, crop_area, crop_crop_system_s, crop_s,
   crop_system_s, grid_s, ir_area, ir_crop, pa, pa_fs, pa_fs_raw, pa_raw, rur_pop_share, s_system_s, score, score_raw, scalef)
rm(list = ls()[grep("_gdx", ls())])
   
   