#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Analyze model results
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


############### LINK GAMS LIBRARIES ###############
igdx(gams_path)

############### LOAD DATA ###############
### MODEL OUTPUT
# model output file
output_file <- file.path(model_path, glue("output_{model_sel}_{grid_sel}_{year_sel}_{iso3c_sel}"))

# Allocation
alloc <- rgdx.param(output_file, "palloc", names = c("gridID", "crop_system", "alloc"),  compress = T) %>%
  mutate(gridID = as.numeric(as.character(gridID))) %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F)

# ir slack
ir_slack_raw <- rgdx.param(output_file, "ir_slack_l", names = c("gridID", "ir_slack"),  compress = T) %>%
  mutate(gridID = as.numeric(as.character(gridID)))

# cl slack
cl_slack_raw <- rgdx.param(output_file, "cl_slack_l", names = c("gridID", "cl_slack"),  compress = T) %>%
  mutate(gridID = as.numeric(as.character(gridID)))

# adm slack
adm_slack_raw <- rgdx.param(output_file, "adm_slack_l", names = c("adm_code", "crop", "sign", "adm_slack"),  compress = T) 


### MODEL_INPUT
# Adm
adm <- readRDS(file.path(proc_path, glue("maps/adm/adm_{year_sel}_{iso3c_sel}.rds")))

# Adm_r
adm_r <- readRDS(file.path(proc_path, glue("maps/adm/adm_r_{grid_sel}_{year_sel}_{iso3c_sel}.rds"))) 

# Grid
grid <- raster(file.path(proc_path, glue("maps/grid/grid_{grid_sel}_r_{year_sel}_{iso3c_sel}.tif")))
names(grid) <- "gridID"

# Physical area
pa_raw <- read_csv(file.path(proc_path, glue("harmonized/pa_{year_sel}_{iso3c_sel}.csv"))) 

# Farming system
pa_fs_raw <- read_csv(file.path(proc_path, glue("harmonized/pa_fs_{year_sel}_{iso3c_sel}.csv"))) 

# Synergy cropland
cl_raw <- readRDS(file.path(proc_path, glue("harmonized/cl_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# Synergy irrigated area
cl_ir_raw <- readRDS(file.path(proc_path, glue("harmonized/cl_ir_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# Score
score_raw <- readRDS(file.path(proc_path, glue("harmonized/score_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))


############### PREPARATIONS ###############
# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid))

# Combine all relevant variables
db <- score_raw %>%
  dplyr::select(gridID, crop, system, score) %>%
  full_join(alloc) %>%
  left_join(adm_r) %>%
  left_join(cl_ir_raw) %>%
  left_join(ir_slack_raw) %>%
  left_join(cl_slack_raw) 
summary(db)

# Put statistics in long format
pa <- pa_raw %>%
  gather(crop, pa, -adm_code, -adm_name, -adm_level)

pa_fs <- pa_fs_raw %>%
  gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)

# # gmia df
# grid_size <- area(grid)
# names(grid_size) <- "grid_size"
# gmia <- as.data.frame(rasterToPoints(stack(grid, grid_size, gmia_01_raw))) %>%
#   mutate(gmia_area = grid_size * gmia * 100) %>%
#   filter(!is.na(gmia_area)) %>%
#   dplyr::select(gridID, gmia_area)


############### ANALYSE IR SLACK ###############
# ir slack db
# We only include I system to exclude non-I crops allocated in the same grid cell.
ir_slack_db <- db %>%
  left_join(grid_df) %>%
  filter(!is.na(ir_slack) & !is.na(alloc) & system == "I") %>%
  group_by(gridID) 
sum(ir_slack_db$ir_slack, na.rm = T)

# ir_slack by crop and region
ir_slack_db %>%
  group_by(crop, adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarize(ir_slack = sum(ir_slack, na.rm = T))

# ir_slack by crop and I system
ir_slack_db %>%
  filter(system == "I") %>%
  group_by(crop, system) %>%
  summarize(ir_slack = sum(ir_slack, na.rm = T))

ir_slack_crop_system <- filter(ir_slack_db) %>%
  dplyr::select(gridID, crop_system, alloc, ir_slack) %>%
  spread(crop_system, alloc)

# ir_slack by region
ir_slack_db %>%
  dplyr::select(ir_slack, adm1_name, adm1_code, adm2_name, adm2_code) %>%
  unique() %>%
  group_by(adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarize(ir_slack = sum(ir_slack, na.rm = T))

# Ir slack map
ir_slack_r <- rasterFromXYZ(left_join(grid_df, ir_slack_db) %>% 
                              dplyr::select(x, y, ir_slack))
crs(ir_slack_r) <- crs(adm)

# lu_ir_harm map
cl_ir_map <- left_join(grid_df, cl_ir_raw) %>%
  filter(!is.na(cl_ir)) 

cl_ir_r <- rasterFromXYZ(cl_ir_map %>%
                                dplyr::select(x, y, cl_ir))
crs(cl_ir_r) <- crs(adm)

# Visualise
mapview(ir_slack_r, map.types = "CartoDB.DarkMatter", col.regions = "green", maxpixels =  1500000) +
  mapview(adm, alpha.regions = 0) +
  mapview(cl_ir_r, maxpixels =  1500000) +
  mapview(gia_raw) +
  mapview(gmia_01_raw)
+
  mapview(det_all, zcol = "category")

sync(mapview(ir_slack_r), mapview(gmia_01_raw))


############### ANALYSE LC SLACK ###############
# ir slack db
cl_slack_db <- db %>%
  left_join(grid_df) %>%
  filter(!is.na(cl_slack)) %>%
  dplyr::select(gridID, cl_slack, adm1_name, adm1_code, adm2_name, adm2_code, x, y) %>%
  unique()
sum(cl_slack_db$cl_slack, na.rm = T)  

# ir_slack by region
cl_slack_db %>%
  group_by(adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarize(cl_slack = sum(cl_slack, na.rm = T))

# Ir slack map
cl_slack_r <- rasterFromXYZ(cl_slack_db %>% 
                              dplyr::select(x, y, cl_slack))
crs(cl_slack_r) <- crs(adm)

# Visualise
mapview(cl_slack_r, map.types = "CartoDB.DarkMatter", col.regions = "green", maxpixels =  1500000) +
  mapview(adm, alpha.regions = 0) 


### STAT
# Irrigated stat
pa_fs_ir <- pa_fs %>%
  filter(adm_level == 0) %>%
  spread(system, pa) %>%
  filter(I >0) %>%
  arrange(desc(I))

# ir slack at adm2
ir_slack_adm2 <- grid_df %>%
  left_join(adm_r) %>%
  left_join(ir_slack_raw) %>%
  left_join(cl_ir_raw) %>%
  group_by(adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarise_at(c("ir_slack", "cl_ir"), sum, na.rm = T) %>%
  arrange(desc(ir_slack))

# compare stat with ir_slack at adm2
comp_ir_adm2 <- pa %>% 
  filter(adm_level == 2, crop %in% pa_fs$crop[pa_fs$system == "I"], adm_name %in% ir_slack_raw$adm2_name, pa > 0) %>%
  left_join(ir_slack_db %>%
              group_by(adm2_name) %>%
              summarize(ir_slack = sum(ir_slack, na.rm = T)) %>%
              rename(adm_name = adm2_name)) %>%
  arrange(adm_name)


############### CHECK ADM SLACK ###############
# Compare total adm with total allocation
adm0_check <- pa %>%
  filter(adm_level == 0) %>%
  dplyr::select(crop, adm_code, adm_name, pa) %>%
  left_join(
    db %>%
      group_by(crop) %>%
      summarize(alloc = sum(alloc, na.rm = T))) 
all.equal(adm0_check$pa, adm0_check$alloc)

adm1_check <- pa %>%
  filter(adm_level == 1) %>%
  dplyr::select(crop, adm1_code = adm_code, adm1_name = adm_name, pa) %>%
  left_join(
    db %>%
      group_by(crop, adm1_code, adm1_name) %>%
      summarize(alloc = sum(alloc, na.rm = T))) %>%
  drop_na() %>%
  mutate(dif = pa - alloc) %>%
  arrange(dif)
all.equal(adm1_check$pa, adm1_check$alloc)

adm2_check <- pa %>%
  filter(adm_level == 2) %>%
  dplyr::select(crop, adm2_code = adm_code, adm2_name = adm_name, pa) %>%
  left_join(
    db %>%
      group_by(crop, adm2_code, adm2_name) %>%
      summarize(alloc = sum(alloc, na.rm = T))) %>%
  drop_na() %>%
  mutate(dif = pa - alloc) %>%
  arrange(dif)
all.equal(adm2_check$pa, adm2_check$alloc)


############### VISUALIZE RESULTS ###############
# Crop allocation
view_4p_f("vege", "alloc", df = db)
view_st_f("maiz", "alloc", "S")

# Across systems
by_system <- db %>%
  group_by(gridID, system) %>%
  summarize(alloc = plus(alloc, na.rm = T)) %>%
  ungroup() %>%
  mutate(crop = "total")

view_4p_f("total", "alloc", df = by_system)


### COMPARE Proirs with alloc
# Compare prior area total with allocated area total
# CHECK WHY WHEA H and I are 8 different?
check_alloc_tot <- db %>%
  group_by(crop_system) %>%
  summarize(tot_prior_area = sum(prior_area, na.rm = T),
            tot_alloc  = sum(alloc, na.rm = T)) %>%
  mutate(dif = tot_prior_area-tot_alloc)




