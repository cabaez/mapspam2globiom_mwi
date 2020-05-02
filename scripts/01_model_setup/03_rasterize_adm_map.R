#'========================================================================================================================================
#' Project:  mapspam2globiom
#' Subject:  Code to rasterize adm
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### LOAD DATA ###############
load_data(c("adm_map", "grid"), param)


############### CREATE RASTER OF ADMS WITH GRIDID ###############
# Rasterize adm
# getCover ensures all grid cells covered are rasterized, not only where the center is covered by the polyon.
# Otherwise grid cells might get lost.
adm_map_r <- rasterize(adm_map, grid)
names(adm_map_r) <- "ID"
plot(adm_map_r)

# Get adm info
if(param$adm_level == 0){
  adm_df <- levels(adm_map_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code)
} else if(param$adm_level == 1){
  adm_df <- levels(adm_map_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code, adm1_name, adm1_code)
} else if(param$adm_level == 2){
  adm_df <- levels(adm_map_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)
}

# stack
adm_map_r <- stack(grid, adm_map_r)

# Create data.frame, remove cells outside border and add adm names
adm_map_r <- as.data.frame(rasterToPoints(adm_map_r)) %>%
  left_join(adm_df, by = "ID") %>%
  na.omit %>%
  dplyr::select(-ID, -x, -y)


############### SAVE ###############
# Save
temp_path <- file.path(param$spam_path, glue("processed_data/maps/adm/{param$res}"))
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
saveRDS(adm_map_r, file.path(temp_path,
  glue("adm_map_r_{param$res}_{param$year}_{param$iso3c}.rds")))


############### CLEAN UP ###############
rm(grid, adm_df, adm_map, adm_map_r)
