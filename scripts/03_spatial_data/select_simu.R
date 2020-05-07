#'========================================================================================================================================
#' Project:  mapspam2globiom
#' Subject:  Code to select simu
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### LOAD DATA ###############
# simu global map
simu_global <- st_read(file.path(param$raw_path, "Simu/simu_global.shp"))

# Simu area
simu_area <- read_csv(file.path(param$raw_path, "simu/simu_area.csv"))

load_data("grid", param)


############### PROCESS ###############
# select country simu and add area info
simu <- simu_global %>%
  filter(COUNTRY == param$iso3n) %>%
  left_join(simu_area)
plot(simu$geometry)

# rasterize
simu_r <- rasterize(simu, grid, field = "SimUID")
plot(simu_r)


############### SAVE ###############
temp_path <- file.path(param$spam_path, glue::glue("processed_data/maps/simu/{param$res}"))
dir.create(temp_path, showWarnings = F, recursive = T)

saveRDS(simu, file.path(temp_path, glue::glue("simu_{param$res}_{param$year}_{param$iso3c}.rds")))
writeRaster(simu_r, file.path(temp_path, glue::glue("simu_r_{param$res}_{param$year}_{param$iso3c}.tif")), overwrite = T)


### CLEAN UP
rm(grid, simu, simu_area, simu_r, simu_global, temp_path)



