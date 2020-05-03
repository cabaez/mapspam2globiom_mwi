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

load_data("grid", param)

############### PROCESS ###############
# select country simu
simu <- simu_global %>%
  filter(COUNTRY == param$iso3n)
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
rm(grid, simu, simu_r, simu_global)



