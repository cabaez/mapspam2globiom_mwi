#'========================================================================================================================================
#' Project:  mapspam2globiom
#' Subject:  Script to convert mapspam crop distribution maps to globiom simu input
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### CREATE GLOBIOM INPUT GDX FILES ###############
# We use the ESACCI land cover map as land cover base map. 
# The user can replace this by a country specific product if available.
# If so, a new land_cover2globiom land cover class has to be procuced and loaded
# that substitues the esacci2globiom mapping.
lc_file <- file.path(param$spam_path,
                              glue("processed_data/maps/cropland/{param$res}/esa_raw_{param$year}_{param$iso3c}.tif"))
lc_map <- raster(lc_file)
plot(lc_map)

# Update crop2globiom.csv and map coff to coff (instead of the rest category) and overwrite
load_data("crop2globiom", param)
crop2globiom <- crop2globiom %>%
  mutate(globiom_crop = ifelse(crop == "coff", "Coff", globiom_crop))
write_csv(crop2globiom, file.path(param$spam_path, "mappings/crop2globiom.csv"))

# Load mapping of lc classes to globiom lc classes
lc_class2globiom <- read_csv(file.path(param$spam_path, "mappings/esacci2globiom.csv"))


# Aggregate land cover map to GLOBIOM land cover classes at simu level
# Not that the area is expressed in 1000 ha, which is common in GLOBIOM!
create_globiom_input(lc_class2globiom, lc_map, param)

