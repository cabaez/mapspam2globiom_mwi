#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Code to select GAEZ spam 1.0 input maps per country
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


########## LOAD DATA ##########
load_data(c("adm_map", "grid", "gaez2crop"), param)

# As some gaez maps are not available (see Convert_GAEZ_too_Suit_v4.docx, we need a specific mapping).
gaez2crop <- gaez2crop %>%
  mutate(id = paste(crop, system, sep = "_"))


########## CREATE 5 ARCMIN MAPS FROM RAW GAEZ FOR CROPSUIT ##########
# Create file lookup table
lookup <- bind_rows(
  data.frame(files_full = list.files(file.path(param$raw_path, "gaez/cropsuitabilityindexvalue"), pattern = ".tif$", full.names = T),
                             files = list.files(file.path(param$raw_path, "gaez/cropsuitabilityindexvalue"), pattern = ".tif$")) %>%
    separate(files, into = c("suit_variable", "gaez_crop", "gaez_system", "input"), sep = "_", remove = F) %>%
    separate(input, into = c("gaez_input", "ext"), sep = "\\.") %>%
    dplyr::select(-ext),
  data.frame(files_full = list.files(file.path(param$raw_path, "gaez/cropsuitabilityindexforcurrentcultivatedland"), pattern = ".tif$", full.names = T),
                                    files = list.files(file.path(param$raw_path, "gaez/cropsuitabilityindexforcurrentcultivatedland"), pattern = ".tif$")) %>%
    separate(files, into = c("suit_variable", "gaez_crop", "gaez_system", "input"), sep = "_", remove = F) %>%
    separate(input, into = c("gaez_input", "ext"), sep = "\\.") %>%
    dplyr::select(-ext)) %>%
  left_join(gaez2crop,., by = c("gaez_crop", "gaez_input", "gaez_system", "suit_variable"))  


### WARP AND MASK
# Set files
grid <- file.path(param$spam_path,
                  glue("processed_data/maps/grid/{param$res}/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/{param$res}/adm_map_{param$year}_{param$iso3c}.shp"))


# Function to loop over gaez files, warp and mask
clip_gaez <- function(id, var, folder){
  cat("\n", id)
  temp_path <- file.path(param$spam_path, glue("processed_data/maps/{folder}/{param$res}"))
  dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
  input <- lookup$files_full[lookup$id == id]
  output <- file.path(temp_path,
                      glue("{id}_{var}_{param$res}_{param$year}_{param$iso3c}.tif"))
  output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                              cutline = mask, crop_to_cutline = F, 
                              r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
  plot(output_map, main = id)
}

# warp and mask
walk(lookup$id, clip_gaez, "bs", "biophysical_suitability")


############### CLEAN UP  ###############
rm(lookup)


########## CREATE 5 ARCMIN MAPS FROM RAW GAEZ FOR PRODUCTIONCAPACITY ##########
# Create file lookup table
lookup <- bind_rows(
  data.frame(files_full = list.files(file.path(param$raw_path, "gaez/totalproductioncapacity"), pattern = ".tif$", full.names = T),
                             files = list.files(file.path(param$raw_path, "gaez/totalproductioncapacity"), pattern = ".tif$")) %>%
    separate(files, into = c("prod_variable", "gaez_crop", "gaez_system", "input"), sep = "_", remove = F) %>%
    separate(input, into = c("gaez_input", "ext"), sep = "\\.") %>%
    dplyr::select(-ext),
  data.frame(files_full = list.files(file.path(param$raw_path, "gaez/potentialproductioncapacityforcurrentcultivatedland"), pattern = ".tif$", full.names = T),
                                    files = list.files(file.path(param$raw_path, "gaez/potentialproductioncapacityforcurrentcultivatedland"), pattern = ".tif$")) %>%
    separate(files, into = c("prod_variable", "gaez_crop", "gaez_system", "input"), sep = "_", remove = F) %>%
    separate(input, into = c("gaez_input", "ext"), sep = "\\.") %>%
    dplyr::select(-ext)) %>%
  left_join(gaez2crop,., by = c("gaez_crop", "gaez_input", "gaez_system", "prod_variable"))  

# warp and mask
walk(lookup$id, clip_gaez, "py", "potential_yield")


############### CLEAN UP  ###############
rm(adm_loc, gaez2crop, grid, mask)
rm(clip_gaez, gaez2crop, lookup)

