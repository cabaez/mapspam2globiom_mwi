#'========================================================================================================================================
#' Project:  mapspam2globiom_mwi
#' Subject:  Process adm shapefile
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### LOAD DATA ###############
# replace the name of the shapefile with that of your own country.
iso3c_shp <- "adm_2010_MWI.shp"

# load shapefile
adm_map_raw <- read_sf(file.path(param$spam_path, glue("raw_data/adm/{iso3c_shp}")))

# plot
plot(adm_map_raw$geometry)


############### PROCESS ###############
# Project to standard global projection
adm_map <- adm_map_raw %>%
  st_transform(param$crs)

# Check names
head(adm_map)
names(adm_map)

# Change names In order to use the country polygon as input, the column names of
# the attribute table have to be set. 
# The names of the administrative units should be set to admX_name, where X is the adm level.
# The codes of the administrative units should be set to admX_code, where X is the adm code.

# Set the original names, i.e. the ones that will be replaced. Remove adm1
# and/or adm2 entries if such data is not available.
adm0_name_orig <- "ADM0_NAME"
adm0_code_orig <- "FIPS0"
adm1_name_orig <- "ADM1_NAME"
adm1_code_orig <- "FIPS1"
adm2_name_orig <- "ADM2_NAME"
adm2_code_orig <- "FIPS2"

# Replace the names
names(adm_map)[names(adm_map) == adm0_name_orig] <- "adm0_name"
names(adm_map)[names(adm_map) == adm0_code_orig] <- "adm0_code"
names(adm_map)[names(adm_map) == adm1_name_orig] <- "adm1_name"
names(adm_map)[names(adm_map) == adm1_code_orig] <- "adm1_code"
names(adm_map)[names(adm_map) == adm2_name_orig] <- "adm2_name"
names(adm_map)[names(adm_map) == adm2_code_orig] <- "adm2_code"

# Only select relevant columns
adm_map <- adm_map %>%
  dplyr::select(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)

# Check names
head(adm_map)
names(adm_map)

# Union separate polygons that belong to the same adm    
adm_map <- adm_map %>%
  group_by(adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code) %>%
  summarize() %>%
  ungroup() %>%
  mutate(adm0_name = param$country,
         adm0_code = param$iso3c)

par(mfrow=c(1,2))
plot(adm_map$geometry, main = "ADM all polygons")

# Set names of ADMs that need to be removed from the polygon. 
# These are ADMs where no crop should be allocated. Here we remove 
# Area under National Administration, which is the part of Lake Malawi that belongs to Malawi
# and Likoma, several small islands in the lake that are not covered by the statistics.
# Set the adm_name by ADM level which need to be removed. Otherwise remove the script.
adm1_to_remove <- c("Area under National Administration")
adm2_to_remove <- c("Likoma")

# Remove ADMs
adm_map <- adm_map %>%
  filter(adm1_name != adm1_to_remove) %>%
  filter(adm2_name != adm2_to_remove)

plot(adm_map$geometry, main = "ADM polygons removed")
par(mfrow=c(1,1))

# Create pdf with the location of administrative units
create_adm_map_pdf(param)

# Create adm_list
create_adm_list(adm_map, param)  


############### SAVE ###############
# Save adm maps
temp_path <- file.path(param$spam_path, glue("processed_data/maps/adm/{param$res}"))
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

saveRDS(adm_map, file.path(temp_path, glue("adm_map_{param$year}_{param$iso3c}.rds")))
write_sf(adm_map, file.path(temp_path, glue("adm_map_{param$year}_{param$iso3c}.shp")))


############### CREATE GRID ###############
create_grid(param)


############### RASTERIZE ADM_MAP ###############
rasterize_adm_map(param)


############### CLEAN UP ###############

