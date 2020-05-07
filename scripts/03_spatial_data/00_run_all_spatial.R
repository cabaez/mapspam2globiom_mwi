#'========================================================================================================================================
#' Project:  mapspam2globiom
#' Subject:  Code to run all core scripts that select spatial layers
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# NOTE: if you have prefer to use alternative spatial layers you can add them
# and replace some of the the global layers.


############### CROPLAND ###############
source(here::here("scripts/03_spatial_data/select_esa.r"))
source(here::here("scripts/03_spatial_data/select_sasam.r"))


############### IRRIGATED AREA ###############
source(here::here("scripts/03_spatial_data/select_gia.r"))
source(here::here("scripts/03_spatial_data/select_gmia.r"))


############### BIOPHYSICAL SUITABILITY AND POTENTIAL YIELD ###############
source(here::here("scripts/03_spatial_data/select_gaez.r"))


############### ACCESSIBILITY ###############
source(here::here("scripts/03_spatial_data/select_travel_time_2000_2015.r"))


############### POPULATION ###############
source(here::here("scripts/03_spatial_data/select_worldpop.r"))
source(here::here("scripts/03_spatial_data/select_urban_extent.r"))


############### SIMU ###############
source(here::here("scripts/03_spatial_data/select_simu.r"))

