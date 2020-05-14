#'========================================================================================================================================
#' Project:  mapspam2globiom_mwi
#' Subject:  Run model
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### INSPECT RESULTS ###############
view_panel("rice", var = "ha", param)
view_stack("rice", var = "ha", param)


############### CREATE TIF ###############
create_all_tif(param)

