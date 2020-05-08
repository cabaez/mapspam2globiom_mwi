#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to create synergistic ir area map file that can be used in harmonization part
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### PREPARE BIOPHYSICAL SUITABILITY AND POTENTIAL YIELD ###############
prepare_bs_yg("biophysical_suitability", param)
prepare_bs_yg("potential_yield", param)


############### PREPARE SCORE ###############
prepare_score(param)


