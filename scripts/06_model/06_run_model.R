#'========================================================================================================================================
#' Project:  mapspam2globiom_mwi
#' Subject:  
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### HARMONIZE INPUT DATA ###############
harmonize_inputs(param)


############### PREPARE BIOPHYSICAL SUITABILITY AND POTENTIAL YIELD ###############
prepare_bs_yg("biophysical_suitability", param)
prepare_bs_yg("potential_yield", param)


############### PREPARE SCORE ###############
prepare_score(param)


############### COMBINE MODEL INPUTS ###############
combine_inputs(param)


############### RUN MODEL ###############
run_spam(param)


############### PREPARE RESULTS ###############
prepare_results(param)


############### PREPARE RESULTS ###############
create_all_tif(param)
