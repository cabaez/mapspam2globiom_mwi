#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to create synergistic cropland map file that can be used in harmonization part
#' Author:   Michiel van Dijk, Yating
#' Contact:  michiel.vandijk@wur.nl, y.ru@cgiar.org
#'========================================================================================================================================

############### SOURCE PARAMETERS ###############
source(here::here("scripts/01_model_setup/01_model_setup.r"))


############### CREATE SYNERGY CROPLAND INPUT ###############
prepare_cropland(param)
