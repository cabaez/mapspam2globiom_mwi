#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to combine and save gams results
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster", "mapview", "gdxrrw")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))


############### LINK GAMS LIBRARIES ###############
# R will automatically find the location of GAMS if this is stored in your system (see documentation).
# If for some reason this does not work you can set it specifically, e.g. igdx("C:/Program Files/GAMS/win64/24.6")
igdx("")


############### SET ADM IN LINE WITH SOLVE_SEL ###############
# Adm list
adm_list <- read_csv(file.path(proc_path, glue("lists/adm_map_list_{year_sel}_{iso3c_sel}.csv")))

if(solve_sel == 0) {
  adm_code_list <- unique(adm_list$adm0_code)
} else {
  adm_code_list <- unique(adm_list$adm1_code)
}


############### LOAD DATA ###############
# function to prepare output file, combining ADM results
comb_output <- function(adm_code_sel, model_sel) {
  output_file <- file.path(proc_path, glue("harmonized/{adm_code_sel}/output_{model_sel}_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}"))
  message(glue("Load {basename(output_file)}"))
  
  grid_df <- as.data.frame(rasterToPoints(raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif"))))) %>%
    setNames(c("x", "y", "gridID"))
  adm_r <- readRDS(file.path(proc_path, paste0("maps/adm/adm_r_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) 
  df <- rgdx.param(output_file, "palloc", names = c("gridID", "crop_system", "pa"),  compress = T) %>%
    mutate(gridID = as.numeric(as.character(gridID)),
           crop_system = as.character(crop_system)) %>%
    separate(crop_system, into = c("crop", "system"), sep = "_", remove = T) %>%
    left_join(adm_r, by = "gridID") %>%
    left_join(grid_df, by = "gridID")
  # %>%
  #   mutate(year = year_sel,
  #          resolution = grid_sel,
  #          model = model_sel,
  #          solve_level = solve_sel,
  #          subnat_level = adm_sel)
  return(df)
}

############### SAVE ###############
temp_path <- file.path(proc_path, "output")
dir.create(temp_path, showWarnings = F, recursive = T)

output <- map_df(adm_code_list, comb_output, model_sel)
saveRDS(output, file.path(proc_path, glue("output/output_{model_sel}_{grid_sel}_year_sel_{iso3c_sel}.rds")))
