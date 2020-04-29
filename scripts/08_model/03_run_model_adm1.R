#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to run model
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster", "mapview")

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


############### RUN MODEL INPUT AT ADM LEVEL ###############

# Function to run mapspam in gams
run_gams <- function(adm_code_sel, model_sel, verbose = T){
  model <- file.path(root, glue("code/08_model/{model_sel}.gms"))
  input <- file.path(proc_path, glue("harmonized/{adm_code_sel}/input_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.gdx"))
  output <- file.path(proc_path, glue("harmonized/{adm_code_sel}/output_{model_sel}_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.gdx"))
  lst <- file.path(proc_path, glue("harmonized/{adm_code_sel}/lst_{model_sel}_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.lst"))
  logf <- file.path(proc_path, glue("harmonized/{adm_code_sel}/log_{model_sel}_{grid_sel}_{year_sel}_{adm_code_sel}_{iso3c_sel}.log"))
  gams_system_call <- glue("gams.exe {model} --gdx_input={input} --gdx_output={output} lf={logf} o={lst} logOption 4")
  gams_system_call <- gsub("/", "\\\\", gams_system_call) # convert forward- into backslash 
  message(glue("Running {model_sel} for {adm_code_sel}"))
  cmd_output = system(gams_system_call, intern = TRUE)
  if (verbose) {
    message((paste(cmd_output, collapse = "\n")))
  }
}

if(model_sel == "max_score") {
  walk(adm_code_list, run_gams, "max_score")  
  } else {
  if(model_sel == "min_entropy") {
    message("This model is not implemented yet")
    stop("Stop process")
    } else {
    message("This model is not implemented yet")
    stop("Stop process")
  }
}
