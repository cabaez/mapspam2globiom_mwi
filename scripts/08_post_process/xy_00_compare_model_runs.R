#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to compare model runs
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


############### LOAD DATA ###############
# solve_sel = 0
adm_code_list_s0 <- unique(adm_list$adm0_code)
output_s0 <- map_df(adm_code_list_s0, comb_output, model_sel = "max_score") %>%
  mutate(solve_level = "adm0")

# solve_sel = 1
adm_code_list_s1 <- unique(adm_list$adm1_code)
output_s1 <- map_df(adm_code_list_s1, comb_output, model_sel = "max_score") %>%
  mutate(solve_level = "adm1")


############### PROCESS ###############
# Combine data
output <- bind_rows(output_s0, output_s1)

# Compare allocation 
output <- output %>%
  dplyr::select(gridID, adm0_code, adm0_name, adm1_code, adm1_name, 
                adm2_code, adm2_name, crop, system, pa, solve_level)

alloc_comp <- output %>%
  spread(solve_level, pa) %>%
  mutate(dif = adm0- adm1)

ggplot(alloc_comp, aes(x = adm0, y = adm1, colour = crop)) +
  geom_point()

adm_comp <- output %>%
  group_by(solve_level, crop, adm0_code, adm0_code) %>%
  summarize(pa = sum(pa, na.rm = T)) %>%
  spread(solve_level, pa) %>%
  mutate(dif = adm0- adm1)

adm_comp1 <- output %>%
  group_by(solve_level, crop, system, adm1_code, adm1_code) %>%
  summarize(pa = sum(pa, na.rm = T)) %>%
  spread(solve_level, pa) %>%
  mutate(dif = adm0- adm1)

adm_comp2 <- output %>%
  group_by(solve_level, crop, system, adm2_code, adm2_code) %>%
  summarize(pa = sum(pa, na.rm = T)) %>%
  spread(solve_level, pa) %>%
  mutate(dif = adm0- adm1)

### FUNCTIONS
# side by side
view2 <- function(crp, sys, n1 = "file1", n2 = "file2", df1 = db1, df2 = db2){
  ext <- extent(grid)
  p1 <- df1 %>%
    filter(crop == crp, system == sys)  %>%
    left_join(grid_df)
  r1 <- rasterFromXYZ(p1[c("x", "y", "alloc")], crs = crs(grid))
  names(r1) <- paste(crp, sys, n1, sep= "_")
  mv1 <- mapview(r1, use.layer.names = T) +
    mapview(adm, alpha.region = 0)
  
  p2 <- df2 %>%
    filter(crop == crp, system == sys)  %>%
    left_join(grid_df)
  r2 <- rasterFromXYZ(p2[c("x", "y", "alloc")], crs = crs(grid))
  names(r2) <- paste(crp, sys, n2, sep = "_")
  mv2 <- mapview(r2, use.layer.names = T) +
    mapview(adm, alpha.region = 0)
  sync(mv1, mv2)    
}


### ANALYSE
view2("rest", "S", "max_score", "max_score_no_weights")

