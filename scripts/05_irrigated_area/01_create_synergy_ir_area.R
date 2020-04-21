#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to prepare synergy irrigated area
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


########## LOAD DATA ##########
load_data(c("adm_list", "adm_map_r", "grid", "gmia", "gia"), param)


############### PROCESS ###############
# Create grid area 
grid_size <- area(grid)
grid_size <- grid_size * 100 # in ha
names(grid_size) <- "grid_size"

# Create df of gia
ir_df <-   as.data.frame(rasterToPoints(stack(grid, grid_size, gmia, gia))) %>%
  dplyr::select(-x, -y) %>%
  filter(!is.na(gridID))

# Create ranking by first taking the maximum of the irrigated area share,
# calculate irrigated area, and then rank. In this way we prefer the largest
# area, and hence prefer GIA over GMIA when the resolution is 30 arcsec (GIA is
# 1 or 0). At a resolution of 5 arcmin the GMIA and grid cells with a lot of GIA
# observations get a high rank, which is also desirable.

ir_df <- ir_df %>%
  mutate(ir_max = pmax(gmia, gia, na.rm = T),
         ir_rank = cut(ir_max, labels = c(1:10), breaks = seq(0, 1, 0.1), include.lowest = T),
         ir_rank = dense_rank(desc(ir_rank)),
         ir_max = ir_max * grid_size) %>%
  filter(!is.na(ir_rank)) %>%
  dplyr::select(-gmia, -gia, -grid_size)


############### CREATE ADM LEVEL FILES ###############
# Set adm_level
if(param$solve_level == 0) {
  adm_code_list <- unique(adm_list$adm0_code)
} else {
  adm_code_list <- unique(adm_list$adm1_code)
}


# Save 
prepare_spatial <- function(adm_code, df, var, adm_map_r, param){
  adm_sel <- paste0("adm", param$solve_level, "_code")
  df <- dplyr::left_join(adm_map_r, ir_df)
  df <- df[df[[adm_sel]] == adm_code_list,] %>%
    dplyr::select(gridID, ir_rank, ir_max)
  
  temp_path <- file.path(param$spam_path,
                         glue::glue("processed_data/intermediate_output/{adm_code}"))
  dir.create(temp_path, recursive = T, showWarnings = F)
  saveRDS(df, file.path(temp_path,
                              glue::glue("{var}_{param$year}_{adm_code}_{param$iso3c}.rds")))
}

walk(adm_code_list, prepare_spatial, ir_df, "ir", adm_map_r, param)


############### SAVE ###############
saveRDS(ir_df, file.path(proc_path, glue("harmonized/synergy_ir_area_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))


############### CLEAN UP ###############
rm(gia, gmia, grid, grid_size, ir_df)
