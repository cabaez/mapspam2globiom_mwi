#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to prepare synergy irrigated map
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

############### LOAD DATA
# Load data
load_data(c("adm_map", "grid", "gmia", "gia"), param)


############### PREPARE ###############
# Create grid area 
grid_size <- area(grid)
grid_size <- grid_size * 100 # in ha
names(grid_size) <- "grid_size"

# Grid df
grid_df <- as.data.frame(rasterToPoints(grid))
  
# Create df of gia. Remove values < 0.01, most of which are probably caused by reprojecting the maps
ir_df <-   as.data.frame(rasterToPoints(stack(grid, grid_size, gmia, gia))) %>%
  filter(!is.na(gridID)) %>%
  dplyr::select(-x, -y) %>%
  mutate(gia = ifelse(gia < 0.01, 0, gia),
         gmia = ifelse(gmia < 0.01, 0, gmia))
  
# Create ranking by first taking the maximum of the irrigated area share,
# calculate irrigated area, and then rank. In this way we prefer the largest
# area, and hence prefer GIA over GMIA when the resolution is 30 arcsec (GIA is
# 1 or 0). At a resolution of 5 arcmin the GMIA and grid cells with a lot of GIA
# observations get a high rank, which is also desirable.
  
ir_df <- ir_df %>%
  dplyr::mutate(ir_max = pmax(gmia, gia, na.rm = T),
                ir_rank = cut(ir_max, labels = c(1:10), breaks = seq(0, 1, 0.1),
                              include.lowest = T),
                ir_rank = dense_rank(desc(ir_rank)),
                ir_max = ir_max * grid_size) %>%
  filter(!is.na(ir_rank), ir_max > 0) %>%
  dplyr::select(-gmia, -gia, -grid_size)


############### CREATE IR MAX AND IR RANK MAPS ###############
# ir_max
ir_max_map <- ir_df %>%
  left_join(grid_df,.) %>%
  dplyr::select(x, y, ir_max)
ir_max_map <- rasterFromXYZ(ir_max_map)
crs(ir_max_map) <- crs(param$crs)
plot(ir_max_map)
plot(adm_map$geometry, add = T)


# ir_rank
ir_rank_map <- ir_df %>%
  left_join(grid_df,.) %>%
  dplyr::select(x, y, ir_rank)
ir_rank_map <- rasterFromXYZ(ir_rank_map)
crs(ir_rank_map) <- crs(param$crs)
plot(ir_rank_map)
plot(adm_map$geometry, add = T)


############### SAVE ###############
writeRaster(ir_max_map, file.path(param$spam_path, 
  glue("processed_data/maps/irrigated_area/ia_max_{param$res}_{param$year}_{param$iso3c}.tif")),overwrite = T)

writeRaster(ir_rank_map, file.path(param$spam_path, 
  glue("processed_data/maps/irrigated_area/ia_rank_{param$res}_{param$year}_{param$iso3c}.tif")),overwrite = T)

############### CLEAN UP ###############
rm(adm_map, gia, gmia, grid, grid_df, grid_size, id_df, ir_max_map, ir_rank_map)
