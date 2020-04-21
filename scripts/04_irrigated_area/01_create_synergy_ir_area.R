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
load_input(c("grid", "gmia", "gia"), param)


############### PROCESS ###############
# Create grid area 
grid_size <- area(grid)
grid_size <- grid_size * 100 # in ha
names(grid_size) <- "grid_size"

# Create df of gia
ir_df <-   as.data.frame(rasterToPoints(stack(grid, grid_size, gmia, gia))) %>%
  dplyr::select(-x, -y) %>%
  filter(!is.na(gridID))

# Rank gia and gmia_share. Make sure that rank 1 corresponds with the highest gmia_share
ir_df <- ir_df %>%
  mutate(gmia_rank = cut(gmia_share, labels = c(1:10), breaks = seq(0, 1, 0.1)),
         gmia_rank = dense_rank(desc(gmia_rank)))

# Rank gia and gmia. 
# Only gia is preferred, irrespecitive of gmia (rank of 1), 
# followed by gmai with share between 0.9 and 1
# followed by gmia with share between 0.8 and 1
# ...

ir_df <- ir_df %>%
  mutate(ir_rank = if_else(gia_share == 1, 1, gmia_rank+1)) %>%
  filter(!is.na(ir_rank))

# Calculate irrigated area per grid cell and add max irrigated area
ir_df <- ir_df %>%
  mutate(gmia = gmia_share * grid_size,
         gia = gia_share * grid_size,
         ir_max = pmax(gmia, gia, na.rm = T)) %>%
  dplyr::select(-gia_share, -gmia_share, -gmia_rank)


############### SAVE ###############
saveRDS(ir_df, file.path(proc_path, glue("harmonized/synergy_ir_area_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))


############### CLEAN UP ###############
rm(gia_share, gmia_share, grid, grid_size, ir_df)
