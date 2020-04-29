#'========================================================================================================================================
#' Project:  crop map
#' Subject:  Script to create spam maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "mapview")
# Additional packages
p_load("countrycode", "gdxrrw", "plotKML", "viridis")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### LINK GAMS LIBRARIES
igdx(gams_path)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(max.print=1000000) # more is printed on screen
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


### LOAD DATA
# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

# grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Spam
spam <- readRDS(file.path(proc_path, paste0("spam_2.0/spam_final_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))


### FUNCTIONS
# Plus function that ensures NA + NA is NA not 0 as in sum. If na.rm = F (default), NA + 0 = NA, otherwise 0, similar to sum
plus <- function(x, na.rm = F){
  if(all(is.na(x))){
    c(x[0],NA)
  } else {
    if(na.rm == T){
      sum(x, na.rm = TRUE)
    } else {
      sum(x, na.rm)
    }  
  }
} 

# Function to compare crop maps per system in a panel
view_4p_f <- function(crp, vr, sy = c("S", "L", "H", "I"), df = db){
  ext <- extent(grid)
  df <- df %>%
    filter(crop == crp, system %in% sy) %>%
    left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  st <- lapply(seq(length(st)), function(i){
    mapview(st[[i]], layer.name = paste(crp, sys[i], sep = "_"))
  })
  sync(st) 
}

# Function to compare crop maps in stack
view_st_f <- function(crp, vr, sy = c("S", "L", "H", "I"), df = db){
  ext <- extent(grid)
  df <- df %>%
    filter(crop == crp, system %in% sy) %>%
    left_join(grid_df)
  sys <- unique(df$system)
  st <- lapply(sys, function(x) rasterFromXYZ(df[df$system == x, c("x", "y", vr)], crs = crs(grid)))
  st <- lapply(st, function(x) raster::extend(x, ext)) # Harmonize exent for stacking
  if(length(sys) >1){
    st <- stack(st)
  }else{
    st <- st[[1]]  
  }
  names(st) <- paste(crp, sys, vr, sep = "_")
  st[st==0] <- NA
  mapview(st, use.layer.names = T)
}


### PREPARE
# Add grid cell coordinates
grid_df <- as.data.frame(rasterToPoints(grid))

# Combine 
db <- spam
summary(db)


### VISUALIZE
# by crop
view_4p_f("maiz", "alloc")

# Across systems
by_system <- db %>%
  group_by(gridID, system) %>%
  summarize(alloc = plus(alloc, na.rm = T)) %>%
  ungroup() %>%
  mutate(crop = "total")

view_4p_f("total", "alloc", df = by_system)


### CREATE TIF FILES
# Function to save tif files
spam2tif_f <- function(crp, sy, df = db){
  df <- df %>%
    filter(crop == crp, system == sy) %>%
    left_join(grid_df,.) %>%
    dplyr::select(x, y, alloc)
  name <- paste(crp, sy, sep = "_")
  message(name)
  r <- rasterFromXYZ(df, crs = crs(grid))
  names(r) <- name
  plot(r, main = name)
  temp_path <- file.path(proc_path, "maps/spam_2.0")
  dir.create(temp_path, showWarnings = F, recursive = T)
  writeRaster(r, 
              file.path(temp_path, paste0("spam_", crp, "_", sy, "_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)
}


# by crop and system
crop_system_list <- db %>%
  dplyr::select(crop, system) %>%
  unique
walk2(crop_system_list$crop, crop_system_list$system, spam2tif_f)

# by system
system_list <- by_system %>%
  dplyr::select(crop, system) %>%
  unique
walk2(system_list$crop, system_list$system, spam2tif_f, df = by_system)

