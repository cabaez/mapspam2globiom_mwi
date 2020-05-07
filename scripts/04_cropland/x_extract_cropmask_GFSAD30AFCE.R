#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to extract GFSADAFCE crop cover
#' Author:   Yating Ru
#' Contact:  y.ru@cgiar.org
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### SOURCE FUNCTIONS
source(file.path(root, "Code/general/support_functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


### LOAD DATA
#grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

#select GFSAD30 tiles based on adm extent
adm_ext <- extent(adm)
xmin <- (xmin(adm) %/% 10)*10
xmax <- (xmax(adm) %/% 10)*10
ymin <- (ymin(adm) %/% 10)*10
ymax <- (ymax(adm) %/% 10)*10
xlist <- seq(xmin, xmax, 10)
ylist <- seq(ymin, ymax, 10)

m <- expand.grid(ylist, xlist)

m$Var1 <- ifelse(m$Var1>0, paste0("N", as.character(m$Var1)), 
                 ifelse(m$Var1<0, paste0("S", as.character(abs(m$Var1))),
                 paste0("N0", as.character(m$Var1))))
m$Var2 <- ifelse(m$Var2>0, paste0("E", as.character(m$Var2)), 
                 ifelse(m$Var2<0, paste0("W", as.character(abs(m$Var2))),
                        paste0("E0", as.character(m$Var2))))
m$file <- paste0("GFSAD30AFCE_2015_",m$Var1,m$Var2,"_001_2017261090100.tif" )
m$file_full <- file.path(glob_path,"GFSAD30", m$file)
gfsad <- do.call(merge, lapply(m$file_full, raster))

#crop lc using grid
lc_raw <- crop(gfsad, grid)
dir.create(file.path(proc_path, "maps/gfsad"), showWarnings = F)
writeRaster(lc_raw, file.path(proc_path, paste0("maps/gfsad/gfsad_raw_", year_sel, "_", iso3c_sel, ".tif")), overwrite=TRUE)
#lc_raw <- raster(file.path(proc_path, paste0("maps/gfsad/gfsad_raw_", year_sel, "_", iso3c_sel, ".tif")))

#legend
lc_class <- read_csv(file.path(glob_path, "GFSAD30/GFSAD30-Legend.csv")) %>%
  dplyr::select(lc_code, lc)

#create a table to reclassify the classes into crops(1) and non-crops(0)
reclass_table <- lc_class %>% 
  mutate(crop=if_else(lc == "crops", 1, 0)) %>%
  dplyr::select(lc_code, crop) 

#reclassify the land cover classes into crops(1) and non-crops(0) 
lc_reclass <- subs(lc_raw, reclass_table, subsWithNA=FALSE)
writeRaster(lc_reclass, file.path(proc_path, paste0("maps/gfsad/gfsad_reclass_", year_sel, "_", iso3c_sel, ".tif")), overwrite=TRUE)
#lc_reclass <- raster(file.path(proc_path, paste0("maps/gfsad/gfsad_reclass_", year_sel, "_", iso3c_sel, ".tif")))

# #aggregate 30m pixels to 1 km pixels
# lc_aggre <- aggregate(lc_reclass, fact=31, fun=mean, expand=TRUE, na.rm=TRUE)
# 
# #resample to align with 1km grid
# lc_resamp <- resample(lc_aggre, grid, method="ngb")

### AGGREGATE LC TO GRID RESOLUTION
# Specify input and output files
lc_reclass_file <-file.path(proc_path, paste0("maps/gfsad/gfsad_reclass_", year_sel, "_", iso3c_sel, ".tif"))
lc_grid_file <- file.path(proc_path,paste0("maps/gfsad/gfsad_grid_", year_sel, "_", iso3c_sel, ".tif"))
grid_file <- file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif"))

lc_resamp <- align_raster2_f(lc_reclass_file, grid_file, lc_grid_file, nThreads = "ALL_CPUS", verbose = T, 
                               output_Raster = T, overwrite = TRUE, r = "average")

#calculate crop area within each 1km grid(ha)
grid_size <- area(grid)
lc_area <- lc_resamp * grid_size *100

#extract to a data frame
lc_stack <- stack(grid, lc_area)
lc_df <- as.data.frame(rasterToPoints(lc_stack)) %>%
  set_names(c("x", "y", "gridID", "area")) %>%
  filter(!is.na(gridID) & area>0)%>%
  mutate(source = "gfsad") %>%
  dplyr::select(gridID, area, source)


### SAVE
temp_path <- file.path(proc_path, "synergistic_cropmask")
dir.create(temp_path, recursive = T, showWarnings = F)

saveRDS(lc_df, file.path(temp_path, paste0("cropmask_gfsad_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))


### CLEAN UP
rm(grid, grid_size, grid_lc_res, lc_raw, lc_class, reclass_table, lc_reclass, lc_aggre, lc_resamp, lc_area, lc_df, lc_stack, 
   temp_path, adm, adm_extent, m, gfsad, xmin, xmax, ymin, ymax, xlist, ylist)
