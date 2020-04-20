#'========================================================================================================================================
#' Project:  Global-to-local GLOBIOM
#' Subject:  Support functions for crop map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

# TO_UPDATE: assume srcnodata = -9999, is this acceptable. combined #2 functions with base functions
# SOMEHOW DOES NOT PERFORM CORRECTLY.
# Check this for manipulating parameters of nested functions
#https://stackoverflow.com/questions/35587265/pass-arguments-in-nested-function-to-update-default-arguments
#https://gis.stackexchange.com/questions/41718/seting-0-values-in-geotiff-to-nodata-using-gdal

# srcfile <- file.path(proc_path, "maps/gaez/raw/suit/maiz_s_suit_5min_MWI.tif")
# reference <- file.path(proc_path, "maps/grid/grid_30sec_r_2010_MWI.tif")
# dstfile <- file.path(proc_path, "maps/gaez/30sec/suit/maiz_s_suit_30sec_MWI.tif")
# verbose = T
# output_Raster = TRUE
# nThreads = "ALL_CPUS"
# r = "bilinear"
# overwrite = TRUE
# cutline <- file.path(proc_path, "maps/adm/adm_2010_MWI.shp")
# crop_to_cutline = T

# Function to align rasters witht mask at end and setting certain values to NA
# Based on align_rasters from gdalUtils
align_raster <- function (srcfile, reference, dstfile, output_Raster = TRUE, 
                             nThreads = "ALL_CPUS", verbose = F, r = "bilinear", 
                             overwrite = TRUE,...) 
{
  message(glue("Processing {basename(srcfile)}"))
  reference_info <- gdalUtils::gdalinfo(reference, proj4 = TRUE, raw_output = FALSE, 
                             verbose = verbose)
  proj4_string <- reference_info$proj4
  bbox <- reference_info$bbox
  te <- c(reference_info$bbox[1, 1], reference_info$bbox[2,1],
          reference_info$bbox[1, 2], reference_info$bbox[2,2])
  ts <- c(reference_info$columns, reference_info$rows)
  if (missing(dstfile)) 
    dstfile <- tempfile()
  if (is.character(nThreads)) {
    if (nThreads == "ALL_CPUS") {
      multi = TRUE
      wo = "NUM_THREADS=ALL_CPUS"
    }
  }
  else {
    if (nThreads == 1) {
      multi = FALSE
      wo = NULL
    }
    else {
      multi = TRUE
      wo = paste("NUM_THREADS=", nThreads, sep = "")
    }
  }
  r = r
  synced <- gdalUtils::gdalwarp(srcfile = srcfile, dstfile = dstfile, 
                     te = te, t_srs = proj4_string, ts = ts, output_Raster = output_Raster, 
                     multi = multi, wo = wo, verbose = verbose, r = r, overwrite = overwrite)
  
  if(output_Raster) plot(synced, main = paste0(basename(srcfile)))
  if(output_Raster) return(synced)
  message("Done")
}


# Function to warp and crop at the same time
align_cut_raster <- function (srcfile, reference, dstfile, cutline, 
                              output_Raster = TRUE, crop_to_cutline = T, 
                              nThreads = "ALL_CPUS", verbose = F, r = "bilinear", 
                              overwrite = TRUE,...) 
{
  message(glue("Processing {basename(srcfile)}"))
  reference_info <- gdalUtils::gdalinfo(reference, proj4 = TRUE, raw_output = FALSE, 
                                        verbose = verbose)
  proj4_string <- reference_info$proj4
  bbox <- reference_info$bbox
  te <- c(reference_info$bbox[1, 1], reference_info$bbox[2,1],
          reference_info$bbox[1, 2], reference_info$bbox[2,2])
  ts <- c(reference_info$columns, reference_info$rows)
  if (missing(dstfile)) 
    dstfile <- tempfile()
  if (is.character(nThreads)) {
    if (nThreads == "ALL_CPUS") {
      multi = TRUE
      wo = "NUM_THREADS=ALL_CPUS"
    }
  }
  else {
    if (nThreads == 1) {
      multi = FALSE
      wo = NULL
    }
    else {
      multi = TRUE
      wo = paste("NUM_THREADS=", nThreads, sep = "")
    }
  }
  r = r
  synced <- gdalUtils::gdalwarp(srcfile = srcfile, dstfile = dstfile, 
                                te = te, t_srs = proj4_string, ts = ts, output_Raster = output_Raster, 
                                multi = multi, wo = wo, verbose = verbose, r = r, overwrite = overwrite,
                                cutline = cutline, crop_to_cutline = crop_to_cutline)
  
  if(output_Raster) plot(synced, main = paste0(basename(srcfile)))
  message("Done")
  if(output_Raster) return(synced)
}



# Function to warp and crop at the same time, and remove input data values
align_cut_rm_raster <- function (srcfile, reference, dstfile, cutline, 
                              output_Raster = TRUE, crop_to_cutline = T, 
                              nThreads = "ALL_CPUS", verbose = F, r = "bilinear", 
                              overwrite = TRUE, srcnodata = srcnodata,...) 
{
  message(glue("Processing {basename(srcfile)}"))
  reference_info <- gdalUtils::gdalinfo(reference, proj4 = TRUE, raw_output = FALSE, 
                                        verbose = verbose)
  proj4_string <- reference_info$proj4
  bbox <- reference_info$bbox
  te <- c(reference_info$bbox[1, 1], reference_info$bbox[2,1],
          reference_info$bbox[1, 2], reference_info$bbox[2,2])
  ts <- c(reference_info$columns, reference_info$rows)
  if (missing(dstfile)) 
    dstfile <- tempfile()
  if (is.character(nThreads)) {
    if (nThreads == "ALL_CPUS") {
      multi = TRUE
      wo = "NUM_THREADS=ALL_CPUS"
    }
  }
  else {
    if (nThreads == 1) {
      multi = FALSE
      wo = NULL
    }
    else {
      multi = TRUE
      wo = paste("NUM_THREADS=", nThreads, sep = "")
    }
  }
  r = r
  synced <- gdalUtils::gdalwarp(srcfile = srcfile, dstfile = dstfile, 
                                te = te, t_srs = proj4_string, ts = ts, output_Raster = output_Raster, 
                                multi = multi, wo = wo, verbose = verbose, r = r, overwrite = overwrite,
                                cutline = cutline, crop_to_cutline = crop_to_cutline,
                                srcnodata = srcnodata)
  
  if(output_Raster) plot(synced, main = paste0(basename(srcfile)))
  message("Done")
  if(output_Raster) return(synced)
}

# Plus function that ensures NA + NA is NA not 0 as in sum.
# If na.rm = F (default), NA + 0 = NA, otherwise 0, similar to sum
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


# function to extract replacement crops in case of missing information
replace_gaez <- function(crp_sys, db, db_raw){
  
  crp <- strsplit(crp_sys, split = "_")[[1]][1]
  sys <- strsplit(crp_sys, split = "_")[[1]][2]
  crp_subs <- gaez_subs$crop_subs1[gaez_subs$crop == crp]
  
  if(!is.na(crp_subs)) {
    crp_sys_subs <- paste(crp_subs, sys, sep = "_")
    crp_subs_df <- as.data.frame(rasterToPoints(stack(grid, subset(db_raw, crp_sys_subs)))) %>%
      filter(gridID %in% unique(cl_raw$gridID)) %>%
      dplyr::select(-x, -y) %>%
      setNames(c("gridID", "value")) %>%
      mutate(crop_system = crp_sys) %>%
      filter(!is.na(gridID)) %>%
      mutate(value = ifelse(is.na(value), 0, value),
             value = ifelse(value < 0, 0, value))
    
    if(!all(crp_subs_df$value == 0)) {
      message(glue("Use {crp_sys_subs} to substitute {crp_sys}."))
      return(crp_subs_df)
    }
  }
  crp_subs <- gaez_subs$crop_subs2[gaez_subs$crop == crp]
  if(is.na(crp_subs)) {
    stop(glue("GAEZ information for {crp_sys} with no substitute crop!. Add crop in gaez_subs!"))
  } else {
    crp_sys_subs <- paste(crp_subs, sys, sep = "_")
    crp_subs_df <- as.data.frame(rasterToPoints(stack(grid, subset(db_raw, crp_sys_subs)))) %>%
      filter(gridID %in% unique(cl_raw$gridID)) %>%
      dplyr::select(-x, -y) %>%
      setNames(c("gridID", "value")) %>%
      mutate(crop_system = crp_sys) %>%
      filter(!is.na(gridID)) %>%
      mutate(value = ifelse(is.na(value), 0, value),
             value = ifelse(value < 0, 0, value))
    
    if(all(crp_subs_df$value != 0)) {
      message(glue("Use {crp_sys_subs} to substitute {crp_sys}."))
      return(crp_subs_df)
    }
    stop(glue("GAEZ information for {crp_sys} with no substitute crop!. Add crop in gaez_subs!"))
  }
}

### FUNCTIONS
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
  leafsync::sync(st) 
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

### NOTES
#' This script contains functions to convert dataframes into gdx files.
#' To do this the function wdgx of the package gdxrrw needs to be supplied with a list for: (1) the data values and (2) each variable
#' This is tedious and the following should be taken into account
#' 1. Data must be supplied as matrix
#' 2. The list must contain information on the set (see function below), most importantly the list of unique labels.
#' 
#' I use the following procedure to create the file
#' 1. Create a dataframe that first lists the variables (as factors) and in the final column the values.
#' 2. Obtain the factor levels from the factor variables using extractLevels_f
#' 3. Convert the data.frame to a matrix using factor2Number_f.
#' 4. Create the parameter file using GDXPara_f. NOTE, use 'unname' in front of the level argument as it is not allowed to have a name.
#' 5. Create the sets files (one for each set) using GDXSet_f. NOTE, use 'unname' in front of the level argument as it is not allowed to have a name.
#' 6. Create the name of the GDX file including the full path
#' 7. Use wdgx to create the file with as arguments: First the parameter file and second the sets files (in the same order as the parameter file).
#' 


# CHECK: writing dim might not even necessary. See writeTansport.R in gdxrrw package folder for examples.

# Function to create val for parameter prep file
val_gdx <- function(val, variables){
  
  # Create factors of variables
  val[,variables] <- lapply(val[,variables, drop = F] , factor) # Drop added otherwise val becomes a vector
  
  # Convert factor variables to numeric
  for(i in which(sapply(val, class) == "factor")) val[[i]] = as.numeric(val[[i]])  
  val <- as.matrix(val)
  val <- unname(val)
  return(val)  
}


# Function to create uels for parameter prep file
uels_gdx <- function(uels, variables){
  uels <- uels[names(uels) %in% variables]
  uels <- lapply(uels, factor)
  uels <- lapply(uels,levels)
  return(uels)
}

# Function prepare parameter gdx file
para_gdx <- function(df, variables, name, ts = NULL, type = "parameter",  form = "sparse"){
  
  # Prepare input
  val <- val_gdx(df, variables)
  uels <- uels_gdx(df, variables)
  dim <- length(uels)
  ts <- ifelse(is.null(ts), name, ts)
  
  # Create parameter list
  para <- list()
  para[["val"]] <- val    # Array containing the symbol data
  para[["name"]] <- name  # Symbol name (data item)
  para[["dim"]] <- dim    # Dimension of symbol = levels
  para[["ts"]] <- ts      # Explanatory text for the symbol
  para[["uels"]] <- uels  # Unique Element Labels (UELS) (levels)
  para[["type"]] <- type  # Type of the symbol
  para[["form"]] <- form  # Representation, sparse or full
  return(para)
}


# Function prepare sets gdx file
set_gdx <- function(df, variables, name = NULL, ts = NULL, type = "set"){
  
  # Prepare input
  uels <- uels_gdx(df, variables)
  
  if(length(variables) > 1) {
    val <- val_gdx(df, variables)
    form <- "sparse"
  } else {
    val <- array(rep(1, length(uels[[1]])))
    form <- "full"
  }
  
  dim <- length(uels)
  name <- ifelse(is.null(name), variables, name)
  ts <- ifelse(is.null(ts), variables, ts)
  
  # Create set list
  set <- list()
  set[["val"]] <- val
  set[["name"]] <- name
  set[["ts"]] <- ts
  set[["type"]] <- type
  set[["dim"]] <- dim
  set[["form"]] <- form
  set[["uels"]] <- uels
  return(set)
}

# Function to prepare scalar gdx file
scalar_gdx <- function(val, name = NULL, ts = NULL, type = "parameter", form = "full"){
  
  # Create scalar list
  scalar <- list()
  scalar[["val"]] <- val
  scalar[["name"]] <- name
  scalar[["ts"]] <- ts
  scalar[["type"]] <- type
  scalar[["form"]] <- form
  return(scalar)
}

# Build_mapspam
build_mapspam <- function(path = NULL){
  if(is.null(path)) path <- path.expand("~")
  
  raw_folders <- c("adm", "aquastat", "faostat", "gaez", "gia", "gmia", 
  "grump", "sasam", "subnational_statistics", "travel_time_2000", "travel_time_2015",
  "worldpop")
  proc_folders <- c("intermediate", "maps", "output")
  
  invisible(lapply(raw_folders, function(x) dir.create(file.path(path, paste0("raw_data/",x)), showWarnings = F, recursive = T)))
  invisible(lapply(proc_folders, function(x) dir.create(file.path(path, paste0("processed_data/",x)), showWarnings = F, recursive = T)))
  dir.create(file.path(path, "parameters"), showWarnings = F, recursive = T)
  #scripts_path <- system.file("model_scripts", package = "mapspam2globiom")
  #scripts <- list.files(scripts_path, full.names = T)
  #catch <- file.copy(scripts, path, recursive = TRUE)
}
