#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Create pdf with adm information
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue","ggspatial", "sf")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notatio
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# adm
adm <- readRDS(file.path(proc_path, glue("maps/adm/adm_{year_sel}_{iso3c_sel}.rds")))


############### CREATE ADM1 MAP ###############
if(adm_sel %in% c(1,2)){
  
  # Create adm1 polygons
  adm1 <- adm %>%
    group_by(adm1_name, adm1_code) %>%
    summarize()
  
  # Labels at the centre of adm
  adm1_name <- st_centroid(adm1)
  adm1_name <- cbind(adm1, st_coordinates(st_centroid(adm1$geometry)))
  
  # Increase number of colours in palette
  cols <- brewer_pal(palette = "Set1")(9)
  cols <- gradient_n_pal(cols)(seq(0, 1, length.out = length(adm1$adm1_name)))
  
  # Plot
  adm1_plot <- ggplot() +
    geom_sf(data = adm1, colour = "grey30", aes(fill = adm1_name)) +
    scale_fill_manual(values = cols) +
    geom_text(data= adm1_name, aes(x = X, y = Y, label = adm1_name),
              check_overlap = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = "transparent"), plot.title = element_text(hjust = 0.5)) +
    #theme_void(base_size = 14) +
    theme(panel.grid.major = element_line(colour = 'transparent')) +
    labs(fill = "", x = "", y = "", title = country_sel) +
    guides(fill = FALSE)
  
  rm(adm1, adm1_name)
}


############### CREATE ADM2 MAP ###############
if(adm_sel %in% c(2)){
  
  # Create adm2 polygons
  adm2 <- adm 
  
  # Labels at the centre of adm
  adm2_name <- st_centroid(adm2)
  adm2_name <- cbind(adm2, st_coordinates(st_centroid(adm2$geometry)))
  
  # Increase number of colours in palette
  cols <- brewer_pal(palette = "Set1")(9)
  cols <- gradient_n_pal(cols)(seq(0, 1, length.out = length(adm2$adm2_name)))
  
  # Plot
  adm2_plot <- ggplot() +
    geom_sf(data = adm2, colour = "grey30", aes(fill = adm2_name)) +
    scale_fill_manual(values = cols) +
    geom_text(data= adm2_name, aes(x = X, y = Y, label = adm2_name),
              check_overlap = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
                           pad_x = unit(0, "in"), pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = "transparent"), plot.title = element_text(hjust = 0.5)) +
    #theme_void(base_size = 14) +
    theme(panel.grid.major = element_line(colour = 'transparent')) +
    labs(fill = "", x = "", y = "", title = country_sel) +
    guides(fill = FALSE)
  
  rm(adm2, adm2_name)
}


############### SAVE ###############
temp_path <- file.path(proc_path, "reports/")
dir.create(temp_path, recursive = T, showWarnings = F)

if(adm_sel %in% c(1,2)){
  pdf(file = file.path(temp_path, glue("adm_maps_{year_sel}_{iso3c_sel}.pdf")), width = 8.27, height = 11.69)
  if(adm_sel == 1) {
    print(adm1_plot)
    rm(adm1_plot)
  } else if(adm_sel %in% c(1,2)) {
    print(adm1_plot)
    print(adm2_plot)
    rm(adm1_plot, adm2_plot)
  }
  dev.off()
}

########## CLEAN UP ##########
rm(temp_path, adm, cols)
