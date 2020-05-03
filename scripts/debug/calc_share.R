# Function to calculate share of simu that is covered by specific land cover class
calc_class_Share <- function(mapping, lc_map) {
  mp <- mapping %>%
    dplyr::group_by(globiom_lc_code) %>%
    dplyr::summarize(lc_code = list(unique(lc_code)))
  
  calc_share <- function(i, r, mp) {
    
    globiom_lc_code <- mp$globiom_lc_code[i]
    cat("\nProcessing", globiom_lc_code)
    load_data("simu", param, mess = F, local = T)
    
    simu <- simu %>%
      dplyr::group_by(SimUID) %>%
      dplyr::summarize() %>%
      dplyr::mutate(geometry = sf::st_cast(geometry, "MULTIPOLYGON")) %>% # Cast to MP for exactextract
      dplyr::ungroup() 
    
    lc_code <- mp$lc_code[i][[1]]
    cat("\n", lc_code)
    
    # Total lc resolution cells of lc_code class covered
    simu[,c("n_lc")] <- exactextractr::exact_extract(r, simu, function(values, coverage_fraction)
      sum(coverage_fraction[values %in% lc_code]))
    
    # Total number of cells at lc resolution in polygon
    simu[,c("n_all")] <- exactextractr::exact_extract(r, simu, function(values, coverage_fraction)
      sum(coverage_fraction))
    
    # Note that total coverage does not add up to one as border SimUID polygons are not 
    # always fully covered by underlying lc map.
    df <- simu %>%
      sf::st_drop_geometry(.) %>%
      dplyr::mutate(globiom_lc_code = globiom_lc_code,
             share = n_lc/n_all) %>%
      dplyr::select(SimUID, share)
      
      
    return(df) 
  }
  
  df <- purrr::map_df(1:nrow(mp), calc_share, r = r, mp = mp) 
  return(df)
}
