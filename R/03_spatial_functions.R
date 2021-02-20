pacman::p_load(tidyverse, spdep, spdplyr, leaflet, sp, sf, RColorBrewer)

# library(arcgisbinding)
# arcgisbinding::arc.check_product()

workers_between_areas <- function(home_area.sf, work_area.sf, block_centroids.sf, od.df) {
  ##### Function to summarize the characteristics of workers commuting from home area to work area
  
  home_area.sf <- home_area.sf %>% st_transform(4269)
  work_area.sf <- work_area.sf %>% st_transform(4269)
  
  ## Get a list of block FIPS codes that intersect the user-defined shapes
  home_area_selection <- as.character((block_centroids.sf %>%
                                         filter(lengths(st_intersects(., home_area.sf)) > 0) %>%
                                         st_set_geometry(., NULL) %>% 
                                         dplyr::select(GEOID10))[,])
  
  work_area_selection <- as.character((block_centroids.sf %>%
                                         filter(lengths(st_intersects(., work_area.sf)) > 0) %>%
                                         st_set_geometry(., NULL) %>% 
                                         dplyr::select(GEOID10))[,])
  
  ## Summarize workers in home area
  home_area_summary.df <- od.df %>%
    filter(h_geocode %in% home_area_selection) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    mutate(summary_level = "home_area_summary")
  
  ## Summarize workers commuting from home area to work area
  commuters_to_work_area.df <- od.df %>%
    filter(h_geocode %in% home_area_selection,
           w_geocode %in% work_area_selection) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    mutate(summary_level = "commute_summary")
  
  ## Summarize workers in work area
  work_area_summary.df <- od.df %>%
    filter(w_geocode %in% work_area_selection) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    mutate(summary_level = "work_area_summary")
  
  ## Combine summaries, transpose, and calculate the shares of employment that commuters between the areas comprise
  summary.df <- rbind(home_area_summary.df, work_area_summary.df, commuters_to_work_area.df) %>%
    dplyr::select(summary_level, S000:SI03) %>%
    mutate(SE12 = SE01 + SE02) %>% 
    gather(., S000:SE12, key = "segment", value = "employment") %>% 
    spread(., summary_level, employment) %>%
    mutate(share_home_to_work = commute_summary / home_area_summary, ## Share of workers in home area that commute to work area
           share_work_from_home = commute_summary / work_area_summary) ## Share of workers in work area that come from home area
  
  return(summary.df)
}

workers_within_area <- function(home_area.sf, block_centroids.sf, od.df){
  ##### Function to summarize the characteristics of workers who live and work in an area
  
  home_area.sf <- home_area.sf %>% st_transform(4269)
  
  ## Get a list of block FIPS codes that intersect the user-defined shapes
  bg_selection <- as.character((block_centroids.sf %>%
                                         filter(lengths(st_intersects(., home_area.sf)) > 0) %>%
                                         st_set_geometry(., NULL) %>% 
                                         dplyr::select(GEOID10))[,])
  
  ## Summarize workers who live and work in user-defined area
  within_area_summary.df <- od.df %>%
    filter(h_geocode %in% bg_selection,
           w_geocode %in% bg_selection) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    mutate(summary_level = "Working and living in selected area")
  
  ## Summarize workers who live in user-defined area but work outside
  home_area_summary.df <- od.df %>%
    filter(h_geocode %in% bg_selection,
           !(w_geocode %in% bg_selection)) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    mutate(summary_level = "Live in selected area, work outside")
  
  ## Summarize workers who work in user-defined area but live outside
  work_area_summary.df <- od.df %>%
    filter(w_geocode %in% bg_selection,
           !(h_geocode %in% bg_selection)) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    mutate(summary_level = "Work in selected area, live outside")
  
  
  ## Combine summaries, transpose, and calculate the shares of employment that commuters between the areas comprise
  summary.df <- rbind(within_area_summary.df, home_area_summary.df, work_area_summary.df) %>%
    select(summary_level, S000:SI03) %>%
    mutate(SE12 = SE01 + SE02) %>% 
    gather(., S000:SE12, key = "segment", value = "employment") %>% 
    spread(., summary_level, employment)
  
  return(summary.df)
  
}

workers_to_employment_area <- function(user_area.sf, summary_var = "S000", block_centroids.sf, blocks_poly_simple.sf, od.df, od.sf.w, rac.df) {
  ##### Function to summarize where the workers of a selected geography live
  
  ## Substitute "C" for "S" as the summary variable and convert it to symbol to be used in summary function
  summary_var <- sym(gsub("S", "C", summary_var))
  
  if(st_crs(user_area.sf) != st_crs('EPSG:4269')) {
    user_area.sf <- st_transform(user_area.sf, 4269)
  }
  
  ## Get a list of block FIPS codes that intersect the user-defined shape
  work_selection <- block_centroids.sf %>%
    st_filter(., user_area.sf) %>%
    st_set_geometry(., NULL) %>% 
    pull(GEOID10) %>% unique()
  
  ## Get a list of block FIPS codes that match the home location of employment blocks in the user-defined shape
  home_selection <- od.sf.w %>% 
    st_set_geometry(NULL) %>%
    filter(GEOID10 %in% work_selection) %>%
    pull(h_geocode) %>% unique()
  
  ## Get a list of hex IDs that match the home location of employment blocks in the user-defined shape
  home_selection_hexid <- block2hex %>% 
    filter(GEOID10 %in% home_selection) %>% 
    pull(hexid) %>% unique()
  
  ## Old way to do it that might have errors
  # home_selection_hexid <- od.sf.w %>% 
  #   st_set_geometry(NULL) %>%
  #   filter(GEOID10 %in% work_selection) %>%
  #   pull(hexid) %>% unique()
  
  ## Use RAC file to identify blocks with SOME employment and are NOT in the home selection; set the assumed zero values to actual zeros
  zero_workers_in_selection.df <- rac.df %>%
    filter(!!summary_var > 0,
           !(h_geocode %in% home_selection)) %>%
    dplyr::select(h_geocode) %>%
    mutate(S000 = 0, SA01 = 0, SA02 = 0, SA03 = 0, SE01 = 0, SE02 = 0, SE03 = 0, SI01 = 0, SI02 = 0, SI03 = 0)
  
  ## TODO Decide if implicit zero-value hexbins (with no employment and no residents) should be made explicitly zero 
  zero_workers_in_selection_hex.df <- rac.df %>%
    group_by(hexid) %>%
    summarize_at(.vars = vars(!!summary_var), .funs = sum) %>%
    filter(!!summary_var > 0,
           !(hexid %in% home_selection_hexid)) %>%
    dplyr::select(hexid_h = hexid) %>%
    mutate(S000 = 0, SA01 = 0, SA02 = 0, SA03 = 0, SE01 = 0, SE02 = 0, SE03 = 0, SI01 = 0, SI02 = 0, SI03 = 0)
  
  
  ## Create a summary data.frame for the in-commute shed of the user-input geography
  home_summary.df <- od.df %>%
    filter(h_geocode %in% home_selection,
           w_geocode %in% work_selection) %>%
    dplyr::select(h_geocode, w_geocode, S000:SI03) %>%
    group_by(h_geocode) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    rbind(., zero_workers_in_selection.df)
  
  home_hex_summary.df <- od.df %>%
    filter(h_geocode %in% home_selection,
           w_geocode %in% work_selection) %>%
    dplyr::select(hexid_h, S000:SI03) %>%
    group_by(hexid_h) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    rbind(., zero_workers_in_selection_hex.df)
  
  ## Turn the df into a shape. Since the block polygon is a shapefile with 11 counties, the data.frame 
  ## will contain more rows from counties outside the commute area.
  home_summary.sf <- home_summary.df %>%
    inner_join(blocks_poly_simple.sf, ., by = c("GEOID10" = "h_geocode")) %>%
    dplyr::select(GEOID10, S000:SI03) 
  
  ## TODO Change to full_join if implicit zero-value hexbins are made explicitly zero
  home_hex_summary.sf <- home_hex_summary.df %>%
    inner_join(hexgrid.sf, ., by = c("hexid" = "hexid_h")) %>%
    arrange(desc(S000)) %>% 
    distinct(., hexid, .keep_all = T)
  
  ##### Insert section here to return statistics or variables; could potentially 
  ### return list which would be input into generating Gi* maps; print or web or hex
  
  summary_list <- list()
  summary_list$summary <- home_summary.sf
  summary_list$shape <- user_area.sf
  summary_list$hex_summary <- home_hex_summary.sf
  
  return(summary_list) ## GEOID is the home block FIPS of employees commuting to user-defined area
}

workers_from_home_area <- function(user_area.sf, block_centroids.sf, orwablock.poly.sf, od.df, od.sf.h, wac.df) {
  ##### Function to summarize where the working residents of a selected geography work
  
  user_area.sf <- user_area.sf %>% st_transform(4269)
  
  ## Get a list of block FIPS codes that intersect the user-defined shape
  home_selection <- as.character((block_centroids.sf %>%
                                    filter(lengths(st_intersects(., user_area.sf)) > 0) %>%
                                    st_set_geometry(., NULL) %>% 
                                    dplyr::select(GEOID10))[,])
  
  ## Get a list of block FIPS codes that match the work location of resident blocks in the user-defined shape
  work_selection <- as.character((od.sf.h %>%
                                    filter(GEOID10 %in% home_selection) %>%
                                    dplyr::select(w_geocode) %>%
                                    st_set_geometry(NULL) %>%
                                    as.data.frame(.))[,])
  
  ## Use WAC file to identify blocks with SOME employment and are NOT in the work selection; set the assumed zero values to actual zeros
  ## TODO use quo() to select the universe of employment, not just total employment; if user-input variable is
  ## SE01 then CE01 should be the matching variable determining the universe of "employment"; SE02 then CE02, etc.
  zero_workers_in_selection.df <- wac.df %>%
    filter(C000 > 0,
           !(w_geocode %in% work_selection)) %>%
    dplyr::select(w_geocode) %>%
    mutate(S000 = 0, SA01 = 0, SA02 = 0, SA03 = 0, SE01 = 0, SE02 = 0, SE03 = 0, SI01 = 0, SI02 = 0, SI03 = 0)
  
  ## Create a summary data.frame for the out-commute shed of the user-input geography
  work_summary.df <- od.df %>%
    filter(w_geocode %in% work_selection,
           h_geocode %in% home_selection) %>%
    dplyr::select(w_geocode, h_geocode, S000:SI03) %>%
    group_by(w_geocode) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    rbind(., zero_workers_in_selection.df)
  
  ## Turn the df into a shape. Since the block polygon is a shapefile with 11 counties, the data.frame 
  ## will contain more rows from counties outside the commute area.
  work_summary.sf <- work_summary.df %>%
    inner_join(orwablock.poly.sf, ., by = c("GEOID10" = "w_geocode")) %>%
    dplyr::select(GEOID10, S000:SI03) 
  
  summary_list <- list()
  summary_list$summary <- work_summary.sf
  summary_list$shape <- user_area.sf
  
  return(summary_list) ## GEOID is the work destination block FIPS of residents commuting out of user-defined area
}

build_gistar <- function(summary_list, summary_var = "S000", method = "d", distance = 1.2, k.neighbors = 40, use_analysis_grid = TRUE) {
  ## Distance depends on projection of input data; when EPSG = 4269, 1.2 is in kilometers which is about 0.75 miles
  ## k.neighbors is set to 40 to capture the approximate number of blocks in a block group: 39 blocks per block group
  
  summary_var <- sym(summary_var)
  
  ## Get local varnames for summary and user-defined geography; ensure correct projection
  ## TODO write if-then statement to convert to 4269 if it's not already
  if(use_analysis_grid == TRUE){
    block_summary.sf <- summary_list$hex_summary
  }
  else {
    block_summary.sf <- summary_list$summary
  }

  user_area.sf <- summary_list$shape #%>% st_transform(4269)
  
  ## Create spatial object for neighbor calculations
  block_summary.sp <- as(block_summary.sf, "Spatial")
  
  if(method == "d") {
    ## Distance units depends on projection; epsg 4269 is in km (?)
    ## TODO WHAT IS d??
    ## TODO Can we add a progress bar here??
    # d <- dnearneigh(st_coordinates(block_summary.sf)[,1:2], d1 = 0, d2 = distance, longlat = TRUE) 
    d <- dnearneigh(coordinates(block_summary.sp), d1 = 0, d2 = distance, longlat = TRUE) 
    spatial_weights <- nb2listw(include.self(d)) ## the include.self() function makes it Gi* instead of just Gi
  } 
  
  if(method == "k") {
    ## TODO WHAT IS k??
    # k <- knearneigh(st_coordinates(block_summary.sf)[,1:2], k = k.neighbors, longlat = TRUE)
    k <- knearneigh(coordinates(block_summary.sp), k = k.neighbors, longlat = TRUE)
    spatial_weights <- nb2listw(include.self(knn2nb(k))) ## the include.self() function makes it Gi* instead of just Gi
  } 
  
  if(method == "q") {
    ## Not very relevent unless it's aggregated to block groups
    spatial_weights <- nb2listw(include.self(poly2nb(block_summary.sp)))
  } 
  
  if(method == "r") {
    ## Not very relevent unless it's aggregated to block groups
    spatial_weights <- nb2listw(include.self(poly2nb(block_summary.sf, queen = FALSE)))
  }
  
  summary_var_vector <- (block_summary.sf%>%dplyr::select(!!summary_var)%>%st_set_geometry(NULL)%>%as.data.frame(.))[,]
  
  ## TODO use quo() to select the universe of employment
  g <- localG(summary_var_vector, spatial_weights)
  
  block_summary.sf$g <- g

  return(block_summary.sf)
}

build_webmap_census_blocks <- function(weighted_summary, summary_var = "S000") {
  
  block_summary.sf <- weighted_summary
  
  cols <- rev(brewer.pal(7, 'RdYlBu'))
  
  pal <- colorBin(palette = cols, domain = block_summary.sf$g, 
                  bins = c(min(block_summary.sf$g), -2.58, -1.96, -1.65, 
                           1.65, 1.96, 2.58, max(block_summary.sf$g)))
  
  ## TODO change employment via quo()
  popup <- paste0("<strong>Employment (", summary_var,"): </strong>", 
                  "<br/>", 
                  "<strong>Gi* z-score:</strong> ", 
                  as.character(round(block_summary.sf$g, 2)))
  
  gimap <- leaflet(block_summary.sf) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolygons(data = block_summary.sf, fillColor = ~pal(g), stroke = NA, weight = 0.7, fillOpacity = 0.8, 
                popup = popup, smoothFactor = 0.1) %>%
    addPolylines(data = user_area.sf %>% st_transform(4269), 
                 weight = 2, color = "#800080", opacity = 0.7) %>%
    addLegend(pal = pal, values = block_summary.sf$g, title = "Gi* z-score") %>%
    setView(-122.676483, 45.523064, zoom = 11)
  
  return(gimap)
}

load_lodes_resources <- function() {
  ## Load all of the resources needed in the analyses
  
  blocks_poly.sf <<- readRDS(BLOCKS_LOC) %>% st_transform(4269) ; message("blocks_poly.sf successfully read")
  blocks_poly_simple.sf <<- readRDS(BLOCKS_SIMPLE_LOC) %>% st_transform(4269) ; message("blocks_poly_simple.sf successfully read")
  block_centroids.sf <<- readRDS(BLOCK_CENTROIDS_LOC) %>% st_transform(4269) ; message("block_centroids.sf successfully read")
  hexgrid.sf <<- readRDS(HEXGRID_LOC) %>% st_transform(4269) ; message("hexgrid.sf successfully read")
  
  # working_hexgrid.sf <- hexgrid.sf %>%
  #   st_filter(., block_centroids.sf) ## TODO select only hexbins with any jobs by doing a spatial join and then removing the variables after determining selection
  
  block2hex <- block_centroids.sf %>% 
    st_drop_geometry() %>%
    mutate(hexid_h = hexid,
           hexid_w = hexid) %>%
    select(GEOID10, hexid, hexid_h, hexid_w) 
  
  wac.df <<- readRDS(WAC_LOC) %>%
    left_join(., select(block2hex, GEOID10, hexid), by = c("w_geocode" = "GEOID10")) ; message("WAC successfully read")
  rac.df <<- readRDS(RAC_LOC) %>%
    left_join(., select(block2hex, GEOID10, hexid), by = c("h_geocode" = "GEOID10")) ; message("RAC successfully read")
  od.df <<- readRDS(OD_LOC) ; message("OD successfully read")
 
  ## Origin-destination sf for home location
  od.sf.h <<- od.df %>%
    filter(S000 >= 1) %>%
    inner_join(block_centroids.sf, ., by = c("GEOID10" = "h_geocode")) %>%
    dplyr::select(GEOID10, hexid, w_geocode, S000:SI03) %>% st_transform(4269) ; message("od.sf.h successfully joined")
  
  ## Origin-destination sf for work location
  od.sf.w <<- od.df %>%
    filter(S000 >= 1) %>%
    inner_join(block_centroids.sf, ., by = c("GEOID10" = "w_geocode")) %>%
    dplyr::select(GEOID10, hexid, h_geocode, S000:SI03) %>% st_transform(4269) ; message("od.sf.w successfully joined")
  
  od.df <<- od.df %>%
    left_join(., select(block2hex, GEOID10, hexid_h), by = c("h_geocode" = "GEOID10")) %>%
    left_join(., select(block2hex, GEOID10, hexid_w), by = c("w_geocode" = "GEOID10")) ; message("od.df successfully joined")
}

