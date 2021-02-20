## LODES commute shed spatial analysis
## Author: Nick Kobel
## Version: 0.1.4 alpha
## Last revision: 2020 Apr 28

## Path to version 0.2.0
## TODO Configure variable quosures for custom variable mapping; correspondingly adjust in web map
## TODO Add progress bar for neighbor calculation
## TODO Add queen and rook adjacency calculation
## TODO Add hexbin mapping function
## TODO Add function to generate static map
## TODO Add additional documentation to guide user to make correct choices when selecting neighbor weights

### Resources
## http://personal.tcu.edu/kylewalker/spatial-neighbors-in-r.html
## https://crd150.github.io/lab5.html
## https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.3.pdf
## https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1538-4632.1992.tb00261.x
## http://gis-pdx.opendata.arcgis.com/
## http://resources.esri.com/help/9.3/arcgisengine/java/gp_toolref/spatial_statistics_tools/how_hot_spot_analysis_colon_getis_ord_gi_star_spatial_statistics_works.htm

if(!require(pacman)){install.packages("pacman");library(pacman)}
p_load(tidyverse, spdep, spdplyr, leaflet, sp, sf, RColorBrewer)
# setwd("E:/Dropbox (BPS Tech Services)/BPS Tech Services Team Folder/data_analysis/LEHD/")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# httr::set_config(httr::config(ssl_verifypeer = FALSE))

library(arcgisbinding)
arcgisbinding::arc.check_product()

load_lodes_resources <- function(jt = "jt03") {
  ## Load all of the resources needed in the analyses
  
  jt <- jt
  
  ## 50-acre hexbin
  # hexgrid50.sf <<- readRDS("./data/hexgrid50.sf.RDS") %>% st_transform(4269)
  
  ## Block polygons
  orwablock.poly.sf <<- st_read("../data/shapes/tl_2015_orwa_csa_tabblock10_epsg2913.shp") %>% st_transform(4269)
  
  ## Block centroids
  block_centroids.sf <<- orwablock.poly.sf %>% st_centroid(.)
  
  ## Select only those hexbins that intersect a block centroid
  # hexbin.sf <<- hexgrid50.sf %>%
  #   filter(lengths(st_intersects(., block_centroids.sf)) > 0) ## TODO select only hexbins with any jobs by doing a spatial join and then removing the variables after determining selection
  
  ## Assign block centroids a hexbin ID
  block_centroids.sf <<- block_centroids.sf #%>%
    # st_join(., hexbin.sf %>% dplyr::select(hexid), by = st_intersects)
  
  ## Origin-destination file
  ## Load in the OD file
  od_main<-lehdr::grab_lodes(state = c("or", "wa"), year = 2018, lodes_type = "od", job_type = "JT00",
                             segment = "S000", state_part = "main")%>%
    mutate(COUNTYFP = substr(w_geocode, start = 3, stop = 5))

  od_aux<-lehdr::grab_lodes(state = c("or", "wa"), year = 2018, lodes_type = "od", job_type = "JT00",
                            segment = "S000", state_part = "aux")%>%
    mutate(COUNTYFP = substr(w_geocode, start = 3, stop = 5))

  od <- rbind(od_main, od_aux) %>%
    mutate(w_geocode_tct = substr(w_geocode, 1, 11),
           h_geocode_tct = substr(h_geocode, 1, 11))

  saveRDS(od, "../data/processed/od_main_aux_orwa_2018_jt00_s000.RDS")
  
  
  od.df <<- readRDS(paste0("../data/od_main_aux_orwa_2018_", jt, "_s000.RDS")) ## TODO Do we need to filter out blocks outside the CSA?
  
  ## Origin-destination sf for home location
  od.sf.h <<- od.df %>%
    filter(S000 >= 1) %>%
    inner_join(block_centroids.sf, ., by = c("GEOID10" = "h_geocode")) %>%
    dplyr::select(GEOID10, hexid, w_geocode, S000:SI03) %>% st_transform(4269)
  
  ## Origin-destination sf for work location
  od.sf.w <<- od.df %>%
    filter(S000 >= 1) %>%
    inner_join(block_centroids.sf, ., by = c("GEOID10" = "w_geocode")) %>%
    dplyr::select(GEOID10, hexid, h_geocode, S000:SI03) %>% st_transform(4269)
  
  ## RAC file
  rac.df <<- readRDS(paste0("./data/rac_orwa_2017_", jt, "_s000.RDS"))
  
  ## WAC file
  wac.df <<- readRDS(paste0("./data/wac_orwa_2017_", jt, "_s000.RDS"))
}

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

workers_to_employment_area <- function(user_area.sf, summary_var = quo(S000), block_centroids.sf, orwablock.poly.sf, od.df, od.sf.w, rac.df) {
  ##### Function to summarize where the workers of a selected geography live
  
  user_area.sf <- user_area.sf %>% st_transform(4269)
  
  ## Get a list of block FIPS codes that intersect the user-defined shape
  work_selection <- as.character((block_centroids.sf %>%
                                    filter(lengths(st_intersects(., user_area.sf)) > 0) %>%
                                    st_set_geometry(., NULL) %>% 
                                    dplyr::select(GEOID10))[,])
  
  ## Get a list of block FIPS codes that match the home location of employment blocks in the user-defined shape
  home_selection <- as.character((od.sf.w %>%
                                    filter(GEOID10 %in% work_selection) %>%
                                    dplyr::select(h_geocode) %>%
                                    st_set_geometry(NULL) %>%
                                    as.data.frame(.))[,])
  
  ## Use RAC file to identify blocks with SOME employment and are NOT in the home selection; set the assumed zero values to actual zeros
  ## TODO use quo() to select the universe of employment, not just total employment; if user-input variable is
  ## SE01 then CE01 should be the matching variable determining the universe of "employment"; SE02 then CE02, etc.
  zero_workers_in_selection.df <- rac.df %>%
    filter(C000 > 0,
           !(h_geocode %in% home_selection)) %>%
    dplyr::select(h_geocode) %>%
    mutate(S000 = 0, SA01 = 0, SA02 = 0, SA03 = 0, SE01 = 0, SE02 = 0, SE03 = 0, SI01 = 0, SI02 = 0, SI03 = 0)
  
  ## Create a summary data.frame for the in-commute shed of the user-input geography
  home_summary.df <- od.df %>%
    filter(h_geocode %in% home_selection,
           w_geocode %in% work_selection) %>%
    dplyr::select(h_geocode, w_geocode, S000:SI03) %>%
    group_by(h_geocode) %>%
    summarize_at(vars(S000:SI03), sum, na.rm = TRUE) %>%
    rbind(., zero_workers_in_selection.df)
  
  ## Turn the df into a shape. Since the block polygon is a shapefile with 11 counties, the data.frame 
  ## will contain more rows from counties outside the commute area.
  home_summary.sf <- home_summary.df %>%
    inner_join(orwablock.poly.sf, ., by = c("GEOID10" = "h_geocode")) %>%
    dplyr::select(GEOID10, S000:SI03) 
  
  ##### Insert section here to return statistics or variables; could potentially 
  ### return list which would be input into generating Gi* maps; print or web or hex
  
  summary_list <- list()
  summary_list$summary <- home_summary.sf
  summary_list$shape <- user_area.sf
  
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


build_gistar <- function(summary_list, summary_var = quo(S000), method = "d", distance = 1.2, k.neighbors = 40) {
  ## Distance depends on projection of input data; when EPSG = 4269, 1.2 is in kilometers which is about 0.75 miles
  ## k.neighbors is set to 40 to capture the approximate number of blocks in a block group: 39 blocks per block group
  
  ## Get local varnames for summary and user-defined geography; ensure correct projection
  ## TODO write if-then statement to convert to 4269 if it's not already
  block_summary.sf <- summary_list$summary #%>% st_transform(4269)
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


build_webmap_census_blocks <- function(weighted_summary) {
  
  block_summary.sf <- weighted_summary
  
  cols <- rev(brewer.pal(7, 'RdYlBu'))
  
  pal <- colorBin(palette = cols, domain = block_summary.sf$g, 
                  bins = c(min(block_summary.sf$g), -2.58, -1.96, -1.65, 
                           1.65, 1.96, 2.58, max(block_summary.sf$g)))
  
  ## TODO change employment via quo()
  popup <- paste0("<strong>Employment (", as.character(summary_var[[2]]),"): </strong>", 
                  summary_var_vector, 
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





# ##### Commute map example (where do the workers of a location live?) ####
load_lodes_resources()

my_work_area <- geojsonsf::geojson_sf(readLines("https://opendata.arcgis.com/datasets/65c53b0f616a4a13b51e40c237c70050_11.geojson")) %>%
  # filter(NAME == "Central Eastside Industrial")
  filter(NAME == "Columbia Corridor")

home_area_summary <- workers_to_employment_area(user_area.sf = my_work_area, block_centroids.sf = block_centroids.sf,
                                                orwablock.poly.sf = orwablock.poly.sf, od.df = od.df, od.sf.w = od.sf.w, rac.df = rac.df)

gistar_summary <- build_gistar(home_area_summary)

arc.write("E:/HCP/Tech_Services/Nick/TSUS/LUM_Leslie/east/commute_maps/data.gdb", gistar_summary %>% st_transform(2913))

commute_from_webmap <- build_webmap_census_blocks(gistar_summary)

commute_from_webmap


# ##### Commute map example (where do the residents of a location work?) ####
load_lodes_resources()

my_home_area <- geojsonsf::geojson_sf(readLines("https://opendata.arcgis.com/datasets/9f50a605cf4945259b983fa35c993fe9_125.geojson")) %>%
  filter(MAPLABEL == "Laurelhurst")

work_area_summary <- workers_from_home_area(user_area.sf = my_home_area, block_centroids.sf = block_centroids.sf,
                                       orwablock.poly.sf = orwablock.poly.sf, od.df = od.df, od.sf.h = od.sf.h, wac.df = wac.df)

commute_to_webmap <- build_webmap_census_blocks(work_area_summary)

commute_to_webmap


# ##### Paired commute analysis (what are the characteristics of residents of area A who work in area B?) ####
# load_lodes_resources()
# 
coalitions <- geojsonsf::geojson_sf(readLines("https://opendata.arcgis.com/datasets/556e60657edd48de9a947183700fc35e_124.geojson"))
epno <- coalitions %>% filter(MAPLABEL == "East Portland Community Office")
# 
neighborhoods <- geojsonsf::geojson_sf(readLines('https://opendata.arcgis.com/datasets/9f50a605cf4945259b983fa35c993fe9_125.geojson'))
# boise <- neighborhoods %>% filter(MAPLABEL == "Boise")
downtown <- neighborhoods %>% filter(MAPLABEL == "Portland Downtown")

# workers_between_areas(boise, downtown, block_centroids.sf = block_centroids.sf, od.df = od.df)
# 

plan_districts <- geojsonsf::geojson_sf(readLines("https://opendata.arcgis.com/datasets/8da33e0494e44255ac99c1726f7ade04_122.geojson"))
central_city <- plan_districts %>% filter(PLDIST %in% c("CC", "CCSA")) %>% summarize()

workers_between_areas(epno, central_city, block_centroids.sf = block_centroids.sf, od.df = od.df) %>%
  write.table("clipboard", sep = "\t")

workers_between_areas(epno, epno, block_centroids.sf = block_centroids.sf, od.df = od.df) %>%
  write.table("clipboard", sep = "\t")