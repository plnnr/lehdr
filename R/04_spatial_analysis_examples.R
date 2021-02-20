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

## Load constant variables (region definition, directory locations)
source("R/00_constants.R")
source("R/03_spatial_functions.R")

load_lodes_resources()


# ##### Commute map example (where do the workers of a location live?) ####
my_work_area <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/65c53b0f616a4a13b51e40c237c70050_11.geojson") %>%
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