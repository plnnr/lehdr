pacman::p_load(tidyverse, lehdr, purrr, tigris, sf, mapview)

options(
  scipen = 999,
  digits = 4,
  tigris_class = "sf",
  tigris_use_cache = T,
  future.globals.maxSize = 4000 * 1024^5
)

## Modify variables in constants to change which states, counties, years and job type files are downloaded
source("R/00_constants.R")

## Create empty sf object to iterate through next
blocks <- st_sf(st_sfc()) %>% st_set_crs(paste0("EPSG:",TARGET_EPSG))

## Iterate through user-input states to grab all Census blocks
for(state in STATES){
  thistate <- tigris::blocks(state = state, year = CURRENT_YEAR) %>% 
    st_transform(paste0("EPSG:",TARGET_EPSG))
  blocks <- rbind(blocks, thistate)
}

## Filter for user-provided county selection
selection_blocks <- blocks %>% 
  filter(substr(GEOID10, 1, 5) %in% COUNTIES) %>%
  st_make_valid()

saveRDS(selection_blocks, BLOCKS_LOC)

## Simplify the block shapes (experimental)
selection_blocks_simple <- rmapshaper::ms_simplify(selection_blocks)
saveRDS(selection_blocks_simple, BLOCKS_SIMPLE_LOC)

selection_block_centroids <- st_centroid(selection_blocks)

bboxhex <- st_make_grid(selection_block_centroids, 
                    cellsize = 1 / 3.28084 * DESIRED_HEX_DIAMETER_IN_FEET,  # 3.28084 feet in a meter, and our target EPSG units should be meters
                    crs = paste0("EPSG:",TARGET_EPSG),
                    what = "polygons",
                    square = FALSE) %>%
  as.data.frame() %>% st_as_sf() %>%
  mutate(hexid = seq.int(nrow(.))) 

## Select only those hexbins that intersect a block centroid
# hex <- bboxhex %>%
#   st_filter(., selection_block_centroids) ## TODO select only hexbins with any jobs by doing a spatial join and then removing the variables after determining selection

saveRDS(bboxhex, HEXGRID_LOC)

## Assign block centroids a hexbin ID
selection_block_centroids <- selection_block_centroids %>%
  st_join(., bboxhex %>% dplyr::select(hexid), by = st_intersects)

saveRDS(selection_block_centroids, BLOCK_CENTROIDS_LOC)


