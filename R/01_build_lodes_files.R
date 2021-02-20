pacman::p_load(tidyverse, lehdr, purrr)

# plan(multisession)

## Modify variables in constants to change which states, counties, years and job type files are downloaded
source("00_constants.R")

###### WAC and RAC files first #######

## Make list of all possible inputs to the `grab_lods` function
inputs <- list(
  year = c(LONGITUDINAL_YEARS), # Not all states have consistent yearly availability
  lodes_type = c("WAC", "RAC"),
  job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"),
  segment = c("S000", "SE01", "SE02", "SE03", "SA01", "SA02", "SA03", "SI01", "SI02", "SI03")
) 

## Cross the inputs, filter for desired job type, and turn it into a named list
inputs <- inputs %>%
  # Cross the list to generate inputs into the download function
  purrr::cross_df() %>%
  # Filter for the data you want to grab
  filter(job_type == PRIMARY_JT) %>%
  as.list() %>%
  setNames(names(inputs))

## Helper function for `purrr::pmap_dfr` call below, which downloads lodes data for all year-state-lodes_type-job_type-segment combinations
download_lodes_longitudinal <- function(state, year, lodes_type, job_type, segment, download_dir = "../data/lodes_raw") {
  map_df(.x = year, 
         .f = ~grab_lodes(state = state, 
                          year = .x, 
                          lodes_type = lodes_type, 
                          job_type = job_type, 
                          segment = segment,
                          download_dir = download_dir)) %>%
    mutate(segment = segment,
           job_type = job_type,
           lodes_type = lodes_type,
           year = year)
}

## This will download all year-state-lodes_type-job_type-segment combinations. 
statelodes <- pmap_dfr(.l = inputs,
                       .f = ~download_lodes_longitudinal(..., state = STATES))

## Filter for the counties specified
countieslodes <- statelodes %>%
  filter(substr(w_geocode, 1, 5) %in% COUNTIES | 
           substr(h_geocode, 1, 5) %in% COUNTIES) %>%
  select(w_geocode, h_geocode, everything())

## Save the processed WAC and RAC file 
saveRDS(filter(countieslodes, lodes_type == "WAC"), WAC_LOC)
saveRDS(filter(countieslodes, lodes_type == "RAC"), RAC_LOC)

###### Generate OD file next #######

od_main <- grab_lodes(state = STATES, 
                      year = CURRENT_YEAR, 
                      lodes_type = "OD", 
                      job_type = PRIMARY_JT,
                      state_part = "main")

od_aux <- grab_lodes(state = STATES, 
                     year = CURRENT_YEAR, 
                     lodes_type = "OD", 
                     job_type = PRIMARY_JT,
                     state_part = "aux")

od <- rbind(od_main, od_aux) %>%
  mutate(w_geocode_tct = substr(w_geocode, 1, 11),
         h_geocode_tct = substr(h_geocode, 1, 11))

saveRDS(od, OD_LOC)

