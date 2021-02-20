pacman::p_load(tidyverse, lehdr, purrr)

# plan(multisession)

# Enter the state-county FIPS filter for which counties you would like returned
COUNTIES <- c("04019", "04023", "04021", "04003", "04009") # , "04013") # Leave out Maricopa
COUNTIES <- c("41005", "41009", "41027", "41051", "41053", "41047", "41067", "41071", "53011", "53015", "53059")
STATES <- "AZ"
STATES <- c("OR", "WA")

inputs <- list(
  year = c(2015:2018), # Not all states have consistent yearly availability
  lodes_type = c("WAC", "RAC", "OD"),
  job_type = c("JT00", "JT01", "JT02", "JT03", "JT04", "JT05"),
  segment = c("S000", "SE01", "SE02", "SE03", "SA01", "SA02", "SA03", "SI01", "SI02", "SI03")
) 

inputs <- inputs %>%
  # Cross the list to generate inputs into the download function
  purrr::cross_df() %>%
  # Filter for the data you want to grab
  filter(job_type == "JT03", lodes_type != "OD") %>%
  as.list() %>%
  setNames(names(inputs))

download_lodes_longitudinal <- function(state, year, lodes_type, job_type, segment, download_dir = "data/lodes_raw") {
  map_df(.x = year, .f = ~grab_lodes(state = state, 
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

countieslodes <- statelodes %>%
  filter(substr(w_geocode, 1, 5) %in% COUNTIES | 
           substr(h_geocode, 1, 5) %in% COUNTIES) %>%
  select(w_geocode, h_geocode, everything())

saveRDS(filter(countieslodes, lodes_type == "WAC"), "data/processed/wac_az_pima_2004t2018_jt03_all_segments.RDS")

saveRDS(filter(countieslodes, lodes_type == "RAC"), "data/processed/rac_az_pima_2004t2018_jt03_all_segments.RDS")




## Origin-destination file
## Load in the OD file
od_main <- grab_lodes(state = STATE, year = 2018, lodes_type = "OD", job_type = "JT00",
                           segment = "S000", state_part = "main")%>%
  mutate(COUNTYFP = substr(w_geocode, start = 3, stop = 5))

od_aux<-lehdr::grab_lodes(state = c("or", "wa"), year = 2017, lodes_type = "od", job_type = "JT00",
                          segment = "S000", state_part = "aux")%>%
  mutate(COUNTYFP = substr(w_geocode, start = 3, stop = 5))

od <- rbind(od_main, od_aux) %>%
  mutate(w_geocode_tct = substr(w_geocode, 1, 11),
         h_geocode_tct = substr(h_geocode, 1, 11))

saveRDS(od, "./data/od_main_aux_orwa_2017_jt00_s000.RDS")

