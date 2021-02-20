###### Set up constant parameters #######

# Enter the state-county FIPS filter for which counties you would like returned
STATES <- c("OR", "WA")

# Note, first county in vector will be the basis for the projection
COUNTIES <- c("41051", "41009", "41027", "41005", "41053", "41047", "41067", "41071", "53011", "53015", "53059")

# Enter a name for the combination of states and counties selected (no spaces, will be used in output file name)
SELECTION_NAME <- "portland-11-county"

####### Constants for building OD/WAC/RAC files ########
# Enter what job type you want to return (starting 2016, the recommended default is private primary, or JT03)
PRIMARY_JT <- "JT03"

# Enter the most recently available year for which you'd like OD data (as of Dec 2020, latest is 2018)
CURRENT_YEAR <- 2018

# Enter the years you wish to collect RAC and WAC data for; not all states have availability for all years
LONGITUDINAL_YEARS <- 2002:2018

BUILD_DATE <- "2021-02-20"

## Specify directories of compiled WAC, RAC and OD files
WAC_LOC <- paste0(paste("data/processed/WAC", PRIMARY_JT, "all_segments", LONGITUDINAL_YEARS[1], "to", LONGITUDINAL_YEARS[length(LONGITUDINAL_YEARS)], SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")
RAC_LOC <- paste0(paste("data/processed/RAC", PRIMARY_JT, "all_segments", LONGITUDINAL_YEARS[1], "to", LONGITUDINAL_YEARS[length(LONGITUDINAL_YEARS)], SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")
OD_LOC <- paste0(paste("data/processed/OD_main_and_aux", PRIMARY_JT, CURRENT_YEAR, SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")


####### Constants for building spatial files ########
DESIRED_HEX_DIAMETER_IN_FEET <- 4000
area_in_ac <- 3*sqrt(3)/2*((DESIRED_HEX_DIAMETER_IN_FEET / sqrt(3))^2) / 43560
message(sprintf("A hexagon with a d2 of %i feet would have an area of %i acres.", DESIRED_HEX_DIAMETER_IN_FEET, round(area_in_ac, 0)))

BLOCKS_LOC <- paste0(paste("data/shapes/census_blocks", SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")
BLOCKS_SIMPLE_LOC <- paste0(paste("data/shapes/census_blocks_simplified", SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")
BLOCK_CENTROIDS_LOC <- paste0(paste("data/shapes/census_block_centroids", SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")
HEXGRID_LOC <- paste0(paste("data/shapes/hexgrid", round(area_in_ac, 0), "acre", SELECTION_NAME, BUILD_DATE, sep = "_"), ".rds")

## EPSG codes in meter units
county2epsg <- read_csv("https://gist.githubusercontent.com/fitnr/10795511/raw/7468f4fca23631019644bd18f1aa5dfc69ed1b1a/county-epsg.csv")
TARGET_EPSG <- county2epsg %>% filter(COUNTYFIPS == COUNTIES[1]) %>% pull(EPSG)
