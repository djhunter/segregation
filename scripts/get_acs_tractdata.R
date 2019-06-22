# Download and save all the data we need
# a census api key needs to be installed

require(tidyverse)
require(tidycensus)
require(sf)

# a census API key needs to be installed
# To see what the variables are, do:
## v17 <- load_variables(2017, "acs5", cache = TRUE)

options(tigris_use_cache = TRUE)

bk_geom <- get_acs(
      geography = "block group",
      state = "RI", # TODO: delete this line and try at office
      variables = "B19013_001", # median household income                
      geometry = TRUE,
      cache_table = TRUE
    )
saveRDS(bk_geom, file="data/all_block_geom.rds")