# Download and save all the data we need
# a census api key needs to be installed

require(tidyverse)
require(tidycensus)
require(sf)

# a census API key needs to be installed
# To see what the variables are, do:
## v17 <- load_variables(2017, "acs5", cache = TRUE)

options(tigris_use_cache = TRUE)
county_income <- readRDS("data/county_income.rds")
st_code <- unique(substr(county_income$GEOID, 1,2))

# for(s in st_code[-(1:47)]) { # do this if ACS fails
for(s in st_code) { 
  bk_geom <- get_acs(
        geography = "block group",
        state = s,
        variables = c("B02001_001", # race total
                      "B02001_002"), # white alone
        geometry = FALSE,
        cache_table = TRUE
      )
  saveRDS(bk_geom, file=paste0("data/all_block_race", s, ".rds"))
  cat("Saved data for state code", s, "\n")
}

# Once all states have been downloaded, we can combine them 
# into a single tibble

all_block_race_list <- lapply(st_code, function(s) {readRDS(paste0("data/all_block_race", s, ".rds"))})
# all_block_geom_old <- bind_rows(all_block_geom_list) # faster, but loses attributes
all_block_race <- do.call(rbind, all_block_race_list)
saveRDS(all_block_race, "data/all_block_race.rds")

all_block_race %>% 
  select(-moe) %>% 
  spread(variable, estimate) ->
  all_block_white
saveRDS(all_block_white, "data/all_block_white.rds")