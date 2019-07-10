# get county-level segregation indexes

require(tidyverse)
require(tidycensus)
if(!exists("incomeSeg2D", mode="function")) source("scripts/incomeSeg2D.R")

# a census API key needs to be installed
# To see what the variables are, do:
## v17 <- load_variables(2017, "acs5", cache = TRUE)

options(tigris_use_cache = TRUE)

if(!exists("all_block_geom")) {
  all_block_geom <- readRDS("data/all_block_geom.rds") # made using get_acs_tractdata.R
}
if(!exists("county_income")) {
  county_income <- readRDS("data/county_income.rds") # read data made using countyStats.R
}

# Omit small counties
county_income %>% filter(B19001_001 > 200000) -> big_county_income

fips <- big_county_income$GEOID
n <- length(fips)
seg2D <- numeric(n)
cat("Processing", n, "counties:\n")
for(i in seq(n)){
  # seg2D[i] <- incomeSegKS(fips[i]) # Uses contours only
  seg2D[i] <- incomeSeg2D(fips[i])   # uses whole area
  cat(".")
  if((i %% 50) == 0){
    cat(i, "\n")
  }
}
cat("\nFinished processing counties.\n")

big_county_income %>% # other filters and mutates?
  add_column(seg2D) -> 
  big_county_seg
# saveRDS(county_seg, "data/county_segKS.rds") # used contours only
# saveRDS(big_county_seg, "data/county_seg2D.rds")
