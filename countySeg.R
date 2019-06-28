# get county-level segregation indexes

require(tidyverse)
require(tidycensus)
source("scripts/incomeSegAsh.R")
source("scripts/incomeSegkde2d.R")

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

fips <- county_income$GEOID
n <- length(fips)
segAsh <- numeric(n)
segKde2d <- numeric(n)
cat("Processing", n, "counties:\n")
for(i in seq(n)){
  segAsh[i] <- incomeSegAsh(fips[i], nbin = c(500, 500), m = c(20, 20))
  segKde2d[i] <- incomeSegkde2d(fips[i])
  cat(".")
  if((i %% 50) == 0){
    cat(i, "\n")
  }
}
cat("\nFinished processing counties.\n")

county_income %>% # other filters and mutates?
  add_column(segAsh, segKde2d) -> 
  county_seg
# saveRDS(county_income, "data/county_seg.rds")
big_county_seg <- county_seg %>% filter(B19001_001 > 200000)
