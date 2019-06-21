# get county-level segregation indexes

require(tidyverse)
require(tidycensus)
source("scripts/incomeSegAsh.R")
source("scripts/incomeSegkde2d.R")

# a census API key needs to be installed
# To see what the variables are, do:
## v17 <- load_variables(2017, "acs5", cache = TRUE)

options(tigris_use_cache = TRUE)

county_seg <- readRDS("data/county_income.rds") # read data made using countyStats.R

fips <- county_seg$GEOID
st <- substr(fips, 1, 2)
cty <- substr(fips, 3, 5)
n <- length(fips)
segAsh <- numeric(n)
segKde2d <- numeric(n)
cat("Processing", n, "counties:\n")
for(i in seq(5)){
#  segAsh[i] <- incomeSegAsh(st[i], cty[i], nbin = c(500, 500), m = c(20, 20))
  segKde2d[i] <- incomeSegkde2d(st[i], cty[i])
  cat(".")
  if((i %% 50) == 0){
    cat(i, "\n")
  }
}
cat("\nFinished processing counties.\n")

county_seg %>% # other filters and mutates?
  add_column(segAsh, segKde2d) -> 
  county_seg
# saveRDS(county_income, "data/county_seg.rds")
