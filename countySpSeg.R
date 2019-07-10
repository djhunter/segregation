# get county-level spatial segregation indexes (uding seg)

require(tidyverse)
require(tidycensus)
require(seg)

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
seg_d <- numeric(n)
seg_r <- numeric(n)
seg_h <- numeric(n)
seg_p1 <- numeric(n)
seg_p2 <- numeric(n)
cat("Processing", n, "counties:\n")
maxBlockArea <- 15000000
for(i in seq(n)){
    cty <- 
      all_block_geom %>% filter(substr(GEOID,1,5) == fips[i])
    ctyMedian <- 
      county_income %>% filter(GEOID == fips[i]) %>% .$B19013_001
    cty %>% mutate(
      centroid = suppressWarnings(st_centroid(geometry)),
      bgarea = as.numeric(st_area(geometry)),
      X = do.call(rbind, centroid)[, 1],
      Y = do.call(rbind, centroid)[, 2],
      aboveMed = (estimate > ctyMedian)
    ) %>%
      filter(bgarea < maxBlockArea) %>%
      filter(!is.na(aboveMed)) ->
      cty
    cty %>% 
      dplyr::select(X, Y, aboveMed) ->
      allObs
    st_geometry(allObs) <- NULL
    ssg <- spseg(x = allObs[,1:2], data = cbind(as.numeric(allObs$aboveMed), as.numeric(!allObs$aboveMed)))
    # equivalent? spseg(x = as_Spatial(cty$geometry), data = cbind(as.numeric(allObs$aboveMed), as.numeric(!allObs$aboveMed)))
    seg_d[i] <- ssg@d 
    seg_r[i] <- ssg@r 
    seg_h[i] <- ssg@h 
    seg_p1[i] <- ssg@p[1,1] 
    seg_p2[i] <- ssg@p[2,1]
    cat(".")
    if((i %% 50) == 0){
      cat(i, "\n")
    }
}
cat("\nFinished processing counties.\n")

big_county_income %>% # other filters and mutates?
  add_column(seg_d, seg_r, seg_h, seg_p1, seg_p2) -> 
  big_county_spseg
# saveRDS(big_county_spseg, "data/county_spseg.rds")
