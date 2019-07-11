# get county-level spatial segregation indexes (uding seg and seg gradient)

require(tidyverse)
require(tidycensus)
require(seg)

if(!exists("whiteSeg2D", mode="function")) source("scripts/whiteSeg2D.R")

# a census API key needs to be installed
# To see what the variables are, do:
## v17 <- load_variables(2017, "acs5", cache = TRUE)

options(tigris_use_cache = TRUE)

if(!exists("all_block_geom")) {
  all_block_geom <- readRDS("data/all_block_geom.rds") # made using get_acs_tractdata.R
}
if(!exists("all_block_white")) {
  all_block_white <- readRDS("data/all_block_white.rds")
}
if(!exists("county_income")) {
  county_income <- readRDS("data/county_income.rds") # read data made using countyStats.R
}

# Omit small counties
county_income %>% filter(B19001_001 > 200000) -> big_county_white

fips <- big_county_white$GEOID
n <- length(fips)
seg_dsm <- numeric(n)
seg_d <- numeric(n)
seg_r <- numeric(n)
seg_h <- numeric(n)
seg_p1 <- numeric(n)
seg_p2 <- numeric(n)
seg_2D <- numeric(n)
cat("Processing", n, "counties:\n")
maxBlockArea <- 15000000
for(i in seq(n)){
    cty <- 
      all_block_geom %>% filter(substr(GEOID,1,5) == fips[i])
    cty %>% mutate(
      centroid = suppressWarnings(st_centroid(geometry)),
      bgarea = as.numeric(st_area(geometry)),
      X = do.call(rbind, centroid)[, 1],
      Y = do.call(rbind, centroid)[, 2]
    ) %>%
      filter(bgarea < maxBlockArea) ->
      cty
    cty <- left_join(cty, all_block_white, by = "GEOID")
    cty %>% 
      dplyr::select(X, Y, B02001_001, B02001_002) %>% 
      filter(!is.na(X) & !is.na(Y)) ->
      allObs
    st_geometry(allObs) <- NULL
    ssg <- spseg(x = allObs[,1:2], data = cbind(allObs$B02001_002, allObs$B02001_001-allObs$B02001_002))
    # equivalently: ssg <- spseg(x = as_Spatial(cty$geometry), data = cbind(allObs$B02001_002, allObs$B02001_001-allObs$B02001_002))
    # gives error: dsm <- dissim(x = as_Spatial(cty$geometry), data = cbind(allObs$B02001_002, allObs$B02001_001-allObs$B02001_002))
    #seg_dsm[i] <- dsm$d
    seg_d[i] <- ssg@d 
    seg_r[i] <- ssg@r 
    seg_h[i] <- ssg@h 
    seg_p1[i] <- ssg@p[1,1] 
    seg_p2[i] <- ssg@p[2,1]
    seg_2D[i] <- whiteSeg2D(fips[i])
    cat(".")
    if((i %% 50) == 0){
      cat(i, "\n")
    }
}
cat("\nFinished processing counties.\n")

big_county_white %>% # other filters and mutates?
  add_column(seg_dsm, seg_d, seg_r, seg_h, seg_p1, seg_p2, seg_2D) -> 
  big_county_raceseg
big_county_raceseg %>% mutate(seg_2Di = atan(seg_2D)/(pi/2)) -> big_county_raceseg
# saveRDS(big_county_raceseg, "data/county_raceseg.rds")