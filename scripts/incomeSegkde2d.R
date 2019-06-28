# Return a measure of income segregation given state and county fips and optional parameters
# a census api key needs to be installed

require(MASS)
require(tidyverse)
require(tidycensus)
require(sf)
if(!exists("grad", mode="function")) source("scripts/grad.R")

# needs all_block geom (get_acs_tractdata.R)
# needs county_income (countyStats.R)

incomeSegkde2d <-
  function(fips) { # fips is five-digit state/county code
    cty <- 
      all_block_geom %>% filter(substr(GEOID,1,5) == fips)
    ctyMedian <- 
      county_income %>% filter(GEOID == fips) %>% .$B19013_001
    cty %>% mutate(
      centroid = suppressWarnings(st_centroid(geometry)),
      X = do.call(rbind, centroid)[, 1],
      Y = do.call(rbind, centroid)[, 2],
      aboveMed = (estimate > ctyMedian)
    ) ->
      cty
    cty %>% dplyr::select(X, Y, aboveMed) %>%
      filter(!is.na(aboveMed)) ->
      allObs
    st_geometry(allObs) <- NULL
    allObs %>% filter(aboveMed) -> success
    allRange <-
      matrix(c(range(allObs$X), range(allObs$Y)), 2, 2, byrow = TRUE)
    if(any(allRange[,1]==allRange[,2]))
      return(NA) # county not 2D?
    if(nrow(success) < 2)
      return(NA) # too few block groups to be meaningful
    s.hat <- kde2d(success$X, success$Y, n = 200, lims = c(range(allObs$X),range(allObs$Y)))
    a.hat <- kde2d(allObs$X, allObs$Y, n = 200, lims = c(range(allObs$X),range(allObs$Y)))
#    a.hat$z[a.hat$z < 0.05] <- 0 # truncate the tails
    f.hat <- s.hat
    f.hat$z <- f.hat$z / a.hat$z * nrow(success) / nrow(allObs)
    f.hat$z[is.nan(f.hat$z)] <- NA ## replace all 0/0's with NA's
    border <- contourLines(f.hat, levels = 0.5)
    tot <- 0
    totAG <- 0
    if(length(border) == 0)
      return(NA) # No contours?
    for (i in seq(length(border))) {
      g <- grad(f.hat, border[[i]]$x, border[[i]]$y)
      magg <- sqrt(g[1,] ^ 2 + g[2,] ^ 2)
      nmagg <- sum(!is.na(magg))
      aveGrad <- mean(magg, na.rm = TRUE)
      totAG <- totAG + aveGrad * nmagg
      tot <- tot + nmagg
    }
    return(totAG / tot)
  }