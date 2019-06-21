# Return a measure of income segregation given state and county fips and optional parameters
# a census api key needs to be installed

require(tidyverse)
require(tidycensus)
require(sf)
require(ash)
if(!exists("grad", mode="function")) source("scripts/grad.R")

incomeSegAsh <-
  function(state,
           county,
           nbin = c(500, 500),
           m = c(20, 20)) {
    cty <- get_acs(
      geography = "block group",
      state = state,
      county = county,
      variables = "B19013_001",
      # median income
      geometry = TRUE
    )
    ctyTot <- get_acs(
      geography = "county",
      state = state,
      county = county,
      variables = "B19013_001",
      # median income
      geometry = FALSE
    )
    ctyMedian <- ctyTot$estimate
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
    xa <- data.matrix(allObs[, 1:2])
    xs <- data.matrix(success[, 1:2])
    binsa <- bin2(xa, allRange, nbin)
    binss <- bin2(xs, allRange, nbin)
    a.hat <- ash2(binsa, m)
    s.hat <- ash2(binss, m)
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