# Return a measure of income segregation given state and county fips and optional parameters
# a census api key needs to be installed

require(ks)
require(tidyverse)
require(tidycensus)
require(sf)
if(!exists("grad", mode="function")) source("scripts/grad.R")

# needs all_block geom (get_acs_tractdata.R)
# needs county_income (countyStats.R)
if(!exists("all_block_geom")) {
  all_block_geom <- readRDS("data/all_block_geom.rds")
}
if(!exists("county_income")) {
  county_income <- readRDS("data/county_income.rds")
}

incomeSegKS <-
  function(fips,              # five-digit state/county code
    maxBlockArea = 15000000)  # Don't consider sparse block groups
    {
    cty <-
      all_block_geom %>% filter(substr(GEOID,1,5) == fips)
    ctyMedian <-
      county_income %>% filter(GEOID == fips) %>% .$B19013_001
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
    allObs %>% filter(aboveMed) -> success
    aspect <- (max(allObs$X) - min(allObs$X))/(max(allObs$Y) - min(allObs$Y))
    gsize <- c(round(200*aspect), 200)
    support <- 3.7
    abw <- Hpi(cbind(allObs$X,allObs$Y))
    sbw <- Hpi(cbind(success$X,success$Y))
    a.hat <- kde(cbind(allObs$X,allObs$Y), gridsize = gsize,
                 xmin=c(min(allObs$X), min(allObs$Y)), xmax=c(max(allObs$X), max(allObs$Y)), 
                 supp = support, H = abw)
    s.hat <- kde(cbind(success$X,success$Y), gridsize = gsize, 
                 xmin=c(min(allObs$X), min(allObs$Y)), xmax=c(max(allObs$X), max(allObs$Y)), 
                 supp = support, H = sbw)
    f.hat.kde <- a.hat
    rok <- (a.hat$estimate > 0.00001) # avoid dividing by zero, and ignore tails
    f.hat.kde$estimate[rok] <- s.hat$estimate[rok] / a.hat$estimate[rok] * nrow(success) / nrow(allObs)
    f.hat <- list(x = f.hat.kde$eval.points[[1]],
                  y = f.hat.kde$eval.points[[2]],
                  z = f.hat.kde$estimate)
    border <- contourLines(f.hat, levels = 0.5)
    if(length(border) == 0)
      return(NA) # No contours?
    for(i in seq(length(border))){
      # remove parts of contour that fall outside our region
      c <- st_union(cty$geometry)
      b <- cbind(border[[i]]$x, border[[i]]$y)
      pts <- st_sfc(lapply(seq(nrow(b)), function(j) {st_point(b[j,])}))
      st_crs(pts) <- st_crs(c)
      in_cty <- suppressMessages(st_intersects(pts, c, sparse = FALSE)[,1])
      border[[i]]$x[!in_cty] <- NA
      border[[i]]$y[!in_cty] <- NA
    }
    tot <- 0
    totAG <- 0
    for (i in seq(length(border))) {
      g <- grad(f.hat, border[[i]]$x, border[[i]]$y)
      magg <- sqrt(g[1,] ^ 2 + g[2,] ^ 2)
      nmagg <- sum(!is.na(magg))
      if(nmagg != 0) {
        aveGrad <- mean(magg, na.rm = TRUE)
        totAG <- totAG + aveGrad * nmagg
        tot <- tot + nmagg
      }
    }
    return(totAG / tot)
  }
