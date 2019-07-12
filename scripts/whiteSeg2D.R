# Return a measure of income segregation given state and county fips and optional parameters
# a census api key needs to be installed

require(ks)
require(tidyverse)
require(tidycensus)
require(sf)
if(!exists("grad", mode="function")) source("scripts/grad.R")

# needs all_block geom (get_acs_tractdata.R)
# needs county_income (get_acs_racedata.R)
if(!exists("all_block_geom")) {
  all_block_geom <- readRDS("data/all_block_geom.rds")
}
if(!exists("all_block_white")) {
  all_block_white <- readRDS("data/all_block_white.rds")
}

whiteSeg2D <-
  function(fips,              # five-digit state/county code
    maxBlockArea = 15000000)  # Don't consider sparse block groups
    {
    cty <-
      all_block_geom %>% filter(substr(GEOID,1,5) == fips)
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
    aspect <- (max(allObs$X) - min(allObs$X))/(max(allObs$Y) - min(allObs$Y))
    gsize <- c(round(200*aspect), 200)
    a.hat <- suppressWarnings(kde(cbind(allObs$X,allObs$Y), gridsize = gsize, 
                                  xmin=c(min(allObs$X), min(allObs$Y)), xmax=c(max(allObs$X), max(allObs$Y)), 
                                  w = allObs$B02001_001))
    s.hat <- suppressWarnings(kde(cbind(allObs$X,allObs$Y), gridsize = gsize, 
                                  xmin=c(min(allObs$X), min(allObs$Y)), xmax=c(max(allObs$X), max(allObs$Y)), 
                                  w = allObs$B02001_002))
    f.hat.kde <- a.hat
    rok <- (a.hat$estimate > 0.00000) # avoid dividing by zero
    f.hat.kde$estimate[rok] <- s.hat$estimate[rok] / a.hat$estimate[rok] * sum(allObs$B02001_002) / sum(allObs$B02001_001)
    f.hat <- list(x = f.hat.kde$eval.points[[1]],
                  y = f.hat.kde$eval.points[[2]],
                  z = f.hat.kde$estimate)
    gfhat <- as_tibble(expand.grid(f.hat$x, f.hat$y))
    names(gfhat) <- c("x", "y")
    # Tag points that are outside the region
    c <- st_union(cty$geometry)
    pts <- st_sfc(lapply(seq(nrow(gfhat)), function(j) {st_point(as.numeric(gfhat[j,]))}))
    st_crs(pts) <- st_crs(c)
    in_cty <- suppressMessages(st_intersects(pts, c, sparse = FALSE)[,1])
    g <- grad(f.hat, gfhat[in_cty,]$x, gfhat[in_cty,]$y)
    magg <- sqrt(g[1,]^2 + g[2,]^2)
    nmagg <- sum(!is.na(magg))
    aveGrad <- mean(magg, na.rm = TRUE)
    return(aveGrad)
  }
