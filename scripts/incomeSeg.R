# Return a measure of income segregation given state and county fips and optional parameters
# a census api key needs to be installed

require(tidyverse)
require(tidycensus)
require(sf)
if(!exists("grad", mode="function")) source("grad.R")

incomeSeg <-
  function(state,
           county,
           nbin = c(500, 500),
           m = c(20, 20)) {
    cty <- get_acs(
      geography = "block group",
      state = state,
      county = county,
      variables = "B19013_001", # median income
      geometry = TRUE
    )
    ctyMedian <- ??? TODO
  }