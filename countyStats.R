# get county-level statistics (population, median income, gini, theil)

require(tidyverse)
require(tidycensus)
require(binsmooth)

# a census API key needs to be installed

options(tigris_use_cache = TRUE)

cDat <- get_acs(
  geography = "county",
  year = 2017,
  variables = c("B00001_001", # total population
                "B19013_001", # median income
                "B19001_001", # total households
                "B19001_002", "B19001_003", "B19001_004", "B19001_005", 
                "B19001_006", "B19001_007", "B19001_008", "B19001_009", 
                "B19001_010", "B19001_011", "B19001_012", "B19001_013", 
                "B19001_014", "B19001_015", "B19001_016", "B19001_017", # income brackets
                "B19025_001" # aggregate income
                ),
  geometry = FALSE
  )
  
n <- nrow(cDat)
Gini <- numeric(n)
Theil <- numeric(n)
## TODO: calculate Gini and Theil in loop

  cat("Processed", i, "of", n, "counties.\n")

cDat %>% # other filters and mutates?
  add_column(Gini, Theil) -> 
  countyStats

