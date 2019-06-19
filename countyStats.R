# get county-level statistics (population, median income, gini, theil)

require(tidyverse)
require(tidycensus)
require(binsmooth)

# a census API key needs to be installed
# To see what the variables are, do:
## v17 <- load_variables(2017, "acs5", cache = TRUE)

options(tigris_use_cache = TRUE)

cDat <- get_acs(
  geography = "county",
  year = 2017,
  variables = c("B19013_001", # median income
                "B19001_001", # total households
                "B19001_002", "B19001_003", "B19001_004", "B19001_005", 
                "B19001_006", "B19001_007", "B19001_008", "B19001_009", 
                "B19001_010", "B19001_011", "B19001_012", "B19001_013", 
                "B19001_014", "B19001_015", "B19001_016", "B19001_017", # income brackets
                "B19025_001" # aggregate household income
                ),
  geometry = FALSE
  )
  
cDat %>% 
  filter(variable %in% c("B19001_001", "B19013_001", "B19025_001")) %>% 
  select(GEOID, NAME, variable, estimate) %>% 
  spread(variable, estimate) %>% 
  mutate(meanInc = B19025_001/B19001_001) %>% # mean income
  arrange(GEOID) -> # should already be in order, but just being safe 
  county_income
m <- county_income$meanInc

fips <- county_income$GEOID
n <- length(fips)
Gini <- numeric(n)
Theil <- numeric(n)
## TODO: calculate Gini and Theil in loop
binedges <- c(10000,15000,20000,25000,30000,35000,40000,45000,
              50000,60000,75000,100000,125000,150000,200000,NA)
cat("Processing", n, "counties:\n")
for(i in seq(n)){
  cDat %>% 
    filter(GEOID == fips[i]) %>% 
    arrange(variable) %>% # already probably in order, but just being safe
    filter(variable >= "B19001_002" & variable <= "B19001_017") %>% # income brackets
    .$estimate -> 
    bincounts
  sp.fit <- splinebins(binedges, bincounts, m[i])
  Gini[i] <- gini(sp.fit)
  Theil[i] <- theil(sp.fit)
  cat(".")
  if((i %% 50) == 0){
    cat(i, "\n")
  }
}
cat("\nFinished processing counties.\n")

county_income %>% # other filters and mutates?
  add_column(Gini, Theil) -> 
  county_income
# saveRDS(county_income, "data/county_income.rds")
