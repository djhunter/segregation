if(!exists("incomeSeg", mode="function")) source("scripts/incomeSeg.R")
data(fips_codes)
options(tigris_use_cache = TRUE)

n <- nrow(fips_codes)
countyIncSeg <- fips_codes
countyIncSeg$IncSeg <- numeric(n)
for(i in seq(n)) {
  countyIncSeg$IncSeg[i] <- incomeSeg(countyIncSeg$state_code[i], 
                                      countyIncSeg$county_code[i],
                                      m = c(20,20))
  cat("Processed", i, "of", n, "counties.\n")
}