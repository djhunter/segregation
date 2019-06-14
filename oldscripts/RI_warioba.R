showClass("acs")
# load some data from the ACS
data(kansas09)
str(kansas09)



install.packages("devtools")
devtools::install_github("hrecht/censusapi")
24.0


mycensuskey <- ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac


`myresults <- getCensus(name="apiname", 
                        vintage=year, key="mycensuskey",
                        vars=c("datacolumn1", "datacolumn2","datacolumn3"),
                        region="censusregions")`


?devtools

myvintage <- 2015 


availablevars <- listCensusMetadata(name="acs1", vintage=myvintage)
View(availablebars)


data(fips_codes)
data(county_laea)



## Not run:
library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)
ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(options = "magma")
vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")
vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")
## End(Not run)


library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)
ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(options = "magma")
vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")
vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")


get_acs


library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)
ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(options = "magma")

?ggplot
?ggplot

get_acs


library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)
ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(options = "magma")
vt <- get_acs(geography = "county", variables = "B19013_001", state = "VT")
vt %>%
  mutate(NAME = gsub(" County, Vermont", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 3) +
  labs(title = "Household income by county in Vermont",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")

library(tidycensus)
library(tidyverse)
library(viridis)
census_api_key("ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
tarr <- get_acs(geography = "tract", variables = "B19013_001",
                state = "TX", county = "Tarrant", geometry = TRUE)
ggplot(tarr, aes(fill = estimate, color = estimate)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_viridis(option = "magma") +
  scale_color_viridis(options = "magma")


get(acs.colnames(kansas09))

acs.lookup(endyear=2012, span=1, keyword=c("Female", "GED"))
acs.tables.install(table.name)

age.by.sex=acs.lookup(endyear=2014, span=5, table.name="Age by Sex")


acs.fetch(endyear=2015, span=5, variable=acs.lookup(endyear=2014, span=5, table.number="B01003"))







library(rgdal)    
library(sp)       
library(leaflet)  
library(dplyr)    
library(ggplot2) 

#use tidyverse package
#if I give acs a tract number, can I get the average income of all the people in that region
#for some geographic area

install.packages("tidyverse")

#use gazetteer to get longitude, latitude and then use acs to get specific census tracts and their longitudes and lattitude/ variable such as income for a county
#datacarpentry.org/R-genomics

letters <- read_csv("data/correspondence-data-1585.csv")
sources <- distinct(letters, source)
destinations <- distinct(letters, destination)
cities <- full_join(sources, destinations, by = c("source" = "destination"))
cities <- rename(cities, place = source)
head(cities, n = 2)
cities_df <- as.data.frame(cities)
locations_df <- mutate_geocode(cities_df, place)
?mutate_
df <- data.frame(
  address = c("1600 Pennsylvania Avenue, Washington DC", "", "houston texas"),
  stringsAsFactors = FALSE
)


df %>% mutate_geocode(address)
install.packages(ggmap)
install.packages(viridis)






latlong2state<-function(pointsDF){
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


latlong2state<-function(points.default){
  states <- Map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- Map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
  
}




testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))

latlong2state(testPoints)

latlong<- read_tsv("Downloads/2018_Gaz_tracts_national.txt")
latlong %>% filter(USPS=="RI") %>% select(GEOID, INTPTLAT, INTPTLONG)
RI<-latlong %>% filter(USPS=="RI") %>% select(GEOID, INTPTLAT, INTPTLONG)



rifetch<- acs.fetch(geography=RI.tracts, table.number="B03002", endyear='2015', span=5, key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")

RI$tract<- substr(RI$GEOID, 6, 11)

RI$tract

riincome<- acs.fetch(geography=RI.tracts, table.number="S1902", endyear='2015', span=1, key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")

acs.lookup(endyear=2012, span=1, table.number="S1901", table.name = "Income In The Past 12 Months", case.sensitive = F)


acs.lookup(endyear=2012, span=1, table.number="B01001", keyword="Female", key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")

acs.lookup(endyear=2014, span=5, table.number="B01001", key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
acs.tables.install(B01001)

RhodeIsland=geo.make(state="RI")
acs.fetch(endyear=2014, geography = RhodeIsland, table.number="B01003",key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")


exampleRI<-acs.fetch(endyear=2014, geography = RhodeIsland, table.number="B01003",key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")

RI

example2RI<-acs.fetch(endyear=2014, geography = RhodeIsland, table.number="B01003",key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
example2RI

incomeGEN<-read_csv("Downloads/SG1600A2.dat")











rifetch<- acs.fetch(geography=RI.tracts, table.number="B03002", endyear='2015', span=5, key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")

data_frame(rifetch)
data_frame(latlong)
data_frame(RI)






RI.tracts<- geo.make(tract = '*', county = 'Providence', state = "RI") 


estimate(rifetch)
RIdf<- estimate(rifetch)

census_api_key("ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac", install = TRUE)


ri <- get_acs(geography = "tract", variables = c("B03002_002", "B03002_012"), state = "RI")
head(ri)
head(RI)

ri%>% select(estimate, moe)
(ri%>% select(estimate))/((ri%>% select(moe)))
ricol122<- (ri%>% select(estimate))/((ri%>% select(moe)))
head(ricol122)
?left_join

?mutate
?group_by
ri %>% group_by(GEOID)
?summarise
?right_join

full_join(RI, ri, by = NULL, copy = FALSE, suffix = c(".RI", ".ri"))
joinedRI<- full_join(RI, ri, by = "GEOID", copy = FALSE, suffix = c(".RI", ".ri"))

joinedRI %>% select(GEOID, INTPTLAT, INTPTLONG, estimate, moe)
RI2 <- joinedRI %>% select(GEOID, INTPTLAT, INTPTLONG, estimate, moe)

full_join(RI2, ricol122, by = NULL, copy = FALSE, suffix = c(".RI2", ".ricol122"))

?separate

separate(ri, variable, into = NA)

spread(ri, variable, estimate)

ri%>%select(-moe)%>%spread(variable, estimate)

ri%>%select(-moe)%>%spread(variable, estimate)%>%mutate(diff = B03002_002 - B03002_012)
ridiff <- ri%>%select(-moe)%>%spread(variable, estimate)%>%mutate(diff = B03002_002 - B03002_012)

left_join(RI, ridiff, by = "GEOID")
RIbyGEOID<- left_join(RI, ridiff, by = "GEOID")

RIbyGEOID[RIbyGEOID$diff<0, ]
strikes<- RIbyGEOID[RIbyGEOID$diff<1000, ]
balls<- RIbyGEOID[RIbyGEOID$diff>=1000, ]
strikes

l <- c(range(RIbyGEOID$INTPTLAT), range(RIbyGEOID$INTPTLONG))

s.hat <- kde2d(strikes$INTPTLAT, strikes$INTPTLONG, lims = l)
c.hat <- kde2d(RIbyGEOID$INTPTLAT, RIbyGEOID$INTPTLONG, lims = l)
nrow(strikes)
p.hat <- (nrow(strikes))/ (nrow(RIbyGEOID))

f.hat <- s.hat
f.hat$z <- (p.hat*f.hat$z)/c.hat$z

plot(s.hat)
plot(p.hat)
plot(c.hat)
?contour
contour(s.hat, nlevels = 30)
?kde2d
?contour 
contour(f.hat)  
  
?contourLines
f.contour <- contourLines(f.hat, levels = 0.5)

length(f.contour)
#dosomething(f.contour[[i]]) in for loop

fdf <- list()
for(i in 1: length(f.contour)){
  fdf[[i]]<- data.frame(x = f.contour[[i]]$x,  y = f.contour[[i]]$y)
  
}
  

ggplot() + geom_path(data = fdf[[1]], aes(x=x, y=y)) + geom_path(data = fdf[[2]], aes(x=x, y=y)) + geom_path(data = fdf[[3]], aes(x=x, y=y))


ggplot() + geom_path(data = fdf[[2]], aes(x=x, y=y))

dgrad1 <- function(d, x, y) {
  ix <- which.min(abs(d$x - x))
  iy <- which.min(abs(d$y - y))
  R <- d$z[ix + 1, iy]
  L <- d$z[ix-1, iy]
  deltax <- d$x[2] - d$x[1]
  deltay <- d$y[2] - d$y[1]
  dx <- ((R - L) / (2*deltax))
  R <- d$z[ix, iy + 1]
  L <- d$z[ix, iy-1]
  dy <- ((R - L) / (2*deltay))
  return(c(dx, dy))
}

vgrad<- Vectorize(dgrad1, vectorize.args = c("x", "y"))


m<- vgrad(f.hat, f.contour[[2]]$x, f.contour[[2]]$y)

mean(sqrt(m[1, ]^2 + m[2, ]^2))

m<- vgrad(f.hat, f.contour[[3]]$x, f.contour[[3]]$y)

mean(sqrt(m[1, ]^2 + m[2, ]^2))

m<- vgrad(f.hat, f.contour[[1]]$x, f.contour[[1]]$y)

mean(sqrt(m[1, ]^2 + m[2, ]^2))

m





dgrad2 <- function(d, x, y) {
  ix <- which.min(abs(d$x - x))
  iy <- which.min(abs(d$y - y))
  if(ix == 1 | iy == 1 | ix == dim(d$z)[1] | iy == dim(d$z)[2])
    return(c(NA,NA))
  R <- d$z[ix + 1, iy]
  L <- d$z[ix-1, iy]
  deltax <- d$x[2] - d$x[1]
  deltay <- d$y[2] - d$y[1]
  dx <- ((R - L) / (2*deltax))
  R <- d$z[ix, iy + 1]
  L <- d$z[ix, iy-1]
  dy <- ((R - L) / (2*deltay))
  return(c(dx, dy))
}

vgrad<- Vectorize(dgrad2, vectorize.args = c("x", "y"))


m<- vgrad(f.hat, f.contour[[2]]$x, f.contour[[2]]$y)

mean(sqrt(m[1, ]^2 + m[2, ]^2))

m<- vgrad(f.hat, f.contour[[3]]$x, f.contour[[3]]$y)

mean(sqrt(m[1, ]^2 + m[2, ]^2))

m<- vgrad(f.hat, f.contour[[1]]$x, f.contour[[1]]$y)

mean(sqrt(m[1, ]^2 + m[2, ]^2), na.rm = TRUE)

m



