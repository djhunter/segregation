TN.tracts<- geo.make(tract = '*', county = '*', state = "TN") 
acs.fetch(geography=TN.tracts, table.number="B17001", endyear='2015', span=5, key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
TN_income<- acs.fetch(geography=TN.tracts, table.number="B17001", endyear='2015', span=5, key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")
data_frame(TN_income)
tibble(TN_income)



#The following below did not work:
TN_income1<- get_acs(geography=tract, table.number="B17001", year='2015', span=5, key = "ed694b0ac6ca4bed3dd8b6cadf8d53296401f6ac")



#I tried the sequence below:
vars <- paste0("B17001_", sprintf("%03d", 1:20))
TNinc <- get_acs(geography="tract", state="TN", variables = vars)



estimate(TN_income)
incomeTN<- estimate(TN_income)

TN<- get_acs(geography = "tract", variables = c("B17001_001", "B17001_002"), state = "TN")
TN


TN %>% spread(variable, estimate)


TNratio <- TN%>%select(-moe)%>%spread(variable, estimate)%>%mutate(ratio = B17001_002/B17001_001)

summary(TNratio$ratio, na.rm = TRUE)





CA<- get_acs(geography = "tract", variables = c("B17001_001", "B17001_002"), state = "CA", county = "Los Angeles", geometry = TRUE)
CA

LAratio <- CA%>%select(-moe)%>%spread(variable, estimate)%>%mutate(ratio = B17001_002/B17001_001)%>%mutate(hiPOV = ratio > 0.20)

summary(LAratio$ratio, na.rm = TRUE)

# High poverty = 20%

LAratio%>%ggplot(aes(fill = hiPOV)) + geom_sf() + xlim(-118.4, -118.0) + ylim(33.8, 34.2)

#4/12 - Error  in select(-moe).  So now  I can't create a function for the latlong coordinates in order to be able to run dgrad on it
#lat long needs to be in the form of ylim by xlim in order to highlight the inner city area of LA county
#Question: what does the function dgrad need to now incorporate in order to pick any region I want to creat a clear contour between areas of high poverty and not.


center<-function(g){colMeans(st_coordinates(g)[,1:2])}
LAcoord<-t(sapply(1:nrow(LAratio), function(i){center(LAratio$geometry[i])}))

LAratio<-cbind(LAratio, LAcoord)



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

#What is f.hat, s.hat, and p.hat?
#How do I plot the contour onto the geom-map?



strikes<- LAratio[LAratio$ratio<0.20, ]


#l <- c(range(na.omit(LAratio$X)), range(na.omit(LAratio$Y)))
l<- c(-118.4, -118.0, 33.8, 34.2)
s.hat <- kde2d(na.omit(strikes$X), na.omit(strikes$Y), lims = l, n = 200)
c.hat <- kde2d(na.omit(LAratio$X), na.omit(LAratio$Y), lims = l, n = 200)

p.hat <- (nrow(strikes))/ (nrow(LAratio))

f.hat <- s.hat
f.hat$z <- (p.hat*f.hat$z)/c.hat$z

f.contour <- contourLines(f.hat, levels = 0.5)
fdf <- list()
for(i in 1: length(f.contour)){
  fdf[[i]]<- data.frame(x = f.contour[[i]]$x,  y = f.contour[[i]]$y)
}

LAratio%>%ggplot() + 
  geom_sf(aes(fill = hiPOV)) + 
  geom_path(data = fdf[[1]], aes(x=x, y=y)) + 
  geom_path(data = fdf[[2]], aes(x=x, y=y)) + 
  xlim(-118.4, -118.0) + 
  ylim(33.8, 34.2) 






