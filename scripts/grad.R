# Compute the gradient of a 2D function at a point

dgrad <- function(d, x, y) {
  if(any(is.na(c(x,y)))) return(c(NA,NA))
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

grad<- Vectorize(dgrad, vectorize.args = c("x", "y"))

lgrad <- function(d , x, y) {
  g <- grad(d, x, y)
  return(sqrt(g[1,]^2 + g[2,]^2))
}