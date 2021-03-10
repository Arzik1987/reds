dgp10 <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE){

  dim <- 15
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }

  if(use.pts){
    d <- pts
  } else {
    d <- get.data(box, n.points, distr, nval)
  }

  ya <- exp(-12 + 10*d[,1] + 9*d[,2] + 8*d[,3])/(1 + exp(-12 + 10*d[,1] + 9*d[,2] + 8*d[,3]))
  y1 <- ifelse(ya >= 0.3, ya + 0.85*(1 - ya), 0.2*ya)
  y1 <- sapply(y1, function(x) rbinom(1, 1, x))

  ya <- exp(-12 + 10*d[,6] + 5*d[,7] + 10*d[,8])/(1 + exp(-12 + 10*d[,6] + 5*d[,7] + 10*d[,8]))
  y2 <- ifelse(ya >= 0.3, ya + 0.85*(1 - ya), 0.2*ya)
  y2 <- sapply(y2, function(x) rbinom(1, 1, x))

  ya <- exp(-12 + 8*d[,10] + 8*d[,11] + 8*d[,12])/(1 + exp(-12 + 8*d[,10] + 8*d[,11] + 8*d[,12]))
  y3 <- ifelse(ya >= 0.3, ya + 0.85*(1 - ya), 0.2*ya)
  y3 <- sapply(y3, function(x) rbinom(1, 1, x))

  y <- y1 + y2 + y3
  y <- ifelse(y >= 2, 1, 0)
  return(list(d, y))
}
