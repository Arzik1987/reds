# d <- dgp.ellipse(NULL, 10000)
# summary(d[[2]])

dgp.ellipse <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE, thr = 0.8){

  center <- c(0.9751507, 0.8432682, 0.7724470, 0.3247967, 0.8051028,
              0.9450661, 0.2212505, 0.7315873, 0.2888812, 0.5998075,
              0.7098766, 0.3620747, 0.8861793, 0.1429155, 0.9121198)
  weights <- c(0.3532409, 0.4340412, 0.8993585, 0.3734097, 0.2784368,
               0.1640473, 0.9266303, 0.7685996, 0.9747707, 0.6060011,
               0, 0, 0, 0, 0)

  dim <- 15
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }

  if(use.pts){
    if(ncol(pts) != dim) stop(paste0("pts should have ", dim, " dimensions"))
    d <- pts
  } else {
    if(ncol(box) != dim) stop(paste0("box should have ", dim, " dimensions"))
    d <- get.data(box, n.points, distr, nval)
  }

  y <- labs <- apply(d, 1, function(x) sum(weights*(x - center)^2))
  y[labs < thr^2] <- 1
  y[labs >= thr^2] <- 0

  return(list(d, y))
}
