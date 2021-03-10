dgp8 <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE){

  dim <- 5
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }

  if(use.pts){
    d <- pts
  } else {
    d <- get.data(box, n.points, distr, nval)
  }

  ya <- exp(-20 + 20*(d[, 1] + d[, 2]))/(1 + exp(-20 + 20*(d[, 1] + d[, 2])))
  y1 <- sapply(ya, function(x) rbinom(1, 1, 1 - x))

  ya <- exp(-20 + 20*(1 - d[, 1] + 0.5*d[, 2]))/(1 + exp(-20 + 20*(1 - d[, 1] + 0.5*d[, 2])))
  y2 <- sapply(ya, function(x) rbinom(1, 1, 1 - x))

  ya <- exp(-20 + 20*(d[, 1] + 4*d[, 2]))/(1 + exp(-20 + 20*(d[, 1] + 4*d[, 2])))
  y3 <- sapply(ya, function(x) rbinom(1, 1, 1 - x))

  y <- apply(rbind(y1, y2, y3), 2, min)
  return(list(d, y))
}
