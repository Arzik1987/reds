dgp7 <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE){

  dim <- 5
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }

  if(use.pts){
    d <- pts
  } else {
    d <- get.data(box, n.points, distr, nval)
  }

  ya <- exp(-25 + 28*(d[, 1]*d[, 1] + d[, 2]*d[, 2]))/(1 + exp(-25 + 28*(d[, 1]*d[, 1] + d[, 2]*d[, 2])))
  y1 <- sapply(ya, function(x) rbinom(1, 1, 1 - x))

  ya <- exp(-25 + 28*(1.2*(1 - d[, 1])^2 + (1 - d[, 2])^2))/(1 + exp(-25 + 28*(1.2*(1 - d[, 1])^2 + (1 - d[, 2])^2)))
  y2 <- sapply(ya, function(x) rbinom(1, 1, 1 - x))

  y <- apply(rbind(y1, y2), 2, min)
  return(list(d, y))
}
