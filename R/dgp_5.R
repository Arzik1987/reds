dgp5 <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE){

  dim <- 5
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }

  if(use.pts){
    d <- pts
  } else {
    d <- get.data(box, n.points, distr, nval)
  }

  ya <- ifelse((d[, 1] + d[, 2]) > 1 & (d[, 1] + d[, 2]) < 1.3 & (d[, 2] - d[, 1]) > 0 &
                 (d[, 2] - d[, 1]) < 0.5, 1, 0)
  y <- ifelse(ya >= 0.3, 0.998, 0.002)
  y <- sapply(y, function(x) rbinom(1, 1, x))

  return(list(d, y))
}
