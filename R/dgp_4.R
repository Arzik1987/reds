dgp4 <- function(box, n.points, laths = TRUE, pts = 0, use.pts = FALSE){
  
  dim <- 5
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }
  
  if(use.pts){
    d <- pts
  } else {
    d <- get.data(box, n.points, laths)
  }
  
  ya <- exp(-5 + 8 - abs(-5*d[, 1] + 8*d[, 2] - 8))/(1 + exp(-5 + 8 - abs(-5*d[, 1] + 8*d[, 2] - 8)))
  y <- ifelse(ya >= 0.3, ya + 0.85*(1 - ya), 0.2*ya)
  y <- sapply(y, function(x) rbinom(1, 1, x))
  
  return(list(d, y))
}