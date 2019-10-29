dgp.sobol <- function(box, n.points, laths, pts, use.pts, thr = 0.7){
  
  dim <- 8
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }  
  
  if(use.pts){
    if(ncol(pts) != dim) stop(paste0("pts should have ", dim, " dimensions"))
    d <- pts
  } else {
    if(ncol(box) != dim) stop(paste0("box should have ", dim, " dimensions"))
    d <- get.data(box, n.points, laths)
  }
  
  y <- sensitivity::sobol.fun(d)
  y <- ifelse(y < thr, 1, 0)
  
  return(list(d, y))
}