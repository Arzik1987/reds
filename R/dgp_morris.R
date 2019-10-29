# d <- dgp.morris(NULL, 10000)
# summary(d[[2]])

dgp.morris <- function(box, n.points, laths = TRUE, pts = 0, use.pts = FALSE, thr = 20){
  
  dim <- 20
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
  
  y <- sensitivity::morris.fun(d)
  y <- ifelse(y < thr, 1, 0)
  
  return(list(d, y))
}
