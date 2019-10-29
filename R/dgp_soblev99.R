# d <- dgp.soblev99(NULL, 10000)
# summary(d[[2]])

soblev99 <- function(xx, c0=0)
{
  d <- 20
  b <- c(2, 1.95, 1.9, 1.85, 1.8, 
         1.75, 1.7, 1.65, 0.4228, 0.3077, 
         0.2169, 0.1471, 0.0951, 0.0577, 0.0323, 
         0.0161, 0.0068, 0.0021, 0.0004, 0)

  Id <- 1
  for (ii in 1:d) {
    bi  <- b[ii]
    new <- ifelse(bi > 0, (exp(bi)-1) / bi, 0)
    Id  <- Id * new
  }
  
  sum <- 0
  for (ii in 1:d) {
    bi  <- b[ii]
    xi  <- xx[ii]
    sum <- sum + bi*xi
  }
  
  y <- exp(sum) - Id + c0
  return(y)
}


dgp.soblev99 <- function(box, n.points, laths = TRUE, pts = 0, use.pts = FALSE, thr = 2000){
  
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

  y <- apply(d, 1, soblev99)
  y <- ifelse(y < thr, 1, 0)
  
  return(list(d, y))
}
