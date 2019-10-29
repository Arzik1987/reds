dgp9 <- function(box, n.points, laths = TRUE, pts = 0, use.pts = FALSE){
  
  dim <- 15
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }
  
  if(use.pts){
    d <- pts
  } else {
    d <- get.data(box, n.points, laths)
  }
  
  ya <- exp(-20 + d[,1]^5 + d[,2]^4 + d[,3]^5 + d[,4]^4 + d[,5]^5 + d[,6]^4 + d[,7]^5 + d[,8]^4 +
              d[,8]^5 + d[,10]^4 + d[,11]^5 + d[,12]^4)/
    (1 + exp(-20 + d[,1]^5 + d[,2]^4 + d[,3]^5 + d[,4]^4 + d[,5]^5 + d[,6]^4 + d[,7]^5 + d[,8]^4 +
               d[,10]^4 + d[,11]^5 + d[,12]^4))
  y <- ifelse(ya >= 0.3, ya + 0.85*(1 - ya), 0.2*ya)
  y <- sapply(y, function(x) rbinom(1, 1, x))
  
  return(list(d, y))
}
