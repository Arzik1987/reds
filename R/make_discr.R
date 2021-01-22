
# set.seed(1)
# x <- runif(10)
# make.discr(x,0,1,10)
# x <- rnorm(10)
# make.discr(x,0,1,10)

make.discr <- function(x, low = 0, high = 1, nval = 10){
  if(sum(x < low) + sum(x > high) > 0) stop("x values are beyond the boundaries")
  
  breaks = seq(low, high, length.out = nval + 1)
  for(i in 1:nval){
    x[x >= breaks[i] & x <= breaks[i + 1]] <- (breaks[i] + breaks[i + 1])/2 
  }
  x
}
