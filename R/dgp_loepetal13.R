# d <- dgp.loepetal13(NULL, 10000)
# summary(d[[2]])

loepetal13 <- function(xx)
{
  ##########################################################################
  #
  # LOEPPKY ET AL. (2013) FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it 
  # and/or modify it under the terms of the GNU General Public License as 
  # published by the Free Software Foundation; version 2.0 of the License. 
  # Accordingly, this program is distributed in the hope that it will be 
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., x10)
  #
  ##########################################################################
  
  x1 <- xx[1]
  x2 <- xx[2]
  x3 <- xx[3]
  x4 <- xx[4]
  x5 <- xx[5]
  x6 <- xx[6]
  x7 <- xx[7]
  
  term1 <- 6*x1 + 4*x2
  term2 <- 5.5*x3 + 3*x1*x2
  term3 <- 2.2*x1*x3 + 1.4*x2*x3
  term4 <- x4 + 0.5*x5
  term5 <- 0.2*x6 + 0.1*x7
  
  y <- term1 + term2 + term3 + term4 + term5
  return(y)
}


dgp.loepetal13 <- function(box, n.points, laths = TRUE, pts = 0, use.pts = FALSE, thr = 9){
  
  dim <- 10
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
  
  y <- apply(d, 1, loepetal13)
  y <- ifelse(y < thr, 1, 0)
  
  return(list(d, y))
}
