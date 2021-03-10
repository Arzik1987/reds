# d <- dgp.morretal06(NULL, 10000)
# summary(d[[2]])

morretal06 <- function(xx, k1=2)
{
  ##########################################################################
  #
  # MORRIS ET AL. (2006) FUNCTION
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
  # INPUTS:
  #
  # xx = c(x1, x2, ..., x30)
  # k1 = number of arguments with an effect (optional), with default value
  #      2
  #
  ##########################################################################

  alpha <- sqrt(12) - 6*sqrt(0.1)*sqrt(k1-1)
  beta <- 12 * sqrt(0.1) * sqrt(k1-1)

  xi <- xx[1:k1]
  ximat <- matrix(rep(xi,times=k1), k1, k1, byrow=TRUE)
  ximatlow <- ximat
  ximatlow[!upper.tri(ximatlow)] <- 0

  inner <- rowSums(xi*ximatlow)
  outer <- sum(xi + beta*inner)

  y <- alpha * outer
  return(y)
}


dgp.morretal06 <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE, thr = -330){

  dim <- 30
  if(is.null(box)){
    box <- matrix(c(rep(0, dim), rep(1, dim)), nrow = 2, byrow = TRUE)
  }

  if(use.pts){
    if(ncol(pts) != dim) stop(paste0("pts should have ", dim, " dimensions"))
    d <- pts
  } else {
    if(ncol(box) != dim) stop(paste0("box should have ", dim, " dimensions"))
    d <- get.data(box, n.points, distr, nval)
  }

  y <- apply(d, 1, function(x) morretal06(x, k1 = 10))
  y <- ifelse(y < thr, 1, 0)

  return(list(d, y))
}
