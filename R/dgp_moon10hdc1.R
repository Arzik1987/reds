# d <- dgp.moon10hdc1(NULL, 10000)
# summary(d[[2]])

moon10hdc1 <- function(xx)
{
  #########################################################################
  #
  # MOON (2010) HIGH-DIMENSIONALITY FUNCTION, C-1
  # This function is a modification of the function moon10hd.r.
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
  # xx = c(x1, x2, ..., x20)
  #
  ##########################################################################

  x1  <- xx[1]
  x7  <- xx[7]
  x12 <- xx[12]
  x18 <- xx[18]
  x19 <- xx[19]

  term1 <- -19.71*x1*x18 + 23.72*x1*x19
  term2 <- -13.34*x19^2 + 28.99*x7*x12

  y <- term1 + term2
  return(y)
}


dgp.moon10hdc1 <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE, thr = 0){

  dim <- 20
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

  y <- apply(d, 1, moon10hdc1)
  y <- ifelse(y < thr, 1, 0)

  return(list(d, y))
}
