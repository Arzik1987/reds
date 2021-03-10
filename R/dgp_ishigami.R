# d <- dgp.ishigami(NULL, 10000)
# summary(d[[2]])

ishigami <- function(xx, a=7, b=0.1)
{
  ##########################################################################
  #
  # ISHIGAMI FUNCTION
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
  # xx = c(x1, x2, x3)
  # a = coefficient (optional), with default value 7
  # b = coefficient (optional), with default value 0.1
  #
  ##########################################################################

  x1 <- xx[1]
  x2 <- xx[2]
  x3 <- xx[3]

  term1 <- sin(x1)
  term2 <- a * (sin(x2))^2
  term3 <- b * x3^4 * sin(x1)

  y <- term1 + term2 + term3
  return(y)
}


dgp.ishigami <- function(box, n.points, distr = "laths", nval = 5, pts = 0, use.pts = FALSE, thr = 1){

  dim <- 3
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

  low <- rep(-pi, dim)
  up <- rep(pi, dim)

  y <- apply(d.scale(d, low, up), 1, ishigami)
  y <- ifelse(y < thr, 1, 0)

  return(list(d, y))
}
