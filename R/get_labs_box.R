#' Syntetic data generator
#'
#' The function samples points (if not provided) within a multi-dimensional hyperbox
#' and labels them according to pre-specified DGP
#'
#' @param box matrix of real, specifying the box, from which the points should be sampled. If null, the unit box is used
#' @param n.points integer. Number of points to be sampled and labeled
#' @param distr sampling strategy. "laths" stands for the latin hypercube sampling,
#' "tri" means i.i.d. samples of inputs from symmetric triangular distribution,
#' with "discr", points are sampled with latyn hypercube and even inputs discretized into "nval" values
#' @param nvaL used if \code{distr = "discr"}. Number of bins used for discretization.
#' @param pts matrix of real, containing examples to be labeled. Only active if use.pts is true.
#' @param use.pts logical. If true, pre-spesified points \code{pts} are labeled, otherwize, new \code{n.points}
#' points are sampled from the hyperbox \code{box}
#' @param dgp character. Specifies the DGP to be used.
#' \itemize{
#' \item "ellipse" labels with 1 points inside
#' an ellipse and with 0 otherwise.
#' \item Values from "1" to "10" for synthetic processess described in [1].
#' \item "borehole", "hart3", "hart4", "hart6sc", "ishigami", "linketal06dec",
#' "linketal06simple", "linketal06sin", "loepetal13", "moon10hd", "moon10hdc1", "moon10low", "morretal06",
#' "oakoh04", "otlcircuit", "piston", "soblev99", "welchetal92", "willetal06", "wingweight" are the DGPs
#' specified in [2].
#' \item "sobol" and "morris" are the functions defined in R package 'sensitivity'
#' }
#' @param ... parameters in the form \code{thr.x}, where \code{x} is one of possible \code{dgp} values.
#' Threshold value for the \code{x}-th DGP. Used to convert real function output into binary.
#'
#' @return list of two, containing a matrix of generated points and a vector of labels
#'
#' @references [1] Dalal, S. et al. 2013. Improving scenario discovery using orthogonal rotations.
#' Environmental Modelling and Software. 48, (2013), 49-64.
#'
#' [2] Surjanovic, S. & Bingham, D. (2013). Virtual Library of Simulation Experiments: Test Functions and Datasets.
#' \url{http://www.sfu.ca/~ssurjano}
#'
#' @export
#'
#' @examples
#' d <- get.labs.box(n.points = 100, dgp = "1")
#' plot(d[[1]][, 1:2], col = d[[2]] + 1, pch = 20)
#'

get.labs.box <- function(box = NULL, n.points = 0, distr = "laths", nval = 5,
                         pts = 0, use.pts = FALSE, dgp,
                         thr.hart3 = -1, thr.hart6sc = 1, thr.sobol = 0.7, thr.ishigami = 1,
                         thr.morris = 20, thr.otlcircuit = 4.5, thr.borehole = 1000,
                         thr.piston = 0.4, thr.wingweight = 250, thr.moon10hd = 0,
                         thr.moon10hdc1 = 0, thr.moon10low = 1.5, thr.welchetal92 = 0,
                         thr.linketal06dec = 0.15, thr.linketal06simple = 0.33,
                         thr.linketal06sin = 0, thr.loepetal13 = 9, thr.morretal06 = -330,
                         thr.oakoh04 = 10, thr.willetal06 = -1, thr.hart4 = -0.5,
                         thr.ellipse = 0.8, thr.soblev99 = 2000){
  if(dgp == "1"){
    res <- dgp1(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "2"){
    res <- dgp2(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "3"){
    res <- dgp3(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "4"){
    res <- dgp4(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "5"){
    res <- dgp5(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "6"){
    res <- dgp6(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "7"){
    res <- dgp7(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "8"){
    res <- dgp8(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "9"){
    res <- dgp9(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  } else if(dgp == "10"){
    res <- dgp10(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts)
  }

    else if(dgp == "hart3"){
    res <- dgp.hart3(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.hart3)
  } else if(dgp == "hart4"){
    res <- dgp.hart4(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.hart4)
  } else if(dgp == "hart6sc"){
    res <- dgp.hart6sc(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.hart6sc)
  }

    else if(dgp == "sobol"){
    res <- dgp.sobol(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.sobol)
  } else if(dgp == "morris"){
    res <- dgp.morris(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.morris)
  }

    else if(dgp == "otlcircuit"){
    res <- dgp.otlcircuit(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.otlcircuit)
  } else if(dgp == "borehole"){
    res <- dgp.borehole(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.borehole)
  } else if(dgp == "piston"){
    res <- dgp.piston(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.piston)
  } else if(dgp == "wingweight"){
    res <- dgp.wingweight(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.wingweight)
  }

    else if(dgp == "moon10hd"){
    res <- dgp.moon10hd(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.moon10hd)
  } else if(dgp == "moon10hdc1"){
    res <- dgp.moon10hdc1(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.moon10hdc1)
  } else if(dgp == "moon10low"){
    res <- dgp.moon10low(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.moon10low)
  } else if(dgp == "welchetal92"){
    res <- dgp.welchetal92(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.welchetal92)
  }

    else if(dgp == "linketal06dec"){
    res <- dgp.linketal06dec(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.linketal06dec)
  } else if(dgp == "linketal06simple"){
    res <- dgp.linketal06simple(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.linketal06simple)
  } else if(dgp == "linketal06sin"){
    res <- dgp.linketal06sin(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.linketal06sin)
  } else if(dgp == "loepetal13"){
    res <- dgp.loepetal13(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.loepetal13)
  } else if(dgp == "morretal06"){
    res <- dgp.morretal06(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.morretal06)
  } else if(dgp == "oakoh04"){
    res <- dgp.oakoh04(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.oakoh04)
  }

    else if(dgp == "willetal06"){
    res <- dgp.willetal06(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.willetal06)
  } else if(dgp == "ishigami"){
    res <- dgp.ishigami(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.ishigami)
  } else if(dgp == "soblev99"){
    res <- dgp.soblev99(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.soblev99)
  }

    else if(dgp == "ellipse"){
    res <- dgp.ellipse(box = box, n.points = n.points, distr = distr, nval = nval, pts = pts, use.pts = use.pts, thr = thr.ellipse)
  }

    else {
    stop("such DGP does not exist")
  }

  res
}


get.data <- function(box, n.points, distr, nval){

  if(distr == "laths"){
    d <- lhs::randomLHS(n.points, ncol(box))
  } else if(distr == "logitnorm") {
    d <- replicate(ncol(box), logitnorm::rlogitnorm(n.points, 0, 1))
  } else if(distr == "discr"){
    d <- lhs::randomLHS(n.points, ncol(box))
    for(i in seq(2, ncol(box), by = 2)){
      d[, i] <- make.discr(d[, i], nval = nval)
    }
  }

  for(i in 1:ncol(d)){
    d[, i] <- d[, i]*(box[2, i] - box[1, i]) + box[1, i]
  }

  d
}


make.discr <- function(x, low = 0, high = 1, nval){
  if(sum(x < low) + sum(x > high) > 0) stop("x values are beyond the boundaries")

  breaks = seq(low, high, length.out = nval + 1)
  for(i in 1:nval){
    x[x >= breaks[i] & x <= breaks[i + 1]] <- (breaks[i] + breaks[i + 1])/2
  }
  x
}

d.scale <- function(d, low, up){
  if(ncol(d) != length(low) | length(low) != length(up)){
    stop("wrong inputs for d.scale function")
  }
  for(i in 1:length(low)){
    d[, i] <- d[, i]*(up[i] - low[i]) + low[i]
  }
  d
}




