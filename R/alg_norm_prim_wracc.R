#' PRIM returning the box maximizing WRAcc
#'
#' The function applies PRIM to train data and evaluates its quality on test data
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}
#' @param box matrix of real. Initial hyperbox, covering data
#' @param max.peels integer. Maximum length of the peeling trajectory (number of boxes)
#' @param peel.alpha real. The peeling parameter of PRIM from the interval (0,1)
#'
#' @keywords models, multivariate
#'
#' @references Friedman, J.H. and Fisher, N.I. 1999. Bump hunting in high-dimensional data.
#' Statistics and Computing. 9, 2 (1999), 123-143.
#'
#' @return list.
#' \itemize{
#' \item \code{qtest} WRAcc value computed from \code{dtest}
#' \item \code{qtrain} WRAcc value computed from \code{dtrain}
#' \item \code{box} the hyperbox, with highest value of WRAcc on \code{dtrain}
#' }
#'
#' @importFrom stats quantile
#'
#' @seealso \code{\link{norm.prim}},
#' \code{\link{best.interval}}
#'
#' @export
#'
#' @examples
#'
#' dtrain <- dtest <- list()
#' dtest[[1]] <- dsgc_sym[1001:10000, 1:12]
#' dtest[[2]] <- dsgc_sym[1001:10000, 13]
#' dtrain[[1]] <- dsgc_sym[1:1000, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:1000, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' vals <- c(0.01, 0.03, 0.05, 0.07, 0.09, 0.11, 0.13, 0.15, 0.17, 0.19)
#' rtr <- rte <- numeric()
#' for(i in vals){
#'   tmp <- norm.prim.w(dtrain = dtrain, dtest = dtest, box = box, peel.alpha = i)
#'   rtr <- c(rtr, tmp[[2]])
#'   rte <- c(rte, tmp[[1]])
#' }
#'
#' plot(vals, seq(min(c(rtr, rte)), max(c(rtr,rte)), along.with = vals), type = "n")
#' lines(vals, rtr, col = "red")
#' lines(vals, rte, col = "blue")
#'
#' set.seed(1)
#' norm.prim.w(dtrain = dtrain, dtest = dtest, box = box, peel.alpha = 0.05)$qtest
#' norm.prim.w(dtrain = dtrain, dtest = dtest, box = box, peel.alpha = vals)$qtest
#' best.interval(dtrain = dtrain, dtest = dtest, box = box, depth = ncol(dtrain[[1]]), beam.size = 1)$qtest


norm.prim.w <- function(dtrain, dtest = NULL, box, max.peels = 999,
                     peel.alpha = 0.05){

  if(length(peel.alpha) > 1){
    peel.alpha <- select.alpha.w(dtrain = dtrain, box = box,
                               max.peels = max.peels, peel.alpha = peel.alpha)
  }

  peel <- function(){

    hgh <- -Inf
    bnd <- -Inf
    vol.red <- 1

    for(i in 1:ncol(x)){
      bound <- quantile(x[, i], peel.alpha, type = 8)
      vol.r <- (bound - box[1, i])/(box[2, i] - box[1, i])
      retain <- (x[, i] >= bound)                                   # this inequality implicitly assumes low (< peel.alpha) share of duplicates for each value
      tar <- sum(y[retain])/N - sum(retain)*Np/(N^2)
      if(tar > hgh | (tar == hgh & vol.r < vol.red)){
        hgh <- tar
        vol.red <- vol.r
        inds <- retain
        rn = 1
        cn = i
        bnd = bound
      }
      bound <- quantile(x[, i], 1 - peel.alpha, type = 8)
      vol.r <- (box[2, i] - bound)/(box[2, i] - box[1, i])
      retain <- (x[, i] <= bound)
      tar <- sum(y[retain])/N - sum(retain)*Np/(N^2)
      if(tar > hgh | (tar == hgh & vol.r < vol.red)){
        vol.red <- vol.r
        hgh <- tar
        inds <- retain
        rn = 2
        cn = i
        bnd = bound
      }
    }
    x <<- x[inds,]
    y <<- y[inds]
    box[rn, cn] <<- bnd
    q <<- hgh
    continue.peeling <<- ((sum(inds)/length(inds)) < 1 & hgh < threshold)
  }

  x <- dtrain[[1]]
  y <- dtrain[[2]]
  N <- length(y)
  Np <- sum(y)
  continue.peeling <- TRUE
  threshold <- Np/N - (Np/N)^2

  i = 0
  boxes <- list()
  q <- 0
  qtrain <- numeric(0)
  maxq <- 0

  while(continue.peeling & i <= max.peels){

    i <- i + 1
    boxes <- append(boxes, list(box))
    qtrain <- c(qtrain, q)
    peel()

    if(sum(y) < maxq*Np/threshold){
      continue.peeling <- FALSE
    }
  }

  ret <- which(qtrain == max(qtrain))[1]
  box <- boxes[[ret]]
  qtest <- NULL
  if(!is.null(dtest)){
    qtest <- qual.wracc(dtest, box)
  }

  return(list(qtest = qtest, qtrain = qtrain[ret], box = box))
}


