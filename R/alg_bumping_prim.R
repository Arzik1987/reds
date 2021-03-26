#' PRIM with bumping
#'
#' The function implements PRIM with bumping (originally, bumping)
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}. If NULL, the
#' quality metrics on test data are not computed
#' @param deval list, containing data for evaluation. Structured in the same way as \code{dtrain}.
#' By default coincides with \code{dtrain}
#' @param box hyperbox, covering data
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param max.peels integer. Maximum length of the peeling trajectory (number of boxes)
#' @param peel.alpha a set of real. The peeling parameter(s) of PRIM from the interval (0,1).
#' If a vector, the value is selected with \code{\link{select.alpha}} algorithm
#' @param threshold real. If precision of the current box on \code{train}
#' is greater or equal \code{threshold}, PRIM stops peeling
#' @param depth integer, vector of integers, "cv" or "all"; parameter specifying
#' the search depth (the number of restricted attributes). If "all", the full data
#' dimensionality is used; if a vector, the value is selected with
#' \code{\link{select.param}} algorithm; if "cv" a vector of maximum \code{denom}
#' equidistant values is created and a single value is selected with
#' \code{\link{select.param}} algorithm.
#' @param iter integer. The number of iterations (sequences of boxes learned). Default is 50
#' @param denom the maximal length of the set of \code{depth} values to choose from
#' @param seed seed for reproducibility. Default is 2020. Set NULL for not using
#'
#' @keywords models, multivariate
#'
#' @export
#'
#' @references J. H. Kwakkel and S. C. Cunningham, "Improving scenario discovery by bagging random boxes,"
#' Technological Forecasting and Social Change, vol. 111, pp. 124-134, 2016.
#'
#' @return list.
#' \itemize{
#' \item \code{pr.test} matrix with coverage (recall) in the first column and
#' density (precision) in the second column, evaluated on \code{dtest}
#' \item \code{pr.eval} matrix with coverage (recall) in the first column and
#' density (precision) in the second column, evaluated on \code{deval}
#' \item \code{boxes} list of matrices defining boxes constituting peeling trajectory
#' \item \code{peel.alpha} integer; the value of \code{peel.alpha} parameter used
#' \item \code{depth} integer; the value of \code{depth} parameter used
#' }
#'
#' @keywords models, multivariate
#'
#' @seealso \code{\link{norm.prim}},
#' \code{\link{reds.prim}}
#'
#' @examples
#'
#' dtrain <- dtest <- list()
#' dtest[[1]] <- dsgc_sym[1001:10000, 1:12]
#' dtest[[2]] <- dsgc_sym[1001:10000, 13]
#' dtrain[[1]] <- dsgc_sym[1:500, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:500, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' res <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#' res.b <- bumping.prim(dtrain = dtrain, dtest = dtest, box = box, iter = 10)
#' res.b.cv <- bumping.prim(dtrain = dtrain, dtest = dtest, box = box,
#' depth = "cv", peel.alpha = c(0.05, 0.1, 0.15), iter = 10)
#'
#' plot(res[[1]], type = "l", xlim = c(0, 1), ylim = c(0.5, 1),
#' xlab = "recall", ylab = "precision")
#' lines(res[[2]], col = "black", lty = 3)
#' lines(res.b[[1]], lty = 1, col = "red")
#' lines(res.b[[2]], lty = 3, col = "red")
#' lines(res.b.cv[[1]], lty = 1, col = "blue")
#' lines(res.b.cv[[2]], lty = 3, col = "blue")
#' legend("bottomleft", legend = c("norm test", "norm train",
#' "bumping all test", "bumping all train", "bumping cv train",
#' "bumping cv test"),
#' lty = c(1, 3, 1, 3, 1, 3),
#' col = c("black", "black", "red", "red", "blue", "blue"))


bumping.prim <- function(dtrain, dtest = NULL, deval = dtrain, box, minpts = 20,
                         max.peels = 999, peel.alpha = 0.05, threshold = 1,
                         depth = "all", iter = 50, denom = 6, seed = 2020){

  time1 = Sys.time()

  nc <- ncol(dtrain[[1]])

  if(length(peel.alpha) > 1){
    peel.alpha <- select.alpha(dtrain = dtrain, box = box, minpts = minpts,
                               max.peels = max.peels, peel.alpha = peel.alpha,
                               threshold = threshold, seed = seed)
    print("finished with selecting peel.alpha")
  }

  if(depth[1] == "cv"){
    depth = -(seq(-nc, -1, by = ceiling(nc/denom)))
  }

  if(length(depth) > 1){
    depth <- select.param(dtrain = dtrain, box = box, minpts = minpts,
                          peel.alpha = peel.alpha, depth = depth,
                          iter = iter, seed = seed)[2]
  }

  if(depth == "all"){
    depth <- nc
  }

  if(depth > nc){
    warning("Restricting depth parameter to the number of atributes in data!")
    depth <- nc
  }

  d <- dtrain
  dt <- dtest
  de <- deval
  N <- length(dtrain[[2]])

  set.seed(seed = seed)

  for(i in 1:iter){
    ind <- sample(1:N, N, replace = TRUE)
    indc <- sample(1:nc, depth, replace = FALSE)
    d[[1]] <- dtrain[[1]][ind, indc, drop = FALSE]
    d[[2]] <- dtrain[[2]][ind]
    de[[1]] <- deval[[1]][, indc, drop = FALSE]
    boxt <- box[, indc, drop = FALSE]

    temp <- norm.prim(dtrain = d, dtest = NULL, deval = de, box = boxt,
                      minpts = minpts, max.peels = max.peels,
                      peel.alpha = peel.alpha, pasting = FALSE,
                      threshold = threshold)
    if(i == 1){ # TODO: get rid of this construction
      pr.eval <- temp[[2]]
      boxes <- lapply(temp[[3]], function(x){a <- box; a[,indc] <- x; return(a)})
    } else {
      pr.eval <- rbind(pr.eval, temp[[2]])
      boxes <- append(boxes, lapply(temp[[3]], function(x){a <- box; a[,indc] <- x; return(a)}))
    }
  }

  ind <- get.dominant(pr.eval)
  pr.eval <- pr.eval[ind,, drop = FALSE]
  boxes <- boxes[ind]

  time.train = Sys.time() - time1

  qual.pr <- function(d, box.p){
    Np = sum(d[[2]])
    retain <- rep(TRUE, length(d[[2]]))
    for(i in 1:ncol(d[[1]])){
      retain <- retain & d[[1]][, i] > box.p[1, i]
      retain <- retain & d[[1]][, i] < box.p[2, i]
    }
    n = length(d[[2]][retain])
    np = sum(d[[2]][retain])
    rec <- np/Np
    pr <- np/n
    c(rec, pr, sum(retain))
  }

  pr.test <- matrix(ncol = 2, nrow = 0)
  if(!is.null(dtest)){
    for(i in boxes){
      pr.test <- rbind(pr.test, qual.pr(dtest, i)[1:2])
    }
  }


  res <- list(pr.test = pr.test, pr.eval = pr.eval, boxes = boxes,
              peel.alpha = peel.alpha, depth = depth, time.train = time.train)
  return(res)
}
