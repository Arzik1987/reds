#' Optimization of peel.alpha and depth with PR AUC
#'
#' The function chooses one \code{peel.alpha} value and one value of \code{depth}
#' out of given sets for \code{\link{bumping.prim}} algorithm
#' using cross validation and PR AUC as a quality measure
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param box matrix of real. Initial hyperbox, covering data
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param peel.alpha a set of real. The peeling parameters of PRIM to test from the interval (0,1)
#' @param depth a set of integers to select from.
#' Refers to the numbers of attributes used in each iteration.
#' @param iter integer. Parameter of \code{\link{bumping.prim}}.
#' @param seed seed for reproducibility. Set NULL for not using
#'
#' @return a vector of size two. The first element - the optimal value of
#' \code{peel.alpha}; teh second value - the optimal value of \code{depth}
#'
#' @export
#'
#' @seealso \code{\link{bumping.prim}},
#'
#' @examples
#'
#' dtrain <- list()
#' dtrain[[1]] <- dsgc_sym[1:400, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:400, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' select.param(dtrain = dtrain, box = box, minpts = 20,
#' peel.alpha = c(0.17), iter = 10, depth = c(1, 4, 12), seed = 2020)


select.param <- function(dtrain, box, minpts, peel.alpha, depth, iter, seed){
  set.seed(seed = seed)

  a <- caret::createFolds(dtrain[[2]], k = 5, list = TRUE, returnTrain = FALSE)
  res <- array(rep(NA, length(a)*length(peel.alpha)*length(depth)),
                dim = c(length(a), length(peel.alpha), length(depth)))

  for(i in 1:length(a)){
    tetmp <- list(dtrain[[1]][a[[i]],], dtrain[[2]][a[[i]]])
    trtmp <- list(dtrain[[1]][-a[[i]],], dtrain[[2]][-a[[i]]])
    for(j in 1:length(peel.alpha)){
      for(k in 1:length(depth)){
        tmp <- bumping.prim(dtrain = trtmp, dtest = tetmp, deval = trtmp,
                            box = box, minpts = minpts,
                            peel.alpha = peel.alpha[j],
                            depth = depth[k],
                            iter = iter)$pr.test
        tmp <- tmp[tmp[, 1] > 0, , drop = FALSE]
        res[i, j, k] <- qual.auc(tmp)
      }
    }
  }
  res <- apply(res, c(2,3), mean, na.rm  = TRUE)
  inds <- which(res == max(res, na.rm = TRUE), arr.ind = TRUE)[1,]
  c(peel.alpha[inds[1]], depth[inds[2]])
}
