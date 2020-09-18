#' Optimization of peel.alpha with PR AUC
#'
#' The function chooses one alpha value out of a given set using cross validation
#' and PR AUC as a quality measure
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param box matrix of real. Initial hyperbox, covering data
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param max.peels integer. Maximum length of the peeling trajectory (number of boxes)
#' @param peel.alpha a set of real. The peeling parameters of PRIM to test from the interval (0,1)
#' @param threshold real. If precision of the current box on \code{test}
#' is greater or equal \code{threshold}, PRIM stops peeling
#' @param seed seed for reproducibility. Set NULL for not using
#'
#' @return the optimal value of the peeling parameter from the set
#'
#' @export
#'
#' @examples
#'
#' dtrain <- list()
#' dtrain[[1]] <- dsgc_sym[1:500, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:500, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' select.alpha(dtrain = dtrain, box = box, minpts = 20, max.peels = 1000,
#' peel.alpha = c(0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.2),
#' threshold = 1, seed = 2020)


select.alpha <- function(dtrain, box, minpts, max.peels, peel.alpha, threshold, seed){
  set.seed(seed = seed)

  a <- caret::createFolds(dtrain[[2]], k = 5, list = TRUE, returnTrain = FALSE)
  res <- matrix(rep(NA, length(a)*length(peel.alpha)), ncol = length(peel.alpha))
  for(j in 1:length(a)){
    for(i in 1:length(peel.alpha)){
      tetmp <- list(dtrain[[1]][a[[j]],], dtrain[[2]][a[[j]]])
      trtmp <- list(dtrain[[1]][-a[[j]],], dtrain[[2]][-a[[j]]])
      tmp <- norm.prim(dtrain = trtmp, dtest = tetmp, deval = trtmp,
                box = box, minpts = minpts, max.peels = max.peels,
                peel.alpha = peel.alpha[i],
                threshold = threshold)$pr.test
      tmp <- tmp[tmp[, 1] > 0, , drop = FALSE]
      res[j,i] <- qual.auc(tmp)
    }
  }
  res <- apply(res, 2, mean, na.rm  = TRUE)
  peel.alpha[which(res == max(res, na.rm = TRUE))[1]]
}
