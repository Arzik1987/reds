#' optimization of peel.alpha with WRAcc
#'
#' The function chooses one alpha value out of a given set using cross validation
#' and WRAcc as a quality measure
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param box matrix of real. Initial hyperbox, covering data
#' @param max.peels integer. Maximum length of the peeling trajectory (number of boxes)
#' @param peel.alpha a set of real. The peeling parameters of PRIM to test from the interval (0,1)
#'
#' @return the optimal value of the peeling parameter from the set
#'
#' @export
#'
#' @examples
#'
#' dtrain <- list()
#' dtrain[[1]] <- dsgc_sym[1:1000, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:1000, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' select.alpha.w(dtrain = dtrain, box = box,
#' peel.alpha = c(0.01, 0.03, 0.05, 0.07, 0.09, 0.11, 0.13, 0.15, 0.17, 0.19))


select.alpha.w <- function(dtrain, box, max.peels = 999, peel.alpha){
  a <- caret::createFolds(dtrain[[2]], k = 5, list = TRUE, returnTrain = FALSE)
  res <- matrix(rep(NA, length(a)*length(peel.alpha)), ncol = length(peel.alpha))
  for(j in 1:length(a)){
    for(i in 1:length(peel.alpha)){
      tetmp <- list(dtrain[[1]][a[[j]],], dtrain[[2]][a[[j]]])
      trtmp <- list(dtrain[[1]][-a[[j]],], dtrain[[2]][-a[[j]]])
      res[j,i] <- norm.prim.w(dtrain = trtmp, dtest = tetmp,
                                     box = box, max.peels = max.peels,
                                     peel.alpha = peel.alpha[i])$qtest
    }
  }
  res <- apply(res, 2, mean, na.rm  = TRUE)
  peel.alpha[which(res == max(res, na.rm = TRUE))[1]]
}
