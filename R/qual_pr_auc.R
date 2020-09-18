#' AUC metric for PRIM
#'
#' The function calculates "area under the peeling curve" in "coverage-density" coordinates
#' 
#' @param pr matrix with coverage (recall) in the first column and 
#' density (precision) in the second column
#' 
#' @return real. AUC value
#' 
#' @export
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
#' set.seed(1)
#' res <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#'
#' # AUC on test data
#' qual.auc(res[[1]])
#' 
#' # AUC on train data
#' qual.auc(res[[2]])

qual.auc <- function(pr){
  pr <- rbind(pr, c(0, pr[nrow(pr), 2]))
  x <- -diff(pr[, 1])
  y <- halfsum(pr[, 2])
  sum(x*y) - pr[1, 2]
}

halfsum <- function(a){
  res <- numeric()
  for(k in 2:length(a)){
    res <- c(res, (a[k] + a[k - 1])/2)
  }
  res
}
