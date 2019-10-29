#' Consistency based on data
#'
#' The function calculates consistency for two N-dimensional hyperboxes based on data
#' 
#' @param box1 real. Matrix defining the first box to compare
#' @param box2 real. Matrix defining the second box to compare
#' @param ds data to be used for calculations. Matrix/data frame of real attribute values
#' 
#' @return a number, quantidying consistency between two boxes based on data
#' 
#' @seealso \code{\link{consistency.v}},
#' \code{\link{consistency.l}}
#' 
#' @export
#' 
#' @examples
#' 
#' dtrain <- dtest <- list()
#' dtest[[1]] <- dsgc_sym[5001:10000, 1:12]
#' dtest[[2]] <- dsgc_sym[5001:10000, 13]
#' dtrain[[1]] <- dsgc_sym[1:5000, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:5000, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#' 
#' set.seed(1)
#' res1 <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#' res2 <- norm.prim(dtrain = dtest, dtest = dtrain, box = box)
#' box1 <- res1[[3]][[length(res1[[3]])]]
#' box2 <- res2[[3]][[length(res2[[3]])]]
#' consistency.d(box1, box2, dtrain)
#' consistency.d(box1, box2, dtest)

consistency.d <- function(box1, box2, ds){
  pred1 <- as.numeric(prim:::in.box(ds[[1]], box1, d = ncol(box1), boolean = TRUE))
  pred2 <- as.numeric(prim:::in.box(ds[[1]], box2, d = ncol(box2), boolean = TRUE))
  sum(pred1 == 1 & pred2 == 1)/sum(pred1 == 1 | pred2 == 1)
}
