#' Coverage and density of PRIM result
#'
#' The function calculates precision and recall metrics for one-box classifier assuming that the box predicts
#' label "1" and outside the box is label "0"
#' @param pred vector containing true labels (0/1) of the dataset, 
#' which correspond to the points within the evaluated box (e.g., one, found with PRIM algorithm)
#' @param orig vector with true labels (0/1) of the whole dataset
#' 
#' @return matrix of real. Coverage in the first column and density in the second
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
#' box1 <- res[[3]][[length(res[[3]])]]
#' pred <- prim:::in.box(dtest[[1]], box1, d = ncol(box1), boolean = TRUE)
#' pred <- dtest[[2]][pred]
#' 
#' get.metrics(pred, dtest[[2]])
#' res[[1]][nrow(res[[1]]),]

get.metrics <- function(pred, orig){
  prec <- sum(pred)/length(pred)
  rec <- sum(pred)/sum(orig)
  matrix(c(rec, prec), ncol = 2)
}