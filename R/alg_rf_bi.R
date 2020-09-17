#' RF -> BestInterval
#'
#' The function learns RF model on a given dataset. Then it generates new points with latin hypercube sampling
#' and labels them. These new points serve as input for BestInterval algorithm.
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}
#' @param box matrix of real. Initial hyperbox, covering data
#' @param peel.alpha real. The peeling parameter of PRIM from the interval (0,1)
#' @param npts number of points to be generated and labeled
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param grow.deep logical. If true, criterion \code{minpts} is applied to the newly
#' generated dataset, not to the initial one
#'
#' @keywords models, multivariate
#'
#' @return list.
#' \itemize{
#' \item \code{pr.prob} matrix with coverage (recall) in the first column and
#' density (precision) in the second column, evaluated on \code{dtest} and produced
#' when the new data is labelled with probabilities
#' \item \code{pr.pred} matrix with coverage (recall) in the first column and
#' density (precision) in the second column, evaluated on \code{dtest} and produced
#' when the new data is labelled with 0/1
#' \item \code{boxes.prob} list of matrices defining boxes constituting peeling trajectory
#' when the new data is labelled with probabilities
#' \item \code{boxes.pred} list of matrices defining boxes constituting peeling trajectory
#' when the new data is labelled with 0/1
#' \item \code{tune.par} the best hyperparameter value(s) for random forest, produced with
#' the default settings of function \code{train} from 'caret' package.
#' }
#'
#' @importFrom stats predict
#'
#' @seealso \code{\link{norm.prim}},
#' \code{\link{bagging.prim}}
#'
#' @export
#'
#' @examples
#'
#' dtrain <- dtest <- list()
#' dtest[[1]] <- dsgc_sym[1:9500, 1:12]
#' dtest[[2]] <- dsgc_sym[1:9500, 13]
#' dtrain[[1]] <- dsgc_sym[9501:10000, 1:12]
#' dtrain[[2]] <- dsgc_sym[9501:10000, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' set.seed(1)
#' res.rf <- rf.bi(dtrain = dtrain, dtest = dtest, box = box)
#' res <- best.interval(dtrain = dtrain, dtest = dtest, box = box, depth = 5)
#' res.rf.c <- rf.bi(dtrain = dtrain, dtest = dtest, box = box, depth = "cv")


rf.bi <- function(dtrain, dtest = NULL, box, depth = "all", beam.size = 1,
                    keep = 10, denom = 6, npts = 10000, labels = FALSE){

  nc <- ncol(dtrain[[1]])

  if(depth == "cv"){
    depth = -(seq(-nc, -1, by = ceiling(nc/denom)))
  }

  if(length(depth) > 1){
    depth <- select.depth(dtrain = dtrain, box = box, depth = depth,
                          beam.size = beam.size, keep = keep)
  }

  if(depth == "all"){
    depth <- nc
  }

  dp <- list()
  dp[[1]] <- lhs::randomLHS(npts, nc)
    for(i in 1:nc){
    d.width <- box[2, i] - box[1, i]
    dp[[1]][, i] <- dp[[1]][, i]*d.width + box[1, i]
  }

  colnames(dtrain[[1]]) <- colnames(dtest[[1]]) <- colnames(dp[[1]]) <- paste0("x", paste0(1:nc))
  res.rf <- caret::train(as.data.frame(dtrain[[1]]), as.factor(dtrain[[2]]), method = "rf")
  print("finished with training RF")

  if(labels){
    dp[[2]] <- as.numeric(as.character(predict(res.rf, dp[[1]])))
  } else {
    dp[[2]] <- predict(res.rf, dp[[1]], type = "prob")[, 2]
  }

  temp <- best.interval(dtrain = dp, dtest = dtest, box = box, depth = depth,
                        beam.size = beam.size, keep = keep)

  return(list(qtest = temp$qtest, qtrain = temp$qtrain, box = temp$box,
              depth = temp$depth, tune.par = res.rf$bestTune))
}


