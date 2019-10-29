#' RF -> PRIM
#'
#' The function learns RF model on a given dataset. Then it generates new points with latin hypercube sampling
#' and labels them. These new points serve as input for PRIM algorithm.
#' 
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}
#' @param box matrix of real. Initial hyperbox, covering data
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
#' dtest[[1]] <- dsgc_sym[1001:10000, 1:12]
#' dtest[[2]] <- dsgc_sym[1001:10000, 13]
#' dtrain[[1]] <- dsgc_sym[1:500, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:500, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#' 
#' set.seed(1)
#' res.rf <- rf.prim(dtrain = dtrain, dtest = dtest, box = box, grow.deep = TRUE)
#' res <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#' 
#' plot(res.rf[[1]], type = "l", xlim = c(0, 1), ylim = c(0.5, 1),
#' xlab = "recall", ylab = "precision")
#' lines(res.rf[[2]], col = "red")
#' lines(res[[1]], col = "blue")
#' legend("bottomleft", legend = c("rf prob test", "rf pred test", "norm test"),
#' col = c("black", "red", "blue"), lty = c(1, 1, 1))

rf.prim <- function(dtrain, dtest, box, npts = 100000, minpts = 20, grow.deep = FALSE){

  dim <- ncol(dtrain[[1]])
  dp <- list()
  dp[[1]] <- lhs::randomLHS(npts, dim)
    for(i in 1:dim){
    d.width <- box[2, i] - box[1, i]
    dp[[1]][, i] <- dp[[1]][, i]*d.width + box[1, i]
  }
  
  colnames(dtrain[[1]]) <- colnames(dtest[[1]]) <- colnames(dp[[1]]) <- paste0("x", paste0(1:dim))
  res.rf <- caret::train(as.data.frame(dtrain[[1]]), as.factor(dtrain[[2]]), method = "rf")
  print("finished with training RF")
  
  dp[[2]] <- predict(res.rf, dp[[1]], type = "prob")[, 2]
  if(grow.deep){
    dtrain = dp
  }
  temp <- norm.prim(dtrain = dp, dtest = dtest, deval = dtrain, box = box, minpts = minpts)
  pr.prob <- temp[[1]]
  boxes.prob <- temp[[3]]

  dp[[2]] <- predict(res.rf, dp[[1]])
  dp[[2]] <- as.numeric(as.character(dp[[2]]))
  if(grow.deep){
    dtrain = dp
  }
  temp <- norm.prim(dtrain = dp, dtest = dtest, deval = dtrain, box = box, minpts = minpts)
  pr.pred <- temp[[1]]
  boxes.pred <- temp[[3]]

  return(list(pr.prob = pr.prob, pr.pred = pr.pred, boxes.prob = boxes.prob, boxes.pred = boxes.pred,
              tune.par = res.rf$bestTune))
}


