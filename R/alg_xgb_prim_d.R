#' xgbTree -> PRIM
#'
#' The function learns xgbTree model on a given dataset. Then it generates new points with latin hypercube sampling
#' and labels them. These new points serve as input for PRIM algorithm.
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}. If NULL, the
#' quality metrics on test data are not computed
#' @param deval list, containing data for evaluation. Structured in the same way as \code{dtrain}.
#' By default coincides with \code{dtrain}
#' @param box matrix of real. Initial hyperbox, covering data
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param max.peels integer. Maximum length of the peeling trajectory (number of boxes)
#' @param peel.alpha a set of real. The peeling parameter(s) of PRIM from the interval (0,1).
#' If a vector, the value is selected with \code{\link{select.alpha}} algorithm with \code{dtrain}
#' @param threshold real. If precision of the current box on the newly
#' generated dataset is greater or equal \code{threshold}, PRIM stops peeling
#' @param npts number of points to be generated and labeled
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param grow.deep logical. If true, criterion \code{minpts} is applied to the newly
#' generated dataset, not to \code{deval}
#' @param seed seed for reproducibility of hyperparameter optimization procedure.
#' Default is 2020. Set NULL for not using
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
#' \item \code{peel.alpha} integer; the value of \code{peel.alpha} parameter used
#' }
#'
#' @importFrom stats predict
#'
#' @seealso \code{\link{norm.prim}},
#' \code{\link{bumping.prim}}
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
#' res.xgb <- xgb.prim(dtrain = dtrain, dtest = dtest, box = box)
#' res <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#'
#' plot(res.xgb[[1]], type = "l", xlim = c(0, 1), ylim = c(0.5, 1),
#' xlab = "recall", ylab = "precision")
#' lines(res.xgb[[2]], col = "red")
#' lines(res[[1]], col = "blue")
#' legend("bottomleft", legend = c("xgb prob test", "xgb pred test", "norm test"),
#' col = c("black", "red", "blue"), lty = c(1, 1, 1))
#'
#' res.xgb <- xgb.prim(dtrain = dtrain, dtest = dtest, box = box,
#' peel.alpha = c(0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.2))
#' res <- norm.prim(dtrain = dtrain, dtest = dtest, box = box,
#' peel.alpha = c(0.03, 0.05, 0.07, 0.10, 0.13, 0.16, 0.2))
#'
#' plot(res.xgb[[1]], type = "l", xlim = c(0, 1), ylim = c(0.5, 1),
#' xlab = "recall", ylab = "precision")
#' lines(res.xgb[[2]], col = "red")
#' lines(res[[1]], col = "blue")
#' legend("bottomleft", legend = c("xgb prob test", "xgb pred test", "norm test"),
#' col = c("black", "red", "blue"), lty = c(1, 1, 1))


xgb.prim.d <- function(dtrain, dtest = NULL, deval = dtrain, box, minpts = 20, max.peels = 999,
                    peel.alpha = 0.05, threshold = 1, npts = 100000, grow.deep = FALSE, seed = 2020, dis = NULL){

  time1 = Sys.time()

  if(length(peel.alpha) > 1){
    peel.alpha <- select.alpha(dtrain = dtrain, box = box, minpts = minpts,
                               max.peels = max.peels, peel.alpha = peel.alpha,
                               threshold = threshold, seed = seed)
    print("finished with selecting peel.alpha")
    }

  set.seed(seed = seed)

  dim <- ncol(dtrain[[1]])
  dp <- list()
  dp[[1]] <- lhs::randomLHS(npts, dim)
    for(i in 1:dim){
    d.width <- box[2, i] - box[1, i]
    dp[[1]][, i] <- dp[[1]][, i]*d.width + box[1, i]
    }
  for(i in dis){
    dp[[1]][, i] <- make.discr(dp[[1]][, i], low = box[1, i], high = box[2, i], nval = 5)
  }

  colnames(dtrain[[1]]) <- colnames(dp[[1]]) <- paste0("x", paste0(1:dim))
  if(!is.null(dtest)){
    colnames(dtest[[1]]) <- paste0("x", paste0(1:dim))
  }

  fitControl <- caret::trainControl(method = "cv", number = 10, allowParallel = FALSE)

  res.xgb <- caret::train(as.data.frame(dtrain[[1]]), as.factor(dtrain[[2]]),
                          method = "xgbTree", trControl = fitControl, nthread = 1)
  print("finished with training xgbTree")

  time2 = Sys.time()

  dp[[2]] <- predict(res.xgb, dp[[1]], type = "prob")[, 2]
  if(grow.deep){
    deval = dp
  }

  time3 = Sys.time()

  temp <- norm.prim(dtrain = dp, dtest = dtest, deval = deval, box = box,
                    minpts = minpts, peel.alpha = peel.alpha)

  time.prob = time3 - time1 + temp$time.train

  pr.prob <- temp$pr.test
  boxes.prob <- temp$boxes

  time4 = Sys.time()

  dp[[2]] <- predict(res.xgb, dp[[1]])
  dp[[2]] <- as.numeric(as.character(dp[[2]]))
  if(grow.deep){
    dtrain = dp
  }

  time5 = Sys.time()

  temp <- norm.prim(dtrain = dp, dtest = dtest, deval = dtrain, box = box,
                    minpts = minpts, max.peels = max.peels, peel.alpha = peel.alpha,
                    threshold = threshold)

  time.pred = time5 - time4 + time2 - time1 + temp$time.train

  pr.pred <- temp$pr.test
  boxes.pred <- temp$boxes

  return(list(pr.prob = pr.prob, pr.pred = pr.pred, boxes.prob = boxes.prob, boxes.pred = boxes.pred,
              time.prob = time.prob, time.pred = time.pred,
              tune.par = res.xgb$bestTune, peel.alpha = peel.alpha))
}


