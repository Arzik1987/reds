#' REDS with BestInterval
#'
#' The function learns a metamodel on a given dataset. Then it generates new points
#' and labels them. These new points serve as input for BestInterval algorithm.
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}. If NULL, the
#' WRAcc on test data is not computed
#' @param box matrix of real. Initial hyperbox, covering data
#' @param depth integer, vector of integers, "cv" or "all"; parameter specifying
#' the search depth (the number of restricted attributes). If "all", the full data
#' dimensionality is used; if a vector, the value is selected with
#' \code{\link{select.depth}} algorithm; if "cv" a vector of maximum \code{denom}
#' equidistant values is created and a single value is selected with
#' \code{\link{select.depth}} algorithm with \code{dtrain}
#' @param beam.size integer.The size of the beam in the beam search
#' @param keep integer. If greater than \code{beam.size}, specifies the maximum
#' number of boxes to be refined at each iteration in case all have equal WRAcc
#' @param denom the maximal length of the set of \code{depth} values to choose from
#' @param npts number of points to be generated and labeled
#' @param labels boolean. if "TRUE", the points are labeled with 0 or 1,
#' otherwise - with probabilities
#' @param seed seed for reproducibility of hyperparameter optimization procedure.
#' Default is 2020. Set NULL for not using
#' @param distr method for sampling the new \code{npts} points. "laths" means
#' Latin hypercube sampling; "logitnorm" - sampling from logitnormal distribution;
#' "discr" - Latin hypercube sampling with subsequent equal-width discretization of even inputs
#' @param nval integer. The number of bins for discretization if \code{distr} = "discr"
#' @param meth metamodel used in REDS. Can be "rf", "svmRadial" or "xgbTree"
#'
#' @keywords models, multivariate
#'
#' @return list.
#' \itemize{
#' \item \code{qtest} WRAcc measure of the found subgroup evaluated on \code{dtest}
#' \item \code{qtrain} WRAcc measure of the found subgroup evaluated on \code{dtrain}
#' \item \code{box} the hyperbox, with highest value of WRAcc on \code{dtrain}
#' \item \code{depth} integer; the value of \code{depth} parameter used
#' \item \code{tune.par} the best hyperparameter value(s) for random forest, produced with
#' the default settings of function \code{train} from the 'caret' package.
#' \item \code{time.train} time to train REDS with BestInterval
#' }
#'
#' @importFrom stats rbinom
#'
#' @seealso \code{\link{best.interval}},
#' \code{\link{reds.prim}}
#'
#' @export
#'
#' @examples
#'
#' dtrain <- dtest <- list()
#' dtest[[1]] <- dsgc_sym[1:9600, 1:12]
#' dtest[[2]] <- dsgc_sym[1:9600, 13]
#' dtrain[[1]] <- dsgc_sym[9601:10000, 1:12]
#' dtrain[[2]] <- dsgc_sym[9601:10000, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' reds.bi(dtrain = dtrain, dtest = dtest, box = box, depth = "all",
#' npts = 1000, distr = "laths", meth = "rf")
#' reds.bi(dtrain = dtrain, dtest = dtest, box = box, depth = "cv",
#' npts = 1000, distr = "laths", meth = "rf")


reds.bi <- function(dtrain, dtest = NULL, box, depth = "all", beam.size = 1,
                    keep = 10, denom = 6, npts = 10000, labels = FALSE, seed = 2020,
                    distr, nval = 5, meth){

  time1 = Sys.time()

  nc <- ncol(dtrain[[1]])

  if(depth[1] == "cv"){
    depth = -(seq(-nc, -1, by = ceiling(nc/denom)))
  }

  if(length(depth) > 1){
    depth <- select.depth(dtrain = dtrain, box = box, depth = depth,
                          beam.size = beam.size, keep = keep, seed = seed)
  }

  set.seed(seed = seed)

  dp <- list()
  dp[[1]] <- get.data(box = box, n.points = npts, distr = distr, nval = nval)

  colnames(dtrain[[1]]) <- colnames(dp[[1]]) <- paste0("x", paste0(1:nc))
  if(!is.null(dtest)){
    colnames(dtest[[1]]) <- paste0("x", paste0(1:nc))
  }

  fitControl <- caret::trainControl(method = "cv", number = 10, allowParallel = FALSE)
  if(meth == "xgbTree"){
    res.rf <- caret::train(as.data.frame(dtrain[[1]]), as.factor(dtrain[[2]]),
                           method = "xgbTree", trControl = fitControl, nthread = 1)
  } else {
    res.rf <- caret::train(as.data.frame(dtrain[[1]]), as.factor(dtrain[[2]]),
                           method = meth, trControl = fitControl)
  }
  print("finished with training metamodel")

  if(labels){
    dp[[2]] <- as.numeric(as.character(predict(res.rf, dp[[1]])))
  } else {
    if(!(meth %in% c("rf", "xgbTree"))) stop("probabilities are not implemented for this metamodel type")
    dp[[2]] <- predict(res.rf, dp[[1]], type = "prob")[, 2]
  }

  time2 = Sys.time()

  temp <- best.interval(dtrain = dp, dtest = dtest, box = box, depth = depth,
                        beam.size = beam.size, keep = keep, denom = denom, seed = seed)
  time.train = temp$time.train + time2 - time1

  return(list(qtest = temp$qtest, qtrain = temp$qtrain, box = temp$box,
              depth = temp$depth, tune.par = res.rf$bestTune, time.train = time.train))
}


