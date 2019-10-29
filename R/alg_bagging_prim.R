#' PRIM with bagging (bumping)
#'
#' The function implements PRIM with bagging (bumping)
#' 
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}
#' @param box hyperbox, covering data
#' @param features character or integer. Refers to the number of features used in each iteration.
#' If integer, defines this number.
#' If "some", calculated as \code{ceiling(sqrt(ncol(dtrain[[1]])))},
#' if "all", all features are used. Default is "some"
#' @param iter integer. The number of iterations (sequences of boxes learned). Default is 50
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' 
#' @keywords models, multivariate
#' 
#' @export
#' 
#' @references J. H. Kwakkel and S. C. Cunningham, "Improving scenario discovery by bagging random boxes," 
#' Technological Forecasting and Social Change, vol. 111, pp. 124-134, 2016.
#' 
#' @return list.
#' \itemize{
#' \item \code{pr.test} matrix with coverage (recall) in the first column and 
#' density (precision) in the second column, evaluated on \code{dtest}
#' \item \code{pr.train} matrix with coverage (recall) in the first column and 
#' density (precision) in the second column, evaluated on \code{deval}
#' \item \code{boxes} list of matrices defining boxes constituting peeling trajectory
#' }
#' 
#' @keywords models, multivariate
#' 
#' @seealso \code{\link{norm.prim}},
#' \code{\link{rf.prim}}
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
#' res <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#' set.seed(1)
#' res.bag <- bagging.prim(dtrain = dtrain, dtest = dtest, box = box)
#' res.bag.all <- bagging.prim(dtrain = dtrain, dtest = dtest, box = box, features = 12)
#'
#' plot(res[[1]], type = "l", xlim = c(0, 1), ylim = c(0.5, 1),
#' xlab = "recall", ylab = "precision")
#' lines(res[[2]], col = "red")
#' lines(res.bag[[1]], lty = 2)
#' lines(res.bag[[2]], lty = 2, col = "red")
#' lines(res.bag.all[[1]], lty = 3)
#' lines(res.bag.all[[2]], lty = 3, col = "red")
#' legend("bottomleft", legend = c("norm test", "norm train", "bagging test", 
#' "bagging train", "bagging all test", "bagging all train"),
#' lty = c(1, 1, 2, 2, 3, 3), col = c("black", "red", "black", "red", "black", "red"))
#' 
bagging.prim <- function(dtrain, dtest, box, features = "some", iter = 50,  minpts = 20){

  N <- length(dtrain[[2]])
  nc <- ncol(dtrain[[1]])
  
  if(is.na(features) || is.null(features)){
    stop("wrong value for parameter features")
  } else if(features == "some"){
    features <- ceiling(sqrt(nc))
  } else if (features == "all"){
    features <- nc
  } else if (is.na(as.numeric(features))){
    stop("wrong value for parameter features")
  } else {
    features <- min(nc, round(as.numeric(features)))
    features <- max(2, features)
  }
  
  d <- deval <- dtrain
  dt <- dtest

  for(i in 1:iter){
    ind <- sample(1:N, N, replace = TRUE)
    indc <- sample(1:nc, features, replace = FALSE)
    d[[1]] <- dtrain[[1]][ind, indc]
    d[[2]] <- dtrain[[2]][ind]
    dt[[1]] <- dtest[[1]][, indc]
    deval[[1]] <- dtrain[[1]][, indc]
    boxt <- box[, indc]

    temp <- norm.prim(dtrain = d, dtest = dt, deval = deval, box = boxt, minpts = minpts)
    if(i == 1){
      pr.test <- temp[[1]]
      pr.eval <- temp[[2]]
      boxes <- lapply(temp[[3]], function(x){a <- box; a[,indc] <- x; return(a)})
    } else {
      pr.test <- rbind(pr.test, temp[[1]])
      pr.eval <- rbind(pr.eval, temp[[2]])
      boxes <- append(boxes, lapply(temp[[3]], function(x){a <- box; a[,indc] <- x; return(a)}))
    }
  }

  ind <- get.dominant(pr.eval)
  pr.test = pr.test[ind,]
  pr.eval = pr.eval[ind,]
  
  if(length(ind) == 1){
    pr.test <- matrix(pr.test, ncol = 2, byrow = TRUE)
    pr.eval <- matrix(pr.eval, ncol = 2, byrow = TRUE)
  }
  
  res <- list(pr.test = pr.test, pr.eval = pr.eval, boxes = boxes[ind])
  return(res)
}
