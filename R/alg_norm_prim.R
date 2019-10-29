#' PRIM returning peeling trajectory
#'
#' The function applies PRIM to train data and evaluates its quality on test data
#' 
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param dtest list, containing test data. Structured in the same way as \code{dtrain}
#' @param deval list, containing data for evaluation. Structured in the same way as \code{dtrain}.
#' By default coincides with \code{dtrain}
#' @param box matrix of real. Initial hyperbox, covering data
#' @param minpts integer. Minimal number of points in the box for PRIM to continue peeling
#' @param max.peels integer. Maximum length of the peeling trajectory (number of boxes)
#' @param pasting logical. Whether pasting is used on each box forming the peeling rajectory
#' 
#' @keywords models, multivariate
#' 
#' @references Friedman, J.H. and Fisher, N.I. 1999. Bump hunting in high-dimensional data. 
#' Statistics and Computing. 9, 2 (1999), 123-143.
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
#' @seealso \code{\link{rf.prim}},
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
#' res1 <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#' res2 <- norm.prim(dtrain = dtrain, dtest = dtest, box = box, pasting = TRUE)
#'
#' plot(res1[[1]], col = "green", type = "l")
#' lines(res2[[1]], col = "blue")

norm.prim <- function(dtrain, dtest, deval = dtrain, box, minpts = 20, max.peels = 99,
                      pasting = FALSE){
  
  if(pasting){
    x.init <- dtrain[[1]]
    y.init <- dtrain[[2]]
  } else {
    x.init <- NULL
    y.init <- NULL
  }
  boxes <- list()
  boxes[[1]] <- box
  
  pr.eval <- get.metrics(deval[[2]], deval[[2]])
  pr.test <-  get.metrics(dtest[[2]], dtest[[2]])

  res <- prim.single(dtrain[[1]], dtrain[[2]], box, pasting = pasting, x.init = x.init, y.init = y.init)
  ind.in.box <- prim:::in.box(deval[[1]], res[[3]], d = ncol(res[[3]]), boolean = TRUE)
  i <- 0
  
  #### with 'sum(ind.in.box) >= minpts' we allow that the box after peeling contains less
  #### than 'minpts' points in case pasting == TRUE. This does not have huge consequences 
  #### for our experiments, but allows shorter code
  
  while(length(res[[2]]$y) >= minpts & sum(ind.in.box) >= minpts & res[[1]] == FALSE & i < max.peels){
    i <- i + 1
    
    pred <- deval[[2]][ind.in.box]
    pr.eval <- rbind(pr.eval, get.metrics(pred, deval[[2]]))
    
    ind.in.box <- prim:::in.box(dtest[[1]], res[[3]], d = ncol(res[[3]]), boolean = TRUE)
    pred <- dtest[[2]][ind.in.box]
    pr.test <- rbind(pr.test, get.metrics(pred, dtest[[2]]))
    
    boxes <- append(boxes, list(res[[3]]))
    
    num.before <- length(res[[2]]$y)
    res <- prim.single(res[[2]]$x, res[[2]]$y, res[[2]]$box,
                           pasting = pasting, x.init = x.init, y.init = y.init)
    ind.in.box <- prim:::in.box(deval[[1]], res[[3]], d = ncol(res[[3]]), boolean = TRUE)
    
    if(num.before == length(res[[2]]$y) & res[[1]] == FALSE){
      warning("can't peel any direction")
      res[[1]] = TRUE
    }
  }
  
  #### cut the trajectory at its first maximum
  
  last <- min(which(pr.eval[, 2] == max(pr.eval[, 2])))
  pr.test = pr.test[1:last,]
  pr.eval = pr.eval[1:last,]
  
  #### make sure, that pr.x are still matrices

  if(last == 1){
    pr.test <- matrix(pr.test, ncol = 2, byrow = TRUE)
    pr.eval <- matrix(pr.eval, ncol = 2, byrow = TRUE)
  }
  
  return(list(pr.test = pr.test, pr.eval = pr.eval, boxes = boxes[1:last]))
}


