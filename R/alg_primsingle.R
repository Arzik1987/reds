#' One cut PRIM
#'
#' The function makes one cut following the PRIM algorithm, if the threshold is not met.
#' 
#' @param x matrix of real. Data values for examples within the box
#' @param y vector of response values 0/1 for examples within the box
#' @param box matrix of real. Initial covering box
#' @param peel.alpha real. Peeling quantile tuning parameter
#' @param paste.alpha real. Pasting quantile tuning parameter
#' @param threshold real. Threshold tuning parameter. If average value of \code{y} exceeds it,
#' the box is not modyfied anymore.
#' @param pasting logical. If true pasting step is performed
#' @param x.init matrix of real. Data values for all examples
#' @param y.init vector of response values 0/1 for all examples
#' 
#' @return list.
#' \itemize{
#' \item \code{meet.threshold} logical. If the threshold is reached
#' \item \code{res} a list of 3. Contains a new box obtained after one cut (if made),
#' \code{x} elements falling withinin it and resective vector of labels, \code{y}
#' \item \code{box.p} box after pasting step. If pasting is false, coincides with \code{res$box}
#' }
#' 
#' @keywords models, multivariate
#' 
#' @export
#' 
#' @examples
#' 
#' box <- matrix(c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.05, 0.05, 0.05, 0.05, 
#' 5, 5, 5, 5, 4, 4, 4, 4, 1, 1, 1, 1), nrow = 2, byrow = TRUE)
#' x <- dsgc_sym[1:99, 1:12]
#' y <- dsgc_sym[1:99, 13]
#' res <- prim.single(x, y, box, pasting = TRUE, x.init = x, y.init = y)
#' res[[2]]$box
#' 
#' set.seed(1)
#' box <- matrix(c(0, 0, 1, 1), nrow = 2, byrow = TRUE)
#' x <- cbind(runif(1000), runif(1000))
#' y <- ifelse(x[, 1] > 0.98, 0, 1)
#' res <- prim.single(x, y, box, pasting = TRUE, x.init = x, y.init = y)
#' res[[2]]$box
#' res[[3]]


prim.single <- function(x, y, box, peel.alpha = 0.05, threshold = 1,
                            pasting = FALSE, paste.alpha = 0.01, x.init, y.init){

  d <- ncol(box)
  y.fun.val <- mean(y)
  
  if(y.fun.val >= threshold){
    res <- list(box = box, x = x, y = y)
  } else {
    
    #### Peeling step
    
    res <- prim:::peel.one(x = x, y = y, box = box, peel.alpha = peel.alpha, 
                           mass.min = 0, threshold = 0, d = d, n = 1, type = 8, 
                           y.fun = mean, verbose = FALSE)
    if(is.null(res)){
      res <- list(box = box, x = x, y = y)
    } else {
      res <- list(box = res$box, x = res$x, y = res$y)
    }
    
    #### End of peeling
    
    box.p <- res$box
  
    #### Pasting step
    
    if(pasting){
      res.paste <- res
      while (!is.null(res.paste)){
        res.p <- res.paste
        # this function could be more efficient. I do not see why does it need x or y parameters
        res.paste <- prim:::paste.one(x = res.p$x, y = res.p$y, box = res.p$box,
                                      x.init = x.init, y.init = y.init, paste.alpha = paste.alpha,
                                      mass.min = 0, threshold = 0, d = d, n = 1, 
                                      y.fun = mean, verbose = FALSE)      
      }
      box.p <- res.p$box
    }
    
    #### End of pasting
    
  }
  
  y.fun.val <- mean(res$y)
  return(list(meet.threshold = (y.fun.val >= threshold), res = res, box.p = box.p))
}







