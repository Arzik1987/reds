#' interpretability of PRIM result
#'
#' The function calculates the quantity opposite to interpretability,
#' as the number of dimensions restricted. 
#' Higher value stands for lower interpretability
#' 
#' @param box.init real. Matrix describing the initial box containing all data
#' @param res.box box to evaluate
#' 
#' @references Bryant, B.P. and Lempert, R.J. 2010. Thinking inside the box: 
#' A participatory, computer-assisted approach to scenario discovery. 
#' Technological Forecasting and Social Change. 77, 1 (2010), 34-49.
#' 
#' @return number of restricted dimensions in res.box as compared to box
#' 
#' @export
#' 
#' @examples
#' 
#' box <- matrix(c(rep(0, 5), rep(1, 5)), ncol = 5, byrow = TRUE)
#' res.box <- matrix(c(0.40, 0.49, 0, 0.15, 0, 1, 1, 1, 0.93, 1), ncol = 5, byrow = TRUE)
#' box
#' res.box
#' qual.interpretability(res.box, box)


qual.interpretability <- function(res.box, box.init){
  
  res.box[1,] <- apply(rbind(res.box[1,], box.init[1,]), 2, max)
  res.box[2,] <- apply(rbind(res.box[2,], box.init[2,]), 2, min)
  
  bx <- !(box.init == res.box)
  res <- sum(apply(bx, 2, max))
  res
}




