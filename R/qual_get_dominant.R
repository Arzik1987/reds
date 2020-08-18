#' dominating boxes
#'
#' The function calculates pareto-front of multiple peeling trajectories
#' @param pr0 matrix or data frame with recall values in the first column and precision
#' values in the second
#' @keywords pareto front
#' @return index of dominant points
#' @export
#' @examples
#' set.seed(1)
#' pr0 <- matrix(c(1,2,3,4,5,6,3,4,6,5), ncol = 2, byrow = 2)
#' pr <- matrix(sample(1:20, nrow(pr0)*2), ncol = 2)
#' pr
#' pr0
#' get.dominant(pr)
#' get.dominant(pr0)

get.dominant <- function(pr0){
  row.names(pr0) <- 1:nrow(pr0)
  pr0 <- unique(pr0)
  
  if(nrow(pr0) == 1) return(1)
  
  pr0 <- cbind(pr0, as.numeric(row.names(pr0)))
  ind0 <- numeric()
  for(i in 1:nrow(pr0)){
    a <- pr0[pr0[, 1] >= pr0[i, 1], 2]
    a <- a[a >= pr0[i, 2]]
    if(length(a) == 1){
      ind0 <- c(ind0, i)
    }
  }
  ind0 <- unique(c(1, ind0))
  pr0 <- pr0[ind0, ]
  if(length(ind0) == 1){
    ind <- pr0[3]
  } else {
    pr0 <- pr0[order(pr0[, 1], decreasing = TRUE),]
    ind <- pr0[, 3]
  }
  
  return(ind)
}
