#' WRAcc measure of the box
#'
#' The function calculates the Weighted Relative Accuracy (WRAcc) of the box using a given data
#'
#' @param d list containing the data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param box matrix of real describing the box of interest
#'
#' @return WRAcc value
#'
#' @export
#'
#' @examples
#'
#' d <- list()
#' d[[1]] <- dsgc_sym[, 1:12]
#' d[[2]] <- dsgc_sym[, 13]
#' box <- matrix(c(1,1,1,1,1,1,1,1,0.05,0.05,0.05,0.05,
#' 4,4,4,4,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' qual.wracc(d, box)



qual.wracc <- function(d, box){
  Np = sum(d[[2]])
  N = length(d[[2]])
  retain <- rep(TRUE, length(d[[2]]))
  for(i in 1:ncol(d[[1]])){
    retain <- retain & d[[1]][, i] >= box[1, i]
    retain <- retain & d[[1]][, i] <= box[2, i]
  }
  n = length(d[[2]][retain])
  np = sum(d[[2]][retain])
  np/N - n*Np/(N^2)
}
