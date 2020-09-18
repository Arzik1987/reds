#' Optimization of depth parameter
#'
#' The function chooses one \code{depth} value out of a given set using cross validation
#' and WRAcc as a quality measure and \code{link{best.interval}} algorithm
#'
#' @param dtrain list, containing training data. The first element contains matrix/data frame of real attribute values.
#' the second element contains vector of labels 0/1.
#' @param box matrix of real. Initial hyperbox, covering data
#' @param depth a set of integers to choose from
#' @param beam.size integer.The size of the beam in the beam search
#' @param keep integer. If greater than \code{beam.size}, specifies the maximum
#' number of boxes to be refined at each iteration in case all have equal WRAcc
#' @param seed seed for reproducibility. Set NULL for not using
#'
#' @return the optimal value of the \code{depth} parameter from the set
#'
#' @seealso \code{\link{select.alpha}},
#' \code{\link{select.param}}
#'
#' @export
#'
#' @examples
#'
#' dtrain <- list()
#' dtrain[[1]] <- dsgc_sym[1:500, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:500, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#'
#' select.depth(dtrain = dtrain, box = box, depth = c(4, 8, 12),
#' beam.size = 1, keep = 10, seed = 2020)


select.depth <- function(dtrain, box, depth, beam.size, keep, seed){
  set.seed(seed = seed)

  a <- caret::createFolds(dtrain[[2]], k = 5, list = TRUE, returnTrain = FALSE)
  res <- matrix(rep(NA, length(a)*length(depth)), ncol = length(depth))
  for(j in 1:length(a)){
    for(i in 1:length(depth)){
      tetmp <- list(dtrain[[1]][a[[j]],], dtrain[[2]][a[[j]]])
      trtmp <- list(dtrain[[1]][-a[[j]],], dtrain[[2]][-a[[j]]])
      res[j,i] <- best.interval(dtrain = trtmp, dtest = tetmp,
                                box = box, beam.size = beam.size,
                                keep = keep,
                                depth = depth[i])$qtest
    }
  }
  res <- apply(res, 2, mean, na.rm  = TRUE)
  depth[which(res == max(res, na.rm = TRUE))[1]]
}
