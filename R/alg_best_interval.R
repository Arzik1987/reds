#' BestInterval subgroup discovery algorithm
#'
#' The function applies BestInterval to train data and evaluates its quality on test data
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
#' \code{\link{select.depth}} algorithm.
#' @param beam.size integer.The size of the beam in the beam search
#' @param keep integer. If greater than \code{beam.size}, specifies the maximum
#' number of boxes to be refined at each iteration in case all have equal WRAcc
#' @param denom the maximal length of the set of \code{depth} values to choose from
#' @param seed seed for reproducibility of hyperparameter optimization procedure.
#' Default is 2020. Set NULL for not using
#'
#' @keywords models, multivariate
#'
#' @references Mampaey, Michael, et al. "Efficient algorithms for finding richer subgroup descriptions
#' in numeric and nominal data." 2012 IEEE 12th International Conference on Data Mining. IEEE, 2012.
#'
#' @return list.
#' \itemize{
#' \item \code{qtest} WRAcc measure of the found subgroup evaluated on \code{dtest}
#' \item \code{qtrain} WRAcc measure of the found subgroup evaluated on \code{dtrain}
#' \item \code{box} the hyperbox, with highest value of WRAcc on \code{dtrain}
#' \item \code{depth} integer; the value of \code{depth} parameter used
#' }
#'
#' @seealso \code{\link{reds.bi}}
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
#' best.interval(dtrain = dtrain, dtest = dtest, box = box, depth = "all")
#' best.interval(dtrain = dtrain, dtest = dtest, box = box, depth = "cv", denom = 2)
#'
#'
#' dx <- bi_test[, 1:4]
#' dy <- bi_test[, 5]
#' box = matrix(c(0,0,0,0,1,2,1,1), nrow = 2, byrow = TRUE)
#'
#' # clear difference in the fourth dimension
#' best.interval(list(dx, dy), box = box, depth = 3, beam.size = 1)$box
#' best.interval(list(dx, dy), box = box, depth = 3, beam.size = 4)$box
#' best.interval(list(dx, dy), box = box, depth = 4, beam.size = 1)$box


best.interval <- function(dtrain, dtest = NULL, box, depth = "all", beam.size = 1,
                          keep = 10, denom = 6, seed = 2020){

  time1 <- Sys.time()

  nc <- ncol(dtrain[[1]])

  if(depth[1] == "cv"){
    depth = -(seq(-nc, -1, by = ceiling(nc/denom)))
  }

  if(length(depth) > 1){
    depth <- select.depth(dtrain = dtrain, box = box, depth = depth,
                          beam.size = beam.size, keep = keep, seed = seed)
  }

  if(depth == "all"){
    depth <- nc
  }

  if(depth > nc){
    warning("Restricting depth parameter to the number of atributes in data!")
    depth <- nc
  }

  if((min(dtrain[[2]]) < 0) | (max(dtrain[[2]]) > 1)){
    warning("The target variable takes values from outside [0,1]. Just making sure you are aware of it")
  }

  # local function to assess WRAcc quality metric
  wracc <- function(n, np, N, Np){
    (n/N)*(np/n - Np/N)
  }

  # refine a single dimension of the box
  # numbers below correspond to row numbers in Algorithm 3 in the reference
  refine <- function(dx, dy, box.tmp, ind, start.q){

    N <- length(dy)
    Np <- sum(dy)

    ind.in.box <- rep(TRUE, N)
    for(i in 1:ncol(dx)){
      if(!(i == ind)){
        ind.in.box <- ind.in.box & (dx[, i] >= box.tmp[1, i] & dx[, i] <= box.tmp[2, i])
      }
    }
    in.box <- cbind(dx[ind.in.box, ind], dy[ind.in.box])
    in.box <- in.box[order(in.box[, 1]),]

    t.m <- h.m <- -Inf                                      # 3-4
    l <- box.tmp[1, ind]                                        # 1
    r <- box.tmp[2, ind]                                        # 1
    n <- nrow(in.box)
    np <- sum(in.box[, 2])
    wracc.m = start.q                                       # 2

    t <- sort(unique(in.box[, 1]))                          # define T
    itert <- length(t)
    for(i in 1:itert){                                      # 5
      if(i != 1){
        tmp <- in.box[in.box[, 1] == t[i - 1],, drop = FALSE]
        n <- n - nrow(tmp)                                  # 6
        np <- np - sum(tmp[, 2])                            # 6
      }
      h <- wracc(n, np, N, Np)                              # 7
      if(h > h.m){                                          # 8
        h.m <- h                                            # 9
        t.m <- ifelse(i == 1, box[1,ind], (t[i] + t[i - 1])/2)    # 10
      }
      tmp <- in.box[in.box[, 1] >= t.m & in.box[, 1] <= t[i],, drop = FALSE]
      n.i <- nrow(tmp)
      np.i <- sum(tmp[, 2])
      wracc.i <- wracc(n.i, np.i, N, Np)
      if(wracc.i > wracc.m){                                # 11
        l <- t.m                                            # 12
        r <- ifelse(i == itert, box[2,ind], (t[i] + t[i + 1])/2)  # 12
        wracc.m <- wracc.i                                  # 13
      }
    }

    box.tmp[, ind] <- c(l, r)
    list(box.tmp, wracc.m, ifelse(wracc.m == start.q, 0, 1))    # the last value 0 indicates that the box is a dead end
  }

  get.dup.boxes <- function(boxes){
    inds <- numeric()
    for(i in 1:(length(boxes) - 1)){
      for(j in (i + 1):length(boxes)){
        if(sum(abs(boxes[[i]] - boxes[[j]])) == 0){
          inds <- c(inds, j)
        }
      }
    }
    unique(inds)
  }

  #### end functions ####


  dims <- 1:ncol(dtrain[[1]])
  res.box <- list()
  res.tab <- as.data.frame(matrix(ncol = 3, nrow = 0))

  for(i in 1:ncol(box)){
    tmp <- refine(dtrain[[1]], dtrain[[2]], box, i, 0)
    res.box <- c(res.box, list(tmp[[1]]))
    res.tab <- rbind(res.tab, c(tmp[[2]], tmp[[3]], i))
  }

  if(depth > 1){
    add.iter = depth + 50
    while(add.iter > 0){
      add.iter <- add.iter - 1
    # start external for loop
      if(nrow(res.tab) > beam.size){
        retain <- which(res.tab[, 1] >= sort(res.tab[, 1], decreasing = TRUE)[beam.size])
        res.tab <- res.tab[retain, ]
        res.box <- res.box[retain]
        if(length(res.box) > beam.size){
          inds <- get.dup.boxes(res.box)
          if(length(inds) > 0){
            res.tab <- res.tab[-inds, ] # maybe sort here?
            res.box <- res.box[-inds]
          }
        }
        res.tab <- res.tab[1:min(length(res.box), max(keep, beam.size)), ]
        res.box <- res.box[1:min(length(res.box), max(keep, beam.size))]
      }
      if(sum(res.tab[, 2]) == 0) add.iter <- 0                            # if there is nothing to refine, exit the cycle
      for(k in 1:nrow(res.tab)){
        if(res.tab[k, 2] == 1){
          res.tab[k, 2] <- 0
          inds.r <- dims[apply(abs(box - res.box[[k]]), 2, sum) != 0]
          if(length(inds.r) < depth) inds.r <- dims
          inds.r <- inds.r[!(inds.r %in% res.tab[k, 3])]
          for(i in inds.r){
            tmp <- refine(dtrain[[1]], dtrain[[2]], res.box[[k]], i, res.tab[k, 1])
            if(tmp[[3]] == 1){
              res.box <- c(res.box, list(tmp[[1]]))
              res.tab <- rbind(res.tab, c(tmp[[2]], tmp[[3]], i))
            }
          }
        }
      }
    # end external for loop
    }
  }

  winner <- which(res.tab[, 1] == max(res.tab[, 1]))[1]
  box <- res.box[[winner]]

  time.train <- Sys.time() - time1

  qtest <- NULL
  if(!is.null(dtest)){
    qtest <- qual.wracc(dtest, box)
  }

  return(list(qtest = qtest, qtrain = res.tab[winner, 1], box = box, depth = depth, time.train = time.train))
}

