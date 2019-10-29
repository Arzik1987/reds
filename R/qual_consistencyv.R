#' Volume consistency
#'
#' The function calculates volume consistency for two N-dimensional hyperboxes
#' 
#' @param box1 real. Matrix defining the first box to compare
#' @param box2 real. Matrix defining the second box to compare
#' @param box.init real. Matrix describing the initial box containing all data
#' 
#' @return a number, quantidying linear consistency between boxes
#' 
#' @seealso \code{\link{consistency.d}},
#' \code{\link{consistency.l}}
#' 
#' @export
#' 
#' @examples
#' 
#' dtrain <- dtest <- list()
#' dtest[[1]] <- dsgc_sym[5001:10000, 1:12]
#' dtest[[2]] <- dsgc_sym[5001:10000, 13]
#' dtrain[[1]] <- dsgc_sym[1:5000, 1:12]
#' dtrain[[2]] <- dsgc_sym[1:5000, 13]
#' box <- matrix(c(0.5,0.5,0.5,0.5,1,1,1,1,0.05,0.05,0.05,0.05,
#' 5,5,5,5,4,4,4,4,1,1,1,1), nrow = 2, byrow = TRUE)
#' 
#' set.seed(1)
#' res1 <- norm.prim(dtrain = dtrain, dtest = dtest, box = box)
#' res2 <- norm.prim(dtrain = dtest, dtest = dtrain, box = box)
#' box1 <- res1[[3]][[length(res1[[3]])]]
#' box2 <- res2[[3]][[length(res2[[3]])]]
#' consistency.v(box1, box2, box)

consistency.v <- function(box1, box2, box.init){
  box1[1, ] <- apply(rbind(box1[1, ], box.init[1, ]), 2, max)
  box2[1, ] <- apply(rbind(box2[1, ], box.init[1, ]), 2, max)
  box1[2, ] <- apply(rbind(box1[2, ], box.init[2, ]), 2, min)
  box2[2, ] <- apply(rbind(box2[2, ], box.init[2, ]), 2, min)
  
  inter <- box.init
  inter[1, ] <- apply(rbind(box1[1, ], box2[1, ]), 2, max)
  inter[2, ] <- apply(rbind(box1[2, ], box2[2, ]), 2, min)

  sides1 <- box1[2, ] - box1[1, ]
  sides2 <- box2[2, ] - box2[1, ]
  sides.inter <- inter[2, ] - inter[1, ]

  if(sum(sides.inter<=0) > 0){
    cons = 0
  } else {
    cons = 1/(prod(sides1/sides.inter) + prod(sides2/sides.inter) - 1)
  }
  cons
}
