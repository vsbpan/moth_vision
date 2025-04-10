mean_wt <- function(x, w){
  sum(w * x)/sum(w)
}

var_wt <- function(x, w){
  mu <- mean_wt(x, w)
  sum(w * (x - mu)^2) / sum(w)
}

entropy <- function(x){
  -sum(log(x) * x)
}

entropy_null <- function(x, dist, ...){
  r <- do.call(paste0("r", dist), list(n = length(x), ...))
  r <- r / sum(r)
  entropy(r)
}

KL <- function(x,y, test.na = TRUE, unit = "log", est.prob = NULL){
  suppressMessages(philentropy::KL(rbind(x,y), test.na = test.na, unit = unit, est.prob = est.prob))
}

# `n` controls the sample number
# `weight` weighs the average moth
LOOKL <- function(x, n = NULL, weight = NULL, cores = 1, ...){
  x <- do.call("rbind", x)
  if(is.null(n)){
    if(is.null(weight)){
      f <- function(mat,i, n, w){
        mat[-i,,drop = FALSE]
      }
    } else {
      f <- function(mat,i, n, w){
        w[i] <- w[i] - 1
        sweep(mat, 1, w, FUN = "*")
      }
    }
    
  } else {
    if(is.null(weight)){
      f <- function(mat,i, n, w){
        indices <- seq_len(nrow(mat))
        mat[sample(indices[-i], size = n, replace = TRUE),,drop = FALSE]
      }
    } else {
      f <- function(mat,i, n, w){
        indices <- seq_len(nrow(mat))
        ind2 <- sample(indices, size = n, replace = TRUE)
        w2 <- w[ind2]
        w2[ind2 == indices[i]] <- w2[ind2 == indices[i]] - 1
        sweep(mat[ind2, , drop = FALSE], 1, w2, FUN = "*")
      }
    }
  }
  pb_par_lapply(
    seq_len(nrow(x)), 
    FUN = function(i, mat, f, n, w){
      x1 <- matrixStats::colSums2(f(mat,i, n, w))
      x2 <- mat[i,,drop = FALSE]
      mothr::KL(x1 / sum(x1), x2 / sum(x2))
    }, cores = cores, ..., 
    mat = x, 
    f = f, 
    n = n,
    w = weight
  ) %>% 
    do.call("c", .)
}

