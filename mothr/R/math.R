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

LOOKL <- function(x, n = NULL, cores = 1, ...){
  x <- do.call("rbind", x)
  if(is.null(n)){
    f <- function(w,i, n){
      w[-i,,drop = FALSE]
    }
  } else {
    f <- function(w,i, n){
      indices <- seq_len(nrow(w))
      w[sample(indices[-i], size = n, replace = TRUE),,drop = FALSE]
    }
  }
  pb_par_lapply(
    seq_len(nrow(x)), 
    FUN = function(i, w, f, n){
      x1 <- matrixStats::colSums2(f(w,i, n))
      x2 <- w[i,,drop = FALSE]
      mothr::KL(x1 / sum(x1), x2 / sum(x2))
    }, cores = cores, ..., 
    w = x, 
    f = f, 
    n = n
  ) %>% 
    do.call("c", .)
}

