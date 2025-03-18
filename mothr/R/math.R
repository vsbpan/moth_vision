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



LOOKL <- function(x, cores = 1, ...){
  x <- do.call("rbind", x)
  pb_par_lapply(
    seq_len(nrow(x)), 
    FUN = function(i, w){
      x1 <- matrixStats::colSums2(w[-i,,drop = FALSE])
      x2 <- w[i,,drop = FALSE]
      mothr::KL(x1 / sum(x1), x2 / sum(x2))
    }, cores = cores, ..., w = x
  ) %>% 
    do.call("c", .)
}