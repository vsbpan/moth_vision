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