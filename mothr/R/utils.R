
pretty_obj_size <- function(x){
  prettyunits::pretty_bytes(utils::object.size(x))
}

clean_taxon_name <- function(x){
  gsub("(?![-])[[:punct:]]","",x, perl = TRUE)
}


launch_photo <- function(path){
  shell(sprintf("Open %s", path))
}

drop_attributes <- function(x, exclude = NULL){
  if(is.null(exclude)){
    attributes(x) <- NULL
  } else {
    attributes(x) <- attributes(x)[exclude]
  }
  return(x)
}

as_named_vector <- function (x, id_col = 1, val_col = 2) {
  z <- x[, val_col, drop = TRUE]
  names(z) <- x[, id_col, drop = TRUE]
  return(z)
}

dist2 <- function(l, FUN, is_symmetric = TRUE, cores = 1){
  FUN <- match.fun(FUN)
  n <- length(l)
  
  
  if(is_symmetric){
    grid <- expand.grid("a" = seq_along(l), "b" = seq_along(l)) %>% 
      dplyr::mutate(
        key = paste(pmin(a, b), pmax(a, b), sep = "-"),
        dup = duplicated(key) | a == b,
        i = cumsum(!dup)
      ) %>% 
      as.data.frame()
    
    n2 <- choose(n,2)
    res <- vmisc::pb_par_lapply(seq_len(nrow(grid)), function(i, grid){
      if(grid[i, "dup"]){
        return(NA_real_)
      } else {
        FUN(
          l[[grid[i,1]]],l[[grid[i,2]]]
        )
      }
    }, grid = grid, cores = cores) %>% 
      unlist() %>% 
      matrix(nrow = n, 
             ncol = n, 
             dimnames = list(
               names(l),
               names(l)
             ))
    
    res <- as.matrix(as.dist(res))
  } else {
    grid <- expand.grid(seq_along(l), seq_along(l))
    n2 <- n^2
    res <- lapply(seq_len(nrow(grid)), function(i, grid){
      FUN(
        l[[grid[i,1]]],l[[grid[i,2]]]
      )
    }, grid = grid, cores = cores) %>% 
      unlist() %>% 
      matrix(nrow = n, 
             ncol = n, 
             dimnames = list(
               names(l),
               names(l)
             ))
  }
  
  res <- as.dist(res, diag = TRUE, upper = TRUE)
  return(res)
}
