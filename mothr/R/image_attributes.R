sample_typicality <- function(.df, weight, file_name, n = 500, root_path = "C:", pb = TRUE){
  if(pb){
    do_FUN <- pb_par_lapply
  } else {
    do_FUN <- lapply
  }
  fnms <- dplyr::slice_sample(.df, n = n, weight_by = {{weight}}, replace = TRUE) %>% 
    dplyr::select({{file_name}}) %>% 
    unlist(FALSE,FALSE)
  scrs <- mini_moth_path(fnms, root_path = root_path) %>%
    purrr::keep(file.exists) %>% 
    do_FUN(imagefluency::img_read) %>% 
    imagefluency::img_typicality() %>% 
    as.vector()
  res <- data.frame("a" = fnms, "scores" = scrs)
  colnames(res)[1] <- deparse(substitute(file_name))
  gc()
  return(res)
}

img_typicality2 <- function(.df, weight, file_name, nboot = 1, n = 500, cores = 1, root_path = "D:"){
  pb_par_lapply(seq_len(nboot), function(i, .df, weight, file_name, n, root_path){
    sample_typicality(.df, weight, file_name, n, root_path, pb = FALSE)
  }, 
  .df = .df, 
  weight = weight,
  file_name = file_name, 
  n = n, 
  root_path = root_path,
  cores = cores) %>% 
    do.call("bind_rows", .)
}



image_attribute <- function(x, attribute = c("complexity","contrast")){
  stopifnot(is.array(x))
  f <- function(a){
    switch(a, 
           "complexity" = imagefluency::img_complexity,
           "contrast" = imagefluency::img_contrast,
           "self_similarity" = imagefluency::img_self_similarity)
  }
  funs <- lapply(attribute, f)
  
  purrr::map(
    funs, function(foo){
      foo(x)
    }
  ) %>% 
    do.call("c", .) %>% 
    setNames(attribute)
}

LOOKL_nested <- function(x, cores = 1){
  make_avg_moth <- function(x){
    mat <- do.call("rbind", unlist(x, recursive = FALSE, FALSE))
    x1 <- matrixStats::colSums2(mat)
    x1 / sum(x1)
  }
  avg_dev <- function(x,avg_moth){
    lapply(x, function(xi){
      xi <- xi / sum(xi)
      log(mothr::KL(avg_moth, xi))
    }) %>% 
      do.call("c", .) %>% 
      mean()
  }
  
  pb_par_lapply(
    seq_along(x), 
    FUN = function(i,z, avg_dev, make_avg_moth){
      avg_dev(x[[i]],make_avg_moth(x[-1]))
    }, cores = cores, 
    z = x, 
    avg_dev = avg_dev, 
    make_avg_moth = make_avg_moth
  ) %>% 
    do.call("c", .)
}



