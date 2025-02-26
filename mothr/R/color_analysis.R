
count_rgb_bin <- function(x, by = 0.2, range = c(0, 1)){
  res <- apply(x, 4, function(z){
    as.character(vmisc::bin(na.omit(c(z)), by = by, range = range, value = FALSE))
  }, simplify = TRUE) %>% 
    apply(1, function(z){
      paste0(z, collapse = "-")
    }) %>% 
    table()
  list(
    "bin" = names(res),
    "count" = as.numeric(res)
  )
}


collect_color_bins <- function(x, by = 0.2){
  make_color_grid <- function(by = 0.2){
    n <- floor(1/by) + 1
    
    expand.grid(
      "r" = 1:n,
      "g" = 1:n,
      "b" = 1:n
    ) %>% 
      rowwise() %>% 
      transmute(bin = paste0(c(r,g,b), collapse = "-")) %>% 
      unlist(FALSE, FALSE)
  }
  
  format_color_bins <- function(x, grid = make_color_grid()){
    v <- rep(0, length(grid))
    grid <- sort(grid)
    names(v) <- grid
    v[grid %in% x$bin] <- x$count[order(x$bin)]
    list(
      "meta" = do.call("c",x[c("image_id", "instance_id", "thing_class")]),
      "count" = v
    )
  }
  
  
  l2 <- x %>% 
    unlist(FALSE, FALSE) %>% 
    pb_par_lapply(function(z, grid){
      format_color_bins(z, grid)
    }, grid = make_color_grid(by = by), cores = 1)
  
  meta_d <- bind_vec(map(l2,"meta"))
  count_d <- bind_vec(map(l2,"count"))
  
  cbind(meta_d, count_d)
}

split_color_bin <- function(x){
  res <- do.call("rbind", lapply(str_split(x, "-"), as.numeric))
  colnames(res) <- c("r", "g", "b")
  as.data.frame(res)
}






