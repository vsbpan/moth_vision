threshold_black <- function(img, mask){
  res <- color_index(img, "Lx_b", max_px = FALSE, plot = FALSE)[[1]] %>% 
    imager::isoblur(1) %>% 
    imagerExtra::SPE(0.0001, s = 5, range = c(0,1)) %>% 
    immask(!mask, background = 1) %>%  
    imagerExtra::ThresholdFuzzy() %>% 
    imager::fill(3)
  !res
}

thing_black_area_calc <- function(x, FUN = max, things = c("forewing", "hindwing", "body"), 
                                  units = c("px", "mm"), cores = 1){
  vmisc::warnifnot(is.parsed_inference(x))
  
  FUN <- match.fun(FUN)
  units <- match.arg(units)
  
  
  assert_variable_in_df(x, c("inlist"))
  
  file_paths <- mini_moth_path(x$file_name)
  
  find_black_area <- function(img, l, things, group = TRUE, FUN = NULL){
    selected_things <- select_things(l, things)
    img_dim <- dim(img)
    moth_thresh <- threshold_black(img, as.pixset(selected_things, img_dim)) 
    moth_thresh <- imager::as.cimg(moth_thresh)
    res <- selected_things %>% 
      lapply(function(x){
        moth_thresh[!as.pixset(x, img_dim)] <- NA_real_
        sum(moth_thresh, na.rm = TRUE)
      }) %>% 
      do.call("c", .) %>% 
      data.frame("black_area" = .) %>% 
      keep_rowname("instance_id")
    
    if(group){
      res <- lapply(l, find_things) %>% 
        do.call("c", .) %>% 
        data.frame("thing" = .) %>%
        keep_rowname("instance_id") %>% 
        dplyr::right_join(res, by = "instance_id") %>% 
        group_by(thing) %>% 
        dplyr::summarise(
          black_area = FUN(black_area)
        )
    }
    return(res)
  }
  
  res <- vmisc::pb_par_lapply(seq_along(x$inlist),function(i, inl,paths, find_black_area, things, fun){
    img <- fast_load_image(paths[i])
    tidyr::spread(find_black_area(img, as_relative(inl[[i]]), things, FUN = fun), 
                  key = thing, 
                  value = black_area) %>% 
      dplyr::rename_all(function(x){paste0(x, "_area_black")})
    
  }, inl = x$inlist, 
  paths = file_paths, 
  find_black_area = find_black_area,
  things = things, 
  fun = FUN,
  cores = cores,
  inorder = TRUE)
  
  res <- dplyr::bind_rows(res)
  
  res <- res %>% 
    dplyr::mutate_all(function(x){
      ifelse(!is.finite(x), NA_real_, x)
    })
  
  if(units == "mm"){
    assert_variable_in_df(x, c("tick_size"))
    res <- res / x$tick_size^2
  }
  return(res)
}

thing_not_black_color <- function(x, FUN = max, things = c("forewing", "hindwing", "body"), cores = 1){
  vmisc::warnifnot(is.parsed_inference(x))
  
  assert_variable_in_df(x, c("inlist"))
  
  file_paths <- mini_moth_path(x$file_name)
  
  find_not_black_color <- function(img, l, things){
    selected_things <- select_things(l, things)
    img_dim <- dim(img)
    moth_thresh <- !threshold_black(img, as.pixset(selected_things, img_dim)) 
    
    res <- lapply(things, function(thingi){
      as.pixset(select_things(l, thingi), dim_xy = img_dim)
    }) %>% 
      lapply(function(x){
        if(is.null(x)){
          return(NA_character_)
        }
        moth_thresh[!as.pixset(x, img_dim)] <- FALSE
        r <- mean(imager::R(img)[moth_thresh])
        g <- mean(imager::G(img)[moth_thresh])
        b <- mean(imager::B(img)[moth_thresh])
        rgb(r,g,b)
      }) %>% 
      do.call("c", .) %>% 
      data.frame("hex" = ., row.names = things) %>% 
      keep_rowname("thing")
    
    return(res)
  }
  
  res <- vmisc::pb_par_lapply(seq_along(x$inlist),function(i, inl,paths, find_not_black_color, things){
    img <- fast_load_image(paths[i])
    tidyr::spread(find_not_black_color(img, as_relative(inl[[i]]), things), 
                  key = thing, 
                  value = hex) %>% 
      dplyr::rename_all(function(x){paste0(x, "_hex")})
    
  }, inl = x$inlist, 
  paths = file_paths, 
  find_not_black_color = find_not_black_color,
  things = things, 
  cores = cores,
  inorder = TRUE)
  
  res <- dplyr::bind_rows(res)

  return(res)
}


expand_hex <- function(x, name = NULL){
  if(is.null(name)){
    name <- deparse(substitute(x))
  }
  z <- grDevices::col2rgb(x)
  z2 <- grDevices::rgb2hsv(z)
  z <- t(z/255)
  z2 <- t(z2)
  colnames(z2) <- c("hue","saturation", "value")
  res <- dplyr::bind_cols(z, z2)
  colnames(res) <- paste(name,colnames(res),sep = "_") 
  res[is.na(x),] <- NA_real_
  res
}

