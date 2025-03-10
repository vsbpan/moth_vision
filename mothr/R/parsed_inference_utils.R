
merge_parsed_inference <- function(mask_df, kp_df){
  mask_df_name <- deparse(substitute(mask_df))
  kp_df_name <- deparse(substitute(kp_df))
  if(!is.parsed_inference(mask_df)){
    cli::cli_abort("{.var {mask_df_name}} must be of class {.cls parsed_inference}")
  }
  
  if(!is.parsed_inference(kp_df)){
    cli::cli_abort("{.var {kp_df_name}} must be of class {.cls parsed_inference}")
  }
  
  assert_variable_in_df(mask_df, c("inlist", "id"))
  assert_variable_in_df(kp_df, c("inlist", "id"))
  
  n_og <- nrow(mask_df)
  n_kp <- nrow(kp_df)
  matched <- match(kp_df$id, mask_df$id)
  matched <- matched[!is.na(matched)]
  n_new <- length(matched)
  
  cli::cli_alert_info("{n_new} of {n_og} images in {.var {mask_df_name}} found in {.var {kp_df_name}} of {n_kp} images.")
  mask_df <- mask_df[matched, ,drop = FALSE]
  
  
  mask_inl <- mask_df$inlist
  kp_inl <- kp_df$inlist
  
  
  if(!all(do.call("c",lapply(mask_inl, is.inlist)))){
    cli::cli_abort("{.var {mask_df_name}} must have a column for a list of {.cls inlist}")
  }
  
  if(!all(do.call("c",lapply(kp_inl, is.inlist)))){
    cli::cli_abort("{.var {kp_df_name}} must have a column for a list of {.cls inlist}")
  }
  
  mask_df$inlist <- purrr::map2(mask_inl, kp_inl, function(x,y){
    c(x,y)
  })
  
  # Assign new instance_id as there may be dups
  mask_df$inlist <- lapply(mask_df$inlist, function(x){
    instance_id <- gsub("img",
                        "inst", 
                        gsub_element_wise(
                          "00000$", sprintf("%05d", seq_along(x)), 
                          unname(do.call("c", purrr::map(x, "image_id")))
                        )
    )
    names(x) <- instance_id
    for(i in seq_along(x)){
      x[[i]]$instance_id <- names(x)[i]
    }
    return(x)
  })
  
  mask_df <- as.parsed_inference(mask_df)
  
  return(mask_df)
}

wing_length_calc <- function(x, FUN = max, units = c("px", "mm")){
  vmisc::warnifnot(is.parsed_inference(x))
  
  FUN <- match.fun(FUN)
  units <- match.arg(units)
  
  assert_variable_in_df(x, c("inlist"))
  
  res <- lapply(x$inlist, function(x){
    purrr::map(select_things(x, "forewing"), function(x){
      m <- x$keypoints[,c("x", "y")]
      dist(m)
    }) %>% 
      do.call("c",.) %>% 
      FUN()
  }) %>% 
    do.call("c",.) %>% 
    unname()
  
  res[!is.finite(res)] <- NA_real_
  
  if(units == "mm"){
    assert_variable_in_df(x, c("tick_size"))
    res <- res / x$tick_size
  }
  return(res)
}


thing_area_calc <- function(x, FUN = max, things = c("forewing", "hindwing", "body"), units = c("px", "mm")){
  vmisc::warnifnot(is.parsed_inference(x))
  
  FUN <- match.fun(FUN)
  units <- match.arg(units)
  
  
  assert_variable_in_df(x, c("inlist"))
  
  res <- lapply(x$inlist, function(x){
    lapply(
      things, function(thingi){
        p <- purrr::map(select_things(x, thingi), "polygon")
        res <- FUN(do.call("c", lapply(p, area)))
        return(res)
      }
    ) %>% 
      do.call("cbind", .) %>% 
      `colnames<-`(paste0(things, "_area"))
  }) %>% 
    do.call("rbind",.)
  
  res[!is.finite(res)] <- NA_real_
  
  if(units == "mm"){
    assert_variable_in_df(x, c("tick_size"))
    res <- res / x$tick_size^2
  }
  return(res)
}






