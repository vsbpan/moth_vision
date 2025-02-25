plot.keypoints <- function(x, 
                          kp_name = rownames(x), 
                          col = colorpal(nrow(x)), 
                          pch = 19, 
                          shrink = 1,
                          offset = NULL,
                          ...){
  if(is.null(x)){
    cli::cli_alert_danger("No detected keypoint.")
    return(invisible(NULL))
  }
  
  if(is.null(offset)){
    offset <- c(0,0)
  }
  
  x[,1] <- x[,1] - offset[1]
  x[,2] <- x[,2] - offset[2]
  
  x <- x / shrink
  x_coord <- x[,1]
  y_coord <- x[,2]
  
  missing_coord <- is.na(x_coord) | is.na(y_coord)
  
  if(any(missing_coord)){
    a <- kp_name[missing_coord]
    cli::cli_alert_danger("No detected keypoint for {a}")
  }

  points(x = x_coord, y = y_coord, col = col, pch = pch, ...)
}

plot.polygon <- function(x, col = "green", 
                         alpha = 0.5, shrink = 1, border = NA, 
                         offset = NULL, ...){
  col <- grDevices::adjustcolor(col = col, alpha.f = alpha)
  
  if(is.null(offset)){
    offset <- c(0,0)
  }
  x[,1] <- x[,1] - offset[1]
  x[,2] <- x[,2] - offset[2]
  
  x <- x / shrink
  if(is.null(x)){
    cli::cli_alert_danger("No polygon detected!")
  } else {
    x %>% polygon(col = col, border = border, ...)
  }
}

# Draw bounding box
plot.bbox <- function(x, fill = "#00000000", col = "red", 
                      alpha = 1, shrink = 1, offset = NULL, ...){
  col <- grDevices::adjustcolor(col = col, alpha.f = alpha)
  
  if(is.null(x)){
    cli::cli_alert_danger("No bbox detected!")
  }
  
  if(is.null(offset)){
    offset <- c(0,0)
  }
  
  x[,1] <- x[,1] - offset[1]
  x[,2] <- x[,2] - offset[2]
  
  x <- x / shrink
  graphics::rect(xleft = x[1,1], 
                 xright = x[2,1], 
                 ybottom = x[1,2], 
                 ytop = x[2,2], 
                 col = fill, 
                 border = col, 
                 ...)
}


plot.instance <- function(x, 
                          bbox = FALSE,
                          mask_col = "green",
                          mask_alpha = 0.5,
                          mask_border = NA,
                          kp_name = rownames(x$keypoints), 
                          kp_col = colorpal(nrow(x$keypoints)), 
                          kp_pch = 19,
                          bbox_col = "red",
                          bbox_fill = "#00000000",
                          shrink = 1,
                          offset = NULL,
                          ...){

    if(has_bbox(x) && bbox){
    plot(x$bbox, fill = bbox_fill, col = bbox_col, shrink = shrink, offset = offset)
  }
  if(has_mask(x)){
    plot(x$polygon, col = mask_col, alpha = mask_alpha, border = mask_border, shrink = shrink, offset = offset)
  }
  if(has_keypoints(x)){
    plot(x$keypoints, kp_name = kp_name, pch = kp_pch, col = kp_col, shrink = shrink, offset = offset)
  }
}


plot.inlist <- function(x, 
                        bbox = FALSE,
                        bbox_moth = FALSE,
                        mask_alpha = 0.5,
                        mask_border = NA,
                        kp_pch = 19,
                        bbox_col = "red",
                        bbox_fill = "#00000000",
                        bbox_moth_col = "yellow",
                        bbox_moth_fill = "#00000000",
                        shrink = 1,
                        offset = NULL,
                        ...){

  for (i in seq_along(x)){
    thing <- x[[i]]$thing_class
    
    plot(x[[i]], 
         bbox = bbox,
         mask_col = match_category_color(thing),
         mask_alpha = mask_alpha,
         mask_border = mask_border,
         kp_name = rownames(x$keypoints), 
         kp_col = match_keypoint_color(thing)[[1]], 
         kp_pch = kp_pch,
         bbox_col = bbox_col,
         bbox_fill = bbox_fill,
         shrink = shrink,
         offset = offset,
         ...)
  }
  
  if(isTRUE(bbox_moth)){
    plot(moth_bbox(x), shrink = shrink, col = bbox_moth_col, fill = bbox_moth_fill, offset = offset)
  }
  
  return(invisible(NULL))
}





