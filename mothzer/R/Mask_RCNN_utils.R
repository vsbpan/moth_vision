# Turn polygon into a binary mask
# Kind of finiky and doesn't perform as well as graphics::polygon(), but it'll do. 
polygon2mask <- function(x,y = NULL, dim_xy = c(1000, 1000), 
                         mini_mask = FALSE, raw_mat = FALSE){
  if(is.null(x)){
    mask <- imager::imfill(x = dim_xy[1], y = dim_xy[2], val = 0)
    if(raw_mat){
      return(NULL)
    }
    return(mask)
  }
  
  if(is.null(y)){
    y <- x[,2]
    x <- x[,1]
  }
  
  x <- round(x)
  y <- round(y)
  
  mask <- spatstat.geom::owin(poly = list(
    x = x, y = y, check = FALSE
  ))
  
  if(mini_mask){
    if(is.null(mask)){
      if(raw_mat){
        return(mask)
      } else {
        mask <- imager::imfill(x = diff(range(x)), y = diff(range(y)), val = 0)
      }
      return(mask)
    }
    mask <- mask %>% 
      spatstat.geom::as.mask(dimyx = c(diff(range(x)), diff(range(y))), 
                             xy = list(x = seq(min(x), max(x)), y = seq(min(y), max(y))))
    
    if(raw_mat){
      return(mask)
    } else {
      mask <- mask %>% spatstat.geom::as.array.im()
    }
    
  } else {
    if(is.null(mask)){
      mask <- imager::imfill(x = dim_xy[1], y = dim_xy[2], val = 0)
      return(mask)
    }
    mask <- mask %>% 
      spatstat.geom::as.mask(dimyx = c(diff(range(x)), diff(range(y))), 
                             xy = list(x = seq_len(dim_xy[1]), y = seq_len(dim_xy[2]))) %>% 
      spatstat.geom::as.array.im()
  }
  
  dim(mask) <- c(dim(mask)[1:2], 1, dim(mask)[3])
  return(as.cimg(mask) %>% flip_xy())
}

# Turn a binary mask into a polygon
mask2polygon <- function(mask){
  l <- spatstat.geom::owin(mask = as.pixset(mask)[,,1,1]) %>% 
    spatstat.geom::as.polygonal() %>% 
    .$bdry %>% 
    .[[1]]
  out <- do.call("cbind",l)[,c(2,1)]
  colnames(out) <- c("x", "y")
  return(out)
}

# Calculate mask centroid using polygon
polygon_centroid <- function(poly){
  o <- polygon2mask(poly, mini_mask = TRUE, raw_mat = TRUE)
  c(mean_wt(o$xcol, colSums(o$m)), mean_wt(o$yrow, rowSums(o$m)))
}

# Validate polygon to have only positive area
validate_polygon <- function(poly){
  poly <- poly[!is.na(poly[,1]) & !is.na(poly[,2]), , drop = FALSE]
  
  if(is.null(poly)){
    return(NULL)
  }
  
  if(nrow(poly) < 3){
    return(NULL)
  }
  
  if(length(unique(poly[,1])) < 2 || length(unique(poly[,2])) < 2){
    return(NULL)
  }
  
  if(!.polygon_area(poly) < 0){
    out <- apply(poly, 2, rev)
  } else {
    out <- poly
  }
  return(out)
}
