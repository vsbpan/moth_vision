# Turn polygon into a binary mask
# Kind of finiky and doesn't perform as well as graphics::polygon(), but it'll do. 
polygon2mask <- function(x,y = NULL, dim_xy, 
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
  
  warning("Not tested. Don't trust the results!")
  dim(mask) <- c(dim(mask)[1:2], 1, dim(mask)[3])
  return(as.cimg(mask))
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

area.polygon <- function(x, ...){
  if(is.null(x)){
    return(0)
  }
  abs(.polygon_area(x))
}

area.pixset <- function(x, ...){
  if(is.null(x)){
    return(0)
  }
  stopifnot(dim(x)[3] == 1)
  sum(x)
}

area.bbox <- function(x, ...){
  if(is.null(x)){
    return(0)
  }
  prod(abs(x[1,] - x[2,]))
}

# Polygon area calculation engine
.polygon_area <- function(coords){
  n <- nrow(coords)
  x <- coords[, 1, drop = TRUE]
  y <- coords[, 2, drop = TRUE]
  x1 <- x
  y1 <- y
  x2 <- c(x[-1], x[1])  
  y2 <- c(y[-1], y[1])
  
  # Shoelace formula calculation (vectorized sum of cross-products)
  area <- sum(x1 * y2 - x2 * y1)
  # The area is half the absolute value of the sum
  return(abs(area) / 2)
}

