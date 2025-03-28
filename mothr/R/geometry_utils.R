# Turn polygon into a binary mask
# Kind of finiky and doesn't perform as well as graphics::polygon(), but it'll do. 
as.pixset.polygon <- function(x,y = NULL, dim_xy, 
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
  
  mx <- max(x, na.rm = TRUE)
  my <- max(y, na.rm = TRUE)
  if(dim_xy[1] < mx || dim_xy[2] < my){
    
    cli::cli_abort("The maximum dimension in polygon is x = {mx}, y = {my}, but the supplied {.var dim_xy} is smaller.")
  }
  
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
  
  mask <- imager::as.pixset(flip_xy(imager::as.cimg(mask)))
  return(mask)
}

# Turn a binary mask into a polygon
as.polygon.pixset <- function(x){
  if(any(dim(x)[3:4] > 1)){
    cli::cli_warn("Expecting only channel = 1 and depth = 1. Ignoring the rest of the dimensions.")
  }
  if(sum(x) < 1){
    cli::cli_warn("Binary mask has zero area. Returning NULL.")
    return(NULL)
  }
  l <- spatstat.geom::owin(mask = x[,,1,1]) %>% 
    spatstat.geom::as.polygonal() %>% 
    .$bdry %>% 
    .[[1]]
  out <- do.call("cbind",l)[,c(2,1)]
  colnames(out) <- c("x", "y")
  return(out)
}

centroid.pixset <- function(x){
  if(sum(x) < 1){
    cli::cli_warn("Binary mask has zero area. Returning NULL.")
    return(NULL)
  }
  as.data.frame(x) %>% 
    as.matrix() %>% 
    matrixStats::colMeans2() %>% 
    .[1:2]
}

centroid.bbox <- function(x){
  res <- unname(colMeans(x))
  return(c("x" = res[1], "y" = res[2]))
}

as.pixset.bbox <- function(x, dim_xy){
  p <- imager::imfill(val = FALSE, dim = c(dim_xy, 1, 1))
  x_coord <- x[,1]
  y_coord <- x[,2]
  p[vmisc::seq_interval(round(x_coord), by = 1),vmisc::seq_interval(round(y_coord), by = 1), , ] <- TRUE
  return(p)
}

as.pixset.instance <- function(x, dim_xy){
  as.pixset(x$polygon,dim_xy = dim_xy)
}

as.pixset.inlist <- function(x, dim_xy){
  x %>% 
    lapply(function(x, dim_xy){
      as.pixset(x,dim_xy = dim_xy)
    }, dim_xy = dim_xy) %>% 
    imager::as.imlist() %>% 
    imager::parany()
}

centroid.polygon <- function(x){
  n <- nrow(x)
  x <- rbind(x, x[1,])
  
  x_coord <- x[, 1]
  y_coord <- x[, 2]
  area <- sum(x_coord[-(n + 1)] * y_coord[-1] - x_coord[-1] * y_coord[-(n + 1)]) / 2
  
  common_term <- (x_coord[-(n + 1)] * y_coord[-1] - x_coord[-1] * y_coord[-(n + 1)])
  cx <- sum((x_coord[-(n + 1)] + x_coord[-1]) * common_term) / (6 * area)
  cy <- sum((y_coord[-(n + 1)] + y_coord[-1]) * common_term) / (6 * area)
  
  return(c("x" = cx, "y" = cy))
}

area.polygon <- function(x, ...){
  if(is.null(x)){
    return(0)
  }
  abs(.polygon_area(x))
}

area.pixset <- function(x, na.rm = FALSE, ...){
  if(is.null(x)){
    return(0)
  }
  stopifnot(dim(x)[3] == 1)
  sum(x, na.rm = na.rm)
}

area.bbox <- function(x, ...){
  if(is.null(x)){
    return(0)
  }
  prod(abs(x[1,] - x[2,]))
}

as.bbox.polygon <- function(x, ...){
  res <- cbind(range(x[, 1]),(range(x[, 2])))
  class(res) <- c("bbox", "matrix", "array")
  colnames(res) <- c("x", "y")
  res
}

as.polygon.bbox <- function(x){
  x1 <- x[1, 1]
  y1 <- x[1, 2]
  x2 <- x[2, 1]
  y2 <- x[2, 2]
  
  coords <- matrix(c(x1, y1,  # Bottom-left corner
                     x2, y1,  # Bottom-right corner
                     x2, y2,  # Top-right corner
                     x1, y2), # Top-left corner
                   ncol = 2, byrow = TRUE)
  colnames(coords) <- c("x", "y")
  # Close the polygon by repeating the first vertex at the end
  coords <- rbind(coords, coords[1, ])
  coords <- validate_polygon(coords)
  return(coords)
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


as.bbox.instance <- function(x, ...){
  x$bbox
}

as.bbox.inlist <- function(x, ...){
  res <- purrr::map(x, "bbox") %>% do.call("rbind", .)
  as.bbox.polygon(res)
}


shrink_bbox <- function(x, shrink, ...){
  round(x/shrink)
}

bbox_crop <- function(img, bbox){
  stopifnot(imager::is.cimg(img) | imager::is.pixset(img))
  
  if(missing(bbox)){
    cli::cli_abort("{.var bbox} is missing with no default.")
  }
  x_range <- bbox[,"x", drop = TRUE]
  y_range <- bbox[,"y", drop = TRUE]
  
  x_inrange <- all(is.between(x_range, c(1, ncol(img)),inclusive = TRUE))
  y_inrange <- all(is.between(y_range, c(1, nrow(img)),inclusive = TRUE))
  
  if(!x_inrange && !y_inrange){
    cli::cli_abort(c("Bounding box has range outside of the image!",
                     "Image x dim from {.val {1}} to {.val {ncol(img)}}", 
                     "bbox x dim from {.val {min(x_range)}} to {.val {max(x_range)}}",
                     "Image y dim from {.val {1}} to {.val {nrow(img)}}", 
                     "bbox y dim from {.val {min(y_range)}} to {.val {max(y_range)}}"
    ))
  }
  
  img[vmisc::seq_interval(x_range, by = 1),
      vmisc::seq_interval(y_range, by = 1),
      ,, drop = FALSE]
}



# as_relative.default <- function(x, bbox_moth, offset = NULL){
#   if(is.null(offset)){
#     offset <- matrixStats::colMins(bbox_moth)
#   }
#   attr(x, "offset") <- offset
#   x
# }


refind_coords.default <- function(x, offset = NULL){
  x[,"x"] <- x[,"x"] - offset[1]
  x[,"y"] <- x[,"y"] - offset[2]
  return(x)
}





