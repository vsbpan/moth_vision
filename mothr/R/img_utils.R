# Turns image into a single vector
image_flatten <- function(img){
  as.numeric(c(img))
}

# Restores flattened image into a cimg object
image_unflatten <- function(x){
  n <- length(x)
  ns <- sqrt(n)
  stopifnot(ns%%1 == 0)
  mat <- matrix(x, nrow = ns, ncol = ns)
  
  imager::as.cimg(mat)
}

# Generate mapping function for quad to quad transformation (useful for perspective correction)
q2q_map <- function(plist, qlist, z = FALSE){
  p1x <- plist[[1]][[1]]
  p2x <- plist[[2]][[1]]
  p3x <- plist[[3]][[1]]
  p4x <- plist[[4]][[1]]
  p1y <- plist[[1]][[2]]
  p2y <- plist[[2]][[2]]
  p3y <- plist[[3]][[2]]
  p4y <- plist[[4]][[2]]
  
  
  q1x <- qlist[[1]][[1]]
  q2x <- qlist[[2]][[1]]
  q3x <- qlist[[3]][[1]]
  q4x <- qlist[[4]][[1]]
  q1y <- qlist[[1]][[2]]
  q2y <- qlist[[2]][[2]]
  q3y <- qlist[[3]][[2]]
  q4y <- qlist[[4]][[2]]
  
  
  mat <- c(
    p1x, p1y, 1, 0, 0, 0, -p1x*q1x, -p1y*q1x, 
    0, 0, 0, p1x, p1y, 1,  -p1x*q1y, -p1y*q1y,
    p2x, p2y, 1, 0, 0, 0, -p2x*q2x, -p2y*q2x, 
    0, 0, 0, p2x, p2y, 1,  -p2x*q2y, -p2y*q2y,
    p3x, p3y, 1, 0, 0, 0, -p3x*q3x, -p3y*q3x, 
    0, 0, 0, p3x, p3y, 1,  -p3x*q3y, -p3y*q3y,
    p4x, p4y, 1, 0, 0, 0, -p4x*q4x, -p4y*q4x, 
    0, 0, 0, p4x, p4y, 1,  -p4x*q4y, -p4y*q4y
  )
  
  A <- t(matrix(mat, nrow = 8, ncol = 8))
  B <- matrix(c(q1x, q1y, q2x, q2y, q3x, q3y, q4x, q4y), ncol = 1)
  
  X <- solve(A, B)
  
  if(z){
    map_fun <- function(x,y,z) {
      list(
        x = (X[1] * x + X[2] * y + X[3]) / (1 + X[7] * x + X[8] * y),
        y = (X[4] * x + X[5] * y + X[6]) / (1 + X[7] * x + X[8] * y),
        z = z
      )
    }
  } else {
    map_fun <- function(x,y) {
      list(
        x = (X[1] * x + X[2] * y + X[3]) / (1 + X[7] * x + X[8] * y),
        y = (X[4] * x + X[5] * y + X[6]) / (1 + X[7] * x + X[8] * y)
      )
    }
  }
  
  return(map_fun)
}

#' @title Apply quad to quad transformation on an image
#' @description
#' Apply quad to quad transformation on an image using \code{q2q_map()}. Useful for perspective correction. For more details see \code{imager::imwarp()}. 
#' @param img A cimg object
#' @param dest_pts,init_pts list of lists of four coordinates (x,y) setting the location of initial and destination of the transformation. If \code{NULL} (default), the four corners of the image will be selected. See examples. 
#' @param retain_size if \code{TRUE}, the returned image retains the same size. If \code{FALSE} (default), if the destination coordinates are outside of the original image, the image is enlarged. 
#' @param coordinates \code{"absolute"} or \code{"relative"} (default \code{"absolute"})
#' @param boundary boundary conditions: \code{"dirichlet", "neumann", "periodic"}. Default \code{"dirichlet"}.
#' @param interpolation \code{"nearest", "linear", "cubic"} (default \code{"nearest"})
#' @examples
#' img <- image_example() 
#' 
#' original_coords <- list(
#'     list(1,1),
#'     list(1, 443),
#'     list(810, 443),
#'     list(810, 1)
#'    )
#' trans_coords <- list(
#'       list(1,1), # Top left
#'       list(1, 200), # Bottom left
#'       list(500, 300), # Bottom right
#'       list(400, 1) # Top right
#'     )
#' 
#' # Apply perspective transformation
#' trans_img <- q2q_trans(
#'     img,   
#'     dest_pts = trans_coords,
#'     init_pts = original_coords
#'   )
#'  
#'  
#' # Undo perspective transformation
#'  backtrans_img <- q2q_trans(
#'     trans_img,   
#'     dest_pts = original_coords,
#'     init_pts = trans_coords
#'   )  
#'  
#'  
#'  plot(img)
#'  plot(backtrans_img) # Identical, but with lower resolution
#'  
#'  plot(trans_img) # Transformed image
#'   
#' 
q2q_trans <- function(img,
                      init_pts = NULL,
                      dest_pts = NULL,
                      retain_size = FALSE,
                      coordinates = c("absolute","relative"), 
                      interpolation = c("nearest", "linear", "cubic"),
                      boundary = c("dirichlet", "neumann", "periodic")){
  
  coordinates <- match.arg(coordinates)
  interpolation <- match.arg(interpolation)
  boundary <- match.arg(boundary)
  
  
  ny <- ncol(img)
  nx <- nrow(img)
  
  if(is.null(init_pts)){
    init_pts <- list(
      list(1,1),
      list(1, ny),
      list(nx,ny),
      list(nx, 1)
    )
  }
  
  if(is.null(dest_pts)){
    dest_pts <- list(
      list(1,1),
      list(1, ny),
      list(nx,ny),
      list(nx, 1)
    )
  }
  
  if(!retain_size){
    xmax <- max(do.call("c",purrr::map(dest_pts, 1)))
    ymax <- max(do.call("c",purrr::map(dest_pts, 2)))
    
    if(xmax > nx || ymax > ny){
      
      grow_x <- pmax(xmax - nx, 0)
      grow_y <- pmax(ymax - ny, 0)
      nspec <- dim(img)[4]
      
      x_edge <- imager::as.cimg(array(rep(0, grow_x * ny * nspec), 
                              dim = c(grow_x, ny, 1, nspec)))
      y_edge <- imager::as.cimg(array(rep(0, (nx + grow_x) * grow_y * nspec), 
                              dim = c((nx + grow_x), grow_y, 1, nspec)))
      
      
      img <- imager::imappend(
        list(
          imager::imappend(list(img, x_edge), "x"),
          y_edge), 
        "y")
      
    } 
  }
  
  out <- imager::imwarp(img, 
                        map = q2q_map(dest_pts, init_pts, z = dim(img)[3] > 1), # Swap q2q_map direction with backward algorithm
                        direction = "backward", 
                        coordinates = coordinates,
                        interpolation = interpolation, 
                        boundary = boundary)
  
  return(out)
}

# Split image into different regions and returns the largest patch
split_max <- function(img){
  l <- imager::split_connected(img)
  max_index <- lapply(l, function(x) {
    sum(x, na.rm = TRUE)
  }) %>% which.max()
  
  l[[max_index]]
}


# Replace NA values with val
na_replace <- function(img, val){
  img[is.na(img)] <- val
  img
}

# Load image with JPEG 
fast_load_image <- function(path, transform = TRUE){
  bmp <- jpeg::readJPEG(path)
  if(is.na(dim(bmp)[3])){
    dim(bmp)[3] <- 1
  }
  if(transform){
    bmp <- bmp %>% aperm(c(2, 1, 3))
  }
  dim(bmp) <- c(dim(bmp)[1:2], 1, dim(bmp)[3])
  class(bmp) <- c("cimg", "imager_array", "numeric")
  bmp 
}


write_jpg <- function(x, file_path, transform = TRUE){
  if(dim(x)[3L] > 1L){
    cli::cli_abort("{.var x} must be an image not a video with depth {dim(x)[3L]}.")
  }
  x <- as.bmp(x)
  if(transform){
    x <- x %>% aperm(c(2, 1, 3))
  }
  jpeg::writeJPEG(image = x, target = file_path, quality = 1)
}


# Reformat cimg as bitmap format for interface with jpeg package
as.bmp <- function(x){
  x <- imager::as.cimg(x)
  dim(x) <- dim(x)[-3]
  x
}

# Flip x and y axes
flip_xy <- function(img){
  f <- switch(class(img)[1], 
              cimg = imager::as.cimg, 
              pixset = imager::as.pixset, 
              array = as.array)
  return(f(aperm(img, c(2, 1, 3, 4))))
}
