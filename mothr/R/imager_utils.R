plot.cimg <- function (x, frame, xlim = c(1, width(x)), ylim = c(height(x), 
                                                    1), xlab = "x", ylab = "y", rescale = TRUE, colourscale = NULL, 
          colorscale = NULL, interpolate = FALSE, axes = TRUE, main = "", 
          xaxs = "i", yaxs = "i", asp = 1, col.na = grDevices::rgb(0, 0, 0, 0), ...) {
  v <- unique(c(x))
  if (length(v[!is.na(v)]) < 2) {
    rescale <- FALSE
  }
  if (nPix(x) == 0) 
    stop("Empty image")
  im <- x
  if (depth(im) > 1) {
    if (missing(frame)) {
      warning("Showing first frame")
      frame <- 1
    }
    im <- imager::frame(x, frame)
  }
  if (1 %in% dim(im)[1:2]) {
    plot.singleton(im, ...)
  }
  else {
    if (is.character(asp) && asp == "varying") {
      plot(1, 1, xlim = xlim, ylim = ylim, xlab = xlab, 
           ylab = ylab, type = "n", xaxs = xaxs, yaxs = yaxs, 
           axes = axes, ...)
      grDevices::as.raster(im, rescale = rescale, colorscale = colorscale, 
                           colourscale = colourscale, col.na = col.na) %>% 
        graphics::rasterImage(1, height(im), width(im), 
                              1, interpolate = interpolate)
      graphics::title(main)
    }
    else if (is.numeric(asp)) {
      graphics::plot.new()
      graphics::plot.window(xlim = xlim, ylim = ylim, asp = asp, 
                            xaxs = xaxs, yaxs = yaxs, ...)
      rst <- grDevices::as.raster(im, rescale = rescale, 
                                  colorscale = colorscale, colourscale = colourscale, 
                                  col.na = col.na)
      graphics::rasterImage(rst, 1, nrow(rst), ncol(rst), 
                            1, interpolate = interpolate)
      graphics::title(main)
      if (axes) {
        graphics::axis(1)
        graphics::axis(2)
      }
    }
    else {
      stop("Invalid value for parameter asp")
    }
  }
  invisible(x)
}

plot.pixset <- function (x, frame, xlim = c(1, width(x)), ylim = c(height(x), 
                                                    1), xlab = "x", ylab = "y", rescale = TRUE, colourscale = NULL, 
          colorscale = NULL, interpolate = FALSE, axes = TRUE, main = "", 
          xaxs = "i", yaxs = "i", asp = 1, col.na = grDevices::rgb(0, 
                                                                   0, 0, 0), ...) 
{
  plot(as.cimg(x), frame = frame, xlim = xlim, ylim = ylim, 
       xlab = xlab, ylab = ylab, rescale = rescale, colourscale = colourscale, 
       colorscale = colorscale, interpolate = interpolate, axes = axes, 
       main = main, xaxs = xaxs, yaxs = yaxs, asp = asp, col.na = col.na, 
       ...)
}

plot.imlist <- function (x, main = "", main.panel = NULL, interpolate = FALSE, 
                         ...) {
  spatstat.geom::plot.imlist(x, plotcommand = "plot", main = main, 
                             interpolate = interpolate, main.panel = main.panel, ...)
}

plot.singleton <- function (x, ...) {
  varying <- if (width(x) == 1) 
    "y"
  else "x"
  l <- max(dim(x)[1:2])
  if (imager::spectrum(x) == 1) {
    plot(1:l, as.vector(x), xlab = varying, ylab = "Pixel value", 
         type = "l", ...)
  }
  else if (imager::spectrum(x) == 3) {
    ylim <- range(x)
    plot(1:l, 1:l, type = "n", xlab = varying, ylim = ylim, 
         ylab = "Pixel value", ...)
    cols <- c("red", "green", "blue")
    for (i in 1:3) {
      graphics::lines(1:l, as.vector(channel(x, i)), type = "l", 
                      col = cols[i])
    }
  }
  else {
    stop("Unsupported image format")
  }
}


