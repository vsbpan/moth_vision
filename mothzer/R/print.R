print.raw_inference <- function(x, ...){
  a <- nrow(x$meta)
  b <- nrow(x$inference)
  things <- cli::col_yellow(unique(x$inference$thing_class))
  
  anno_types <- c("bbox","polygon","keypoints")
  anno_types <- anno_types[anno_types %in% names(x$inference)]
  
  anno <- cli::col_blue(anno_types)
  cat(cli::cli_text("Raw inference annotation with {a} image{?s} and {b} annotation{?s}"))
  cat(cli::cli_text("things: {things}"))
  cat(cli::cli_text("annotations: {anno}"))
  return(invisible(NULL))
}

print.COCO_Json <- function(x, ...){
  a <- nrow(x$images)
  b <- nrow(x$annotations)
  anno_types <- c("bbox","segmentation","keypoints")
  anno_types <- anno_types[anno_types %in% names(x$annotations)]
  
  anno <- cli::col_blue(anno_types)
  things <- cli::col_yellow(x$categories[,"name"])
  cat(cli::cli_text("COCO annotation with {a} image{?s} and {b} annotation{?s}"))
  cat(cli::cli_text("things: {things}"))
  cat(cli::cli_text("annotations: {anno}"))
  return(invisible(NULL))
}


print.inlist <- function(x, ...){
  n_instances <- length(x)
  anno_types <- lapply(x, find_labels) %>% do.call("c",.) %>% unique()
  anno_types <- cli::col_blue(anno_types)
  image_ids <- lapply(x, function(x){x$image_id}) %>% do.call("c",.) %>% unique()
  things <- lapply(x, function(x){x$thing_class}) %>% do.call("c",.) %>% unique()
  things <- cli::col_yellow(things)
  n_img <- length(image_ids)
  if(n_img != 1L){
    cli::cli_warn("There are {n_img} unique image{?s} in the object {.code inlist}. Is this extected?")
  }
  r1 <- sprintf("%s instances in imageID: %s", n_instances, paste0(image_ids, collapse = ", "))
  w <- cli::console_width() * 0.7
  
  cat(cli::cli_text("Instance list"))
  cat(cli::ansi_columns(r1, width = w, align = "left"))
  cat("\n")
  cat(cli::cli_text("things: {things}"))
  cat(cli::cli_text("labels: {anno_types}"))
  return(invisible(NULL))
}

print.instance <- function(x, ...){
  instance_id <- x$instance_id
  image_id <- x$image_id
  
  anno_types <- find_labels(x)
  anno_types <- cli::col_blue(anno_types)
  thing <- cli::col_yellow(x$thing_class)
  score <- cli::col_grey(format(round(x$score, 2), nsmall = 2))
  w <- cli::console_width() * 0.7
  r1 <- c(sprintf("imageID: %s", image_id), sprintf("instanceID: %s", instance_id))
  cat(cli::cli_text("Instance"))
  cat(cli::ansi_columns(r1, width = w, align = "left"))
  cat("\n")
  cat(cli::cli_text("thing: {thing} ({score})"))
  cat(cli::cli_text("labels: {anno_types}"))
  return(invisible(NULL))
}


print.bbox <- function(x, ...){
  cat(cli::cli_text("bounding box"))
  print(unclass(x))
  return(invisible(NULL))
}

print.polygon <- function(x, ...){
  n <- nrow(x)
  cat(cli::cli_text("polygon with {n} vertices"))
  return(invisible(NULL))
}

print.keypoints <- function(x, ...){
  cat(cli::cli_text("keypoints"))
  print(unclass(x))
  return(invisible(NULL))
}



