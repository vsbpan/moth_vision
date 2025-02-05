print.raw_inference <- function(x){
  a <- nrow(x$meta)
  b <- nrow(x$inference)
  things <- cli::col_yellow(unique(x$inference$thing_class))
  
  anno_types <- c("bbox","polygon","keypoints")
  anno_types <- anno_types[anno_types %in% names(x$inference)]
  
  if(any(anno_types == "polygon")){
    anno_types[anno_types == "polygon"] <- "mask"
  }
  anno <- cli::col_blue(anno_types)
  cat(cli::cli_text("Raw inference annotation with {a} image{?s} and {b} annotation{?s}"))
  cat(cli::cli_text("things: {things}"))
  cat(cli::cli_text("annotations: {anno}"))
}

print.COCO_Json <- function(x){
  a <- nrow(x$images)
  b <- nrow(x$annotations)
  anno_types <- c("bbox","segmentation","keypoints")
  anno_types <- anno_types[anno_types %in% names(x$annotations)]
  
  if(any(anno_types == "segmentation")){
    anno_types[anno_types == "segmentation"] <- "mask"
  }
  anno <- cli::col_blue(anno_types)
  things <- cli::col_yellow(x$categories[,"name"])
  cat(cli::cli_text("COCO annotation with {a} image{?s} and {b} annotation{?s}"))
  cat(cli::cli_text("things: {things}"))
  cat(cli::cli_text("annotations: {anno}"))
}
