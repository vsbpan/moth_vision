is.raw_inference <- function(x){
  inherits(x, "raw_inference")
}

is_empty_instance <- function(x){
  all(is.na(find_things(x)))
}

# Check if path is relative to current working directory
is_relative_path <- function(x){
  !grepl(getwd(), x)
}

is.nested <- function(x){
  do.call("all",purrr::map(x, is.list))
}

is.parsed_inference <- function(x){
  inherits(x, "parsed_inference")
}


is.COCO <- function(x){
  inherits(x, "COCO_Json") 
}

is.inlist <- function(x){
  inherits(x, "inlist")
}

is.instance <- function(x){
  inherits(x, "instance")
}

is.polygon <- function(x){
  inherits(x, "polygon")
}

is.bbox <- function(x){
  inherits(x, "bbox")
}

is.keypoints <- function(x){
  inherits(x, "keypoints")
}

has_keypoints <- function(df){
  "keypoints" %in% names(df)
}

has_mask <- function(df){
  "polygon" %in% names(df)
}

has_bbox <- function(df){
  "bbox" %in% names(df)
}

is_relative <- function(x){
  !is.null(attr(x, "offset"))
}