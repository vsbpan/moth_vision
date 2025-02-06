is.raw_inference <- function(x){
  inherits(x, "raw_inference")
}

# Check if path is relative to current working directory
is_relative_path <- function(x){
  !grepl(getwd(), x)
}

# check if the object is COCO_Json
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