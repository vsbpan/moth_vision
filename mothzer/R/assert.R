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

has_keypoints <- function(df){
  "keypoints" %in% names(df)
}

has_mask <- function(df){
  "polygon" %in% names(df)
}