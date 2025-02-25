
pretty_obj_size <- function(x){
  prettyunits::pretty_bytes(utils::object.size(x))
}


launch_photo <- function(path){
  shell(sprintf("Open %s", path))
}

drop_attributes <- function(x, exclude = NULL){
  if(is.null(exclude)){
    attributes(x) <- NULL
  } else {
    attributes(x) <- attributes(x)[exclude]
  }
  return(x)
}