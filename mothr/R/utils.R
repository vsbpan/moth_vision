
pretty_obj_size <- function(x){
  prettyunits::pretty_bytes(utils::object.size(x))
}