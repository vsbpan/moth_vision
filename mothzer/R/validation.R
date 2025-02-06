# Validate polygon to have only positive area
validate_polygon <- function(poly){
  poly <- poly[!is.na(poly[,1]) & !is.na(poly[,2]), , drop = FALSE]
  
  if(is.null(poly)){
    return(NULL)
  }
  
  if(nrow(poly) < 3){
    return(NULL)
  }
  
  if(length(unique(poly[,1])) < 2 || length(unique(poly[,2])) < 2){
    return(NULL)
  }
  
  if(!.polygon_area(poly) < 0){
    out <- apply(poly, 2, rev)
  } else {
    out <- poly
  }
  class(out) <- c("polygon", "matrix", "array")
  return(out)
}

validate_inference <- function(df_meta, df_inference){
  setdiff_vec <- base::setdiff(unique(df_inference$file_name),unique(df_meta$file_name))
  if(!isTRUE(length(setdiff_vec) == 0)){
    cli::cli_abort("Detected file name in {.var df_inference} that is not found in {.var df_meta}. Are you sure these are the right files?")
  }
  
  # Do other things here
  
  return(invisible(NULL))
  
}


