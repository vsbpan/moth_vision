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

validate_evaluator_input <- function(predictions, ground_truths){
  
  if(!is.parsed_inference(predictions)){
    cli::cli_abort("{.var predictions} must be of class {.cls parsed_inference}")
  }
  
  if(!is.parsed_inference(ground_truths)){
    cli::cli_abort("{.var ground_truths} must be of class {.cls parsed_inference}")
  }
  
  assert_variable_in_df(predictions, c("inlist", "id"))
  assert_variable_in_df(ground_truths, c("inlist", "id"))
  
  n_og <- nrow(predictions)
  n_gt <- nrow(ground_truths)
  matched <- match(ground_truths$id, predictions$id)
  matched <- matched[!is.na(matched)]
  n_new <- length(matched)
  
  cli::cli_alert_info("{n_new} of {n_og} image predictions found in {n_gt} ground truth{?s}.")
  predictions <- predictions[matched, ,drop = FALSE]
  
  
  predictions_inl <- predictions$inlist
  ground_truths_inl <- ground_truths$inlist
  
  if(length(predictions_inl) != nrow(ground_truths)){
    cli::cli_abort("{.var predictions} and {.var ground_truths_inl} must be the same length! Something is wrong in {.fn validate_evaluator_input}")
  }
  
  if(!all(do.call("c",lapply(predictions_inl, is.inlist)))){
    cli::cli_abort("{.var predictions} must have a column for a list of {.cls inlist}")
  }
  
  if(!all(do.call("c",lapply(ground_truths_inl, is.inlist)))){
    cli::cli_abort("{.var ground_truths} must have a column for a list of {.cls inlist}")
  }
  return(
    list(
      "pred" = predictions_inl,
      "gt" = ground_truths_inl
    )
  )
}

assert_variable_in_df <- function(df, variable){
  v <- variable[!variable %in% colnames(df)]
  
  if(length(v) > 0){
    n <- length(v)
    df_name <- deparse(substitute(df))
    cli::cli_abort("Cannot find {n} column{?s} called {.var {v}} in the object {.var {df_name}}")
  }
}


validate_image_dir <- function(root_path = "C:/",
                               dir_name = "moth_photos",
                               subdir = c("input_photos", "pending_merge", "database"), 
                               create = FALSE){
  dir_path <- get_img_dir_path(root_path, dir_name)
  subdir_paths <- paste(dir_path, subdir, sep = "/")
  subdir_paths <- remove_dup_slash(subdir_paths)
  
  all_paths <- c(dir_path, subdir_paths)
  e <- vapply(all_paths, dir.exists, FUN.VALUE = logical(1))
  missing_dir <- all_paths[!e]
  
  if(isFALSE(create) && any(!e)){
    cli::cli_abort("Missing the following expected directories: {.file {missing_dir}}. Try running {.fn init_image_dir} to create the expected file tree.")
  } else if(isTRUE(create) && any(!e)){
    cli::cli_inform("Created missing directories: {.file {missing_dir}}.")
    lapply(missing_dir, dir.create)
  }
  return(invisible(TRUE))
}




