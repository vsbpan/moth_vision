check_image_registration <- function(path, register = FALSE, quiet = FALSE){
  database <- fetch_image_database()
  
  fn <- parse_file_name(path, keep_extn = TRUE)
  
  status <- fn %in% database$file_name
  fn_unmatched <- unique(fn[!status])
  n <- length(fn_unmatched)
  if(n == 0){
    if(!quiet){
      cli::cli_alert_success("All images have been registered.")
    }
    
    if(register){
      return(invisible(database))
    } else {
      return(invisible(status))
    }
    
  } else {
    if(register){
      if(!quiet){
        cli::cli_alert_info("{n} image{?s} ha{?s/ve} not yet been registered. Returning the updated database.") 
      }
      
      last_id <- if(nrow(database) > 0) max(database$image_id) else 0
      
      # Add space in last five digits for unique instance id.
      new_id <- as.character(((last_id / 100000) + seq_along(fn_unmatched)) * 100000)
      
      
      
      res <- rbind.fill(
        database,
        data.frame(
          "file_name" = fn_unmatched,
          "image_id" = new_id
        ) 
      )
      return(invisible(res))
    } else {
      if(!quiet){
        cli::cli_alert_danger("{n} image{?s} ha{?s/ve} not yet been registered. Set {.code register = TRUE}?")
      }
      return(invisible(status))
    }
  }
}

register_image_id <- function(path){
  write_path <- eval(as.list(args(fetch_image_database))$database_path)
  check_image_registration(path, register = TRUE) %>% 
    readr::write_csv(file = write_path)
  cli::cli_alert_success("Images registered!")
}

assign_image_id <- function(path, unregistered.ok = FALSE){
  o <- check_image_registration(path, quiet = TRUE)
  if(unregistered.ok){
    format(seq_along(path) * 100000, scientific = FALSE, trim = TRUE)
  } else {
    if(!all(o)){
      n <- sum(!o)
      cli::cli_abort("{n} image{?s} ha{?s/ve} not been registed. Run {.code register_image_id(path)} to register the images.")
    }
    database <- fetch_image_database()
    fn <- parse_file_name(path, keep_extn = TRUE)
    
    format(database$image_id[match(fn, database$file_name)], scientific = FALSE, trim = TRUE)
  }
  
}
