
update_tag_id <- function(x){
  db <- fetch_verified_tag_id()
  x$tag_id <- vapply(seq_along(x$tag_id_guess), function(i){
    if(x$file_name[i] %in% db$file_name){
      db$tag_id[which(db$file_name %in% x$file_name[i])]
    } else {
      x$tag_id_guess[i]
    }
  }, character(1))
  return(x)
}

update_tick_size <- function(x){
  db <- fetch_verified_tick_size()
  x$tick_size <- vapply(seq_along(x$tick_size), function(i){
    if(x$file_name[i] %in% db$file_name){
      db$tick_size[which(db$file_name %in% x$file_name[i])]
    } else {
      x$tick_size[i]
    }
  }, numeric(1))
  return(x)
}


flag_image_exclude <- function(x){
  db <- fetch_exclude_flags()
  fn <- basename(x)
  extns <- tools::file_ext(x)
  if(any(extns == "")){
    e <- fn[extns == ""]
    cli::cli_abort("Missing {length(e)} expected file extension{?s}: {e}")
  }
  new_d <- data.frame(
    "file_name" = fn
  )
  db <- rbind(db, new_d) %>% dplyr::distinct()
  n <- length(x)
  write_path <- eval(as.list(args(fetch_exclude_flags))$database_path)
  readr::write_csv(db, file = write_path)
  cli::cli_alert_success("Flagged {n} image{?s} to exclude from database.")
}

flag_inference_error<- function(x, version = "1.0"){
  db <- fetch_inference_error()
  fn <- basename(x)
  fn <- gsub("mini_moth","img_moth", x)
  extns <- tools::file_ext(x)
  if(any(extns == "")){
    e <- fn[extns == ""]
    cli::cli_abort("Missing {length(e)} expected file extension{?s}: {e}")
  }
  new_d <- data.frame(
    "file_name" = fn,
    "version" = version
  )
  db <- rbind(db, new_d) %>% dplyr::distinct()
  n <- length(x)
  write_path <- eval(as.list(args(fetch_inference_error))$database_path)
  readr::write_csv(db, file = write_path)
  cli::cli_alert_success(c(
    "Flagged {n} image{?s} as inference error to exclude from analysis.",
    "Current detection model ver. {cli::col_blue(version)}"
  ))
}

flag_verified_tag_id <- function(x, tag_id){
  db <- fetch_verified_tag_id()
  fn <- basename(x)
  extns <- tools::file_ext(x)
  if(any(extns == "")){
    e <- fn[extns == ""]
    cli::cli_abort("Missing {length(e)} expected file extension{?s}: {e}")
  }
  if(missing(tag_id)){
    cli::cli_abort("Missing {.arg tag_id} with no default.")
  }
  if(length(fn) != length(tag_id)){
    cli::cli_abort("{.arg tag_id} of length {length(tag_id)} is not the same length as {.arg x} of length {length(fn)}")
  }
  
  new_d <- data.frame(
    "file_name" = fn,
    "tag_id" = tag_id
  )
  db <- rbind(db, new_d) %>% dplyr::distinct()
  n <- length(x)
  write_path <- eval(as.list(args(fetch_verified_tag_id))$database_path)
  readr::write_csv(db, file = write_path)
  cli::cli_alert_success(c(
    "Added {n} verified tag id{?s} to database."
  ))
}


flag_historic_specimen <- function(x){
  db <- fetch_historic_flag()
  if(missing(x)){
    cli::cli_abort("Missing {.arg x} with no default.")
  }
  fn <- basename(x)
  extns <- tools::file_ext(x)
  if(any(extns == "")){
    e <- fn[extns == ""]
    cli::cli_abort("Missing {length(e)} expected file extension{?s}: {e}")
  }
  new_d <- data.frame(
    "file_name" = fn
  )
  db <- rbind(db, new_d) %>% dplyr::distinct()
  n <- length(x)
  write_path <- eval(as.list(args(fetch_historic_flag))$database_path)
  readr::write_csv(db, file = write_path)
  cli::cli_alert_success("Flagged {n} image{?s} as photo{?s} of historic specimen{?s}.")
}



