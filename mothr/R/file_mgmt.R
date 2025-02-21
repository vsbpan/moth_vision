# Detect file orientation with exif data
file_orientation <- function(files){
  res <- exifr::read_exif(files, tags = c("Orientation","ExifImageWidth", "ExifImageHeight"))
  
  a <- vapply(res$ExifImageHeight > res$ExifImageWidth, isTRUE, logical(1))
  res[a, "Orientation"] <- 8L
  return(res)
}

# Auto correct the image orientation based on exif data. Can be wrong if the exif data is gussed wrong by the camera
auto_rotate <- function(file, orientation){
  angle <- switch(as.character(orientation), 
                  "3" = 180,
                  "1" = 0, 
                  "8" = 90,
                  "6" = 270)
  if(angle != 0){
    img <- imager::load.image(file)
    img <- imager::imrotate(img, angle = angle + 90)
    imager::save.image(img, file, quality = 1)
  }
}

# Rotate images in a root path
auto_rotate_dir <- function(root_path, pattern = "JPG|jpeg|jpg|JPEG", recursive = FALSE){
  res <- list.files(root_path, full.names = TRUE, pattern = pattern, recursive = recursive) %>% 
    file_orientation()
  z <- vapply(res$Orientation != "1", isTRUE, logical(1))
  files <- res$SourceFile[z]
  o <- res$Orientation[z]
  n <- length(files)
  cli::cli_alert("Detected {n} file{?s} to rotate.")
  lapply(cli::cli_progress_along(files, 
  format = "Processing item {cli::pb_current} of {cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}"), function(i){
    auto_rotate(files[i], o[i])
  })
  return(invisible(NULL))
}


# Move files with file.rename() with a progress bar
move_files_pb <- function (from_dir, to_dir, file_names, pb = TRUE){
  if(pb){
    a <- seq_along(file_names)
  } else {
    a <- cli::cli_progress_along(file_names, format = "Processing item {cli::pb_current} of {cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")
  }
  
  for (i in a) {
    file.rename(paste(from_dir, file_names[i], sep = "/"), 
                paste(to_dir, file_names[i], sep = "/"))
  }
  if(pb){
    cli::cli_progress_done()
  }
}

# Append working directory to the path
abs_path <- function(x){
  if(is_relative_path(x)){
    paste(getwd(),x, sep = "/") 
  } else {
    x
  }
}

# Perform gsub() by element
gsub_element_wise <- function(pattern, replacement, x, 
                              ignore.case = FALSE, perl = FALSE, 
                              fixed = FALSE, useBytes = FALSE){
  n <- length(x)
  if(n > 1){
    if(length(replacement) == 1){
      replacement <- rep(replacement, n)
    }
    if(length(pattern) == 1){
      pattern <- rep(pattern, n)
    }
  }
  
  lapply(seq_along(x), function(i){
    gsub(pattern[i], replacement[i], x[i])
  }) %>% 
    do.call("c",.)
}

# The name of the root directory
file_root <- function(x){
  gsub_element_wise(basename(x), "", x)
}

# Drop the extension of a path
drop_extn <- function(path){
  extn <- tools::file_ext(path)
  gsub_element_wise(paste0(".",extn), replacement = "", x = path)
}

# Parse file name from a path
parse_file_name <- function(path, keep_extn = TRUE){
  res <- basename(gsub("\\\\","/", path))
  if(keep_extn){
    res <- drop_extn(res)
  }
  return(res)
}


# Auto initiate the file tree if it doesn't exist. 
init_image_dir <- function(root_path = "C:/",
                           dir_name = "moth_photos",
                           subdir = c("input_photos", "pending_merge", "database")){
  res <- validate_image_dir(root_path, dir_name = dir_name, subdir = subdir, create = TRUE)
  if(isTRUE(res)){
    cli::cli_alert_success("Expected directories found.")
  }
}


get_database_path <- function(root_path = "C:/"){
  paste(get_img_dir_path(root_path),"database", sep = "/")
}


get_input_path <- function(root_path = "C:/"){
  paste(get_img_dir_path(root_path),"input_photos", sep = "/")
}


get_pending_path <- function(root_path = "C:/"){
  paste(get_img_dir_path(root_path),"pending_merge", sep = "/")
}

get_img_dir_path <- function(root_path = "C:/", dir_name = "moth_photos"){
  paste(root_path, dir_name, sep = "/")
}


collect_images <- function(root_path = "C:/"){
  validate_image_dir(root_path, create = FALSE)
  
  input_path <- get_input_path(root_path)
  pending_path <- get_pending_path(root_path)
  
  existing_files <- list.file(pending_path, pattern = ".jpg", 
                              full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  n_exist <- length(existing_files)
  if(n_exist > 0){
    cli::cli_abort("{n} image{?s} detected in the directory {.file pending_path}. This is unexpected! Remove those images before rerunning this function.")
  }
  
  files <- list.files(input_path, pattern = ".JPG", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  file_new_path <- gsub("/","__subdir__",gsub(input_path,"",files))
  
  res <- file.copy(files, file_new_path)
  if(!all(res)){
    n <- sum(!res)
    cli::cli_abort("{n} file{?s} failed to copy. Something is wrong.")
  }
  return(invisible(NULL))
}


image_in_database <- function(x, root_path = "C:/", quiet = FALSE){
  db_path <- get_database_path(root_path)
  files <- list.files(db_path, pattern = ".jpg", 
             full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  res <- vapply(files, function(f){
    testthat::compare_file_binary(x, f)
  })
  files_matched <- files[res]
  n <- length(files_matched)
  
  if(isFALSE(quiet) && n > 0){
    cli::cli_alert_warning("{n} image{?s} in the database match the proposed image: {.file files_matched}")
  }
  
  return(any(files_matched))
}

merge_to_database <- function(root_path = "C:/"){
  db_path <- get_database_path(root_path)
  input_path <- get_input_path(root_path)
  pending_path <- get_pending_path(root_path)
  
  pending_files <- list.files(pending_path, 
                              pattern = ".jpg", 
                              full.names = TRUE, 
                              recursive = TRUE, 
                              ignore.case = TRUE)
  n_pending <- length(pending_files)
  
  input_files <- list.files(input_path, 
                              pattern = ".jpg", 
                              full.names = TRUE, 
                              recursive = TRUE, 
                              ignore.case = TRUE)
  n_input <- length(input_files)
  
  db_files <- list.files(db_path, 
                            pattern = ".jpg", 
                            full.names = TRUE, 
                            recursive = TRUE, 
                            ignore.case = TRUE)
  n_db <- length(db_files)
  
  
  if(n_pending == 0){
    cli::cli_inform("There is no image in {.file pending_path} to merge.")
    if(n_input != 0){
      cli::cli_inform("There are {n_input} image{?s} in {.file input_path}. Have you tried to run {.fn collect_images()}?")
    }
    return(invisible(NULL))
  }
  
  ids <- gsub("img_moth_","", parse_file_name(db_files, keep_extn = FALSE))
  last_id <- max(as.numeric(ids))
  
  new_ids <- last_id + seq_len(n_pending)
  new_file_names <- sprintf("img_moth_%08d.jpg", new_ids)
  new_file_paths <- paste(db_path, new_file_names, sep = "/")
  
  dup <- base::interaction(new_file_names, basename(db_files))
  n_dup <- length(dup)
  if(n_dup > 0){
    cli::cli_abort("There seem to be {n_dup} image files in the database with the same name as the proposed new names. Something is wrong. Contact admin for guidence. Offending entr{?es/ies}: {.file dup}")
  }
  
  
  cli::cli_progress_step("Merging {n_pending} new images to the database.", 
                         msg_done = "Successfully merged {n_pending} images to the database!", 
                         msg_failed = "Error in merging {n_pending} images to the database!"
                         )
  file.rename(
    pending_files,
    new_file_paths
  )
  cli::cli_progress_done()
  cli::cli_progress_cleanup()
  
  list.dirs(input_path, full.names = TRUE, recursive = FALSE) %>% 
    lapply(dir_remove)
  return(invisible(NULL))
}

dir_remove <- function(x, recursive = TRUE, force = FALSE) {
  if (unlink(x, recursive, force) == 0)
    return(invisible(TRUE))
  cli::cli_abort("Failed to remove {.file x}")
}

flag_image_exclude <- function(x){
  db <- fetch_exclude_flags()
  new_d <- data.frame(
    "file_name" = x
  )
  db <- rbind(db, new_d) %>% dplyr::distinct()
  n <- length(x)
  write_path <- eval(as.list(args(fetch_exclude_flags))$database_path)
  write_csv(db, file = write_path)
  cli::cli_alert_success("Flagged {n} image{?s} to exclude from database.")
}

