file_orientation <- function(files){
  res <- exifr::read_exif(files, tags = c("Orientation","ExifImageWidth", "ExifImageHeight"))
  
  a <- vapply(res$ExifImageHeight > res$ExifImageWidth, isTRUE, logical(1))
  res[a, "Orientation"] <- 8L
  return(res)
}

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
move_files_pb <- function (from_dir, to_dir, file_names, pb = FALSE){
  n <- length(file_names)
  for (i in seq_len(n)) {
    file.rename(paste(from_dir, file_names[i], sep = "/"), 
                paste(to_dir, file_names[i], sep = "/"))
    if(pb){
      cat("Moving file", i, "of", n  ,"\r")
    }
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

drop_extn <- function(path){
  extn <- tools::file_ext(path)
  gsub_element_wise(paste0(".",extn), replacement = "", x = path)
}

parse_file_name <- function(path, keep_extn = TRUE){
  res <- basename(gsub("\\\\","/", path))
  if(keep_extn){
    res <- drop_extn(res)
  }
  return(res)
}
