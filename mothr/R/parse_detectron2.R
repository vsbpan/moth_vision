# Vectorized function that parse lists from python
parse_pylist <- function(x, coerce = as.numeric, simplify = TRUE){
  x <- gsub("c\\(|\\)","",x)
  out <- gsub("\\[|\\]","",x) %>% 
    str_split(",") %>% 
    lapply(function(x){
      coerce(x)
    })
  
  out[is.na(x)] <- list(NULL)
  
  if(simplify){
    out <- do.call("rbind",out)
    
    if(nrow(out) == 1L){
      out <- as.vector(out)
    } 
  }
  
  return(out)
}

# Inverse of parse_pylist(). Takes a list of vec and package as python list character string
unparse_pylist <- function(x){
  lapply(
    x,
    function(z){
      sprintf("[%s]",paste0(z, collapse = ", "))
    }
  ) %>% 
    do.call("c", .)
}


# Parse vector into n columns by looping through column index. Undo ravel() in python
split_n_steps <- function(x, n, name = paste0("x", seq_len(n))){
  if(all(is.na(x)) & length(x) == 1L || is.null(x)){
    #x <- rep(x, n)
    return(NULL)
  }
  
  index <- seq_along(x)
  
  out <- lapply(c(seq_len(n - 1),0), function(ni){
    x[(index %% n == ni)]
  }) %>% 
    do.call("cbind",.)
  colnames(out) <- name
  
  return(out)
}

# Turn a vector of keypoint values into a matrix
parse_keypoints_vec <- function(x){
  if(is.null(x)){
    return(NULL)
  }
  out <- split_n_steps(x, 3L, name = c("x","y","score"))
  class(out) <- c("keypoints", "matrix", "array")
  return(out)
}

# Turn vector of bbox values into a matrix
parse_bbox_vec <- function(x){
  if(is.null(x)){
    return(NULL)
  }
  out <- split_n_steps(x, 2L, name = c("x","y"))
  
  nr <- nrow(out)
  nc <- ncol(out)
  if(nr != 2 || nc != 2){
    cli::cli_abort("The bbox should be a 2 X 2 matrix, but got a {nr} X {nc} matrix. Something is wrong!")
  }
  
  # First row is the bottom left corner
  # Second row is the top right corner
  out <- apply(out, 2, sort, simplify = TRUE)
  
  class(out) <- c("bbox", "matrix", "array")
  return(round(out, digits = 0))
}

# Parse coco annotation format segmentation coordinates
parse_polygon_vec <- function(x){
  foo <- function(x){
    if(is.null(x)){
      return(NULL)
    }
    poly1 <- split_n_steps(x, n = 2L, name = c("x","y"))
    poly1 <- unique(poly1)
    poly1 <- validate_polygon(poly1)
    return(poly1)
  }
  
  
  if(is.list(x)){
    # A hack right now. Need to rebuild the polygon class to allow for multiple parts
    index <- purrr::map_depth(x,1, length) %>% unlist() %>% which.max()
    out <- foo(x[[index]])
  } else {
    out <- foo(x)
  }
  
  return(out)
}

# Parse model info
parse_inference_info <- function(x){
  if(is.null(x) || is.na(x)){
    model_version <- NA
    model_inference_timestamp <- NA
  } else {
    model_version <- gsub("__.*", "", x)
    model_inference_timestamp <- gsub(".*__", "", x)
  }
  
  c("version" = model_version, "inf_time" = model_inference_timestamp)
}

import_raw_inference <- function(path_meta, path_inference){
  df_inference <- suppressMessages(read_csv(path_inference, progress = FALSE))
  df_meta <- suppressMessages(read_csv(path_meta, progress = FALSE))
  df_meta$file_name <- gsub("\\\\","/", df_meta$file_name)
  df_inference$file_name <- gsub("\\\\","/", df_inference$file_name)
  out <- list(
    "meta" = df_meta,
    "inference" = df_inference
  )
  validate_inference(df_meta, df_inference)
  class(out) <- c("raw_inference", "list")
  return(out)
}



as.parsed_inference.raw_inference <- function(x){
  cli::cli_progress_step("Initiating", msg_done = "Initiation complete.", msg_failed = "Initiation failed.")
  
  stopifnot(is.raw_inference(x))
  
  cli::cli_progress_step("Formating image metadata", 
                         msg_done = "Image metadata formatting complete.", 
                         msg_failed = "Image metadata formatting failed.")
  img_meta <- .format_images_engine(x$meta, x$inference)
  
  img_meta <- img_meta %>% 
    cbind(
      vapply(x$meta$inference_info,parse_inference_info, FUN.VALUE = character(2), USE.NAMES = TRUE) %>% 
        t() %>% 
        `rownames<-`(NULL)
    )
  
  
  if("tag_id_guess" %in% names(x$meta)){
    img_meta$tag_id_guess <- parse_pylist(x$meta$tag_id_guess, as.character, simplify = FALSE) %>% 
      lapply(parse_tag_id) %>% 
      do.call("c", .)
  }
  
  if("tag_text" %in% names(x$meta)){
    img_meta$tag_text <- parse_pylist(x$meta$tag_text, as.character, simplify = FALSE) %>% 
      lapply(parse_tag_text)
  }
  
  cli::cli_progress_step("Formating instance metadata and bbox", 
                         msg_done = "Instance metadata and bbox formatting complete.", 
                         msg_failed = "Instance metadata and bbox formatting failed.")
  
  x$inference <- x$inference %>% 
    dplyr::group_by(file_name) %>% 
    dplyr::mutate(instance_id = seq_along(file_name)) %>% 
    dplyr::ungroup()
  
  image_id <- assign_image_id(x$inference$file_name)
  # add up 0 padding to length 5
  id <- paste0("inst",as.numeric(gsub_element_wise("00000$", sprintf("%05d", x$inference$instance_id), image_id)))
  stopifnot(!any(duplicated(id)))
  image_id <- paste0("img",image_id)
  
  things <- as.list(x$inference$thing_class)
  
  master_list <- list(
    "instance_id" = id,
    "image_id" = image_id,
    "bbox" = parse_pylist(x$inference$bbox, simplify = FALSE) %>%
      lapply(parse_bbox_vec),
    "score" = as.list(x$inference$score),
    "thing_class" = things
  )
  
  if(has_mask(x$inference)){
    cli::cli_progress_step("Formating polygons", 
                           msg_done = "Polygon formatting complete.", 
                           msg_failed = "Polygon formatting failed.")
    master_list <- c(
      master_list,
      list(
        "polygon" = parse_pylist(x$inference$polygon, simplify = FALSE) %>%
          lapply(parse_polygon_vec)
      )
    )
  }
  if(has_keypoints(x$inference)){
    cli::cli_progress_step("Formating keypoints", 
                           msg_done = "Keypoint formatting complete.", 
                           msg_failed = "Keypoint formatting failed.")
    master_list <- c(
      master_list,
      list(
        "keypoints" = parse_pylist(x$inference$keypoints, simplify = FALSE) %>%
          lapply(parse_keypoints_vec) %>% 
          map2(things, function(x, y){
            if(is.null(x)){
              return(NULL)
            } else {
              rownames(x) <- match_keypoints(y)[[1]]
              return(x)
            }
          })
      )
    )
  }
  cli::cli_progress_step("Collecting parsed results", 
                         msg_done = "Parsed results collection complete.", 
                         msg_failed = "Parsed results collection failed.")
  
  master_list <- lapply( 
    seq_len(nrow(x$inference)),
    function(i){
      as.instance(map(master_list, i))
    }
  )
  
  names(master_list) <- id
  
  
  img_meta <- as_tibble(img_meta)
  
  img_id2 <- paste0("img",img_meta$id)
  img_meta$inlist <- lapply(img_id2, function(i) {
    index <- which(image_id %in% i)
    as.inlist(
      master_list[index]
    )
  }) %>% 
    setNames(img_id2)
  img_meta <- as.parsed_inference(img_meta)
  img_meta$empty_instance <- do.call("c", lapply(img_meta$inlist, is_empty_instance))
  img_meta$num_annotations <- ifelse(img_meta$empty_instance,0,img_meta$num_annotations)
  
  on.exit({
    cli::cli_progress_done()
    cli::cli_progress_cleanup()
  })
  
  return(img_meta)
}


as.parsed_inference.COCO_Json <- function(x, refind_bbox = TRUE){
  image_id <- assign_image_id(x$images$file_name)
  
  img_meta <- data.frame(
    "id" = image_id,
    "file_name" = x$images$file_name,
    "path" = x$images$path,
    "height" = x$images$height,
    "width" = x$images$width,
    "num_annotaitons" = x$images$num_annotations,
    "version" = "COCO_annotaiton_import",
    "inf_time" = NA_character_,
    "tag_id_guess" = NA_character_
  )
  img_meta$tag_text <- lapply(seq_len(nrow(img_meta)), function(x) character(0))
  
  image_id2 <- image_id[match(x$annotations$image_id,x$images$id)]
  image_id2 <- paste0("img",image_id2)
  
  
  master_list <- list(
    "instance_id" = x$annotations$id,
    "image_id" = image_id2,
    "bbox" = lapply(x$annotations$bbox, parse_bbox_vec),
    "score" = as.list(rep(NA, nrow(x$annotations))),
    "thing_class" = x$categories$name[match(x$annotations$category_id, x$categories$id)]
  )
  
  if("segmentation" %in% names(x$annotations)){
    master_list <- c(
      master_list,
      list(
        "polygon" = lapply(x$annotations$segmentation, parse_polygon_vec)
      )
    )
    if(refind_bbox){
      cli::cli_alert("bbox overwritten with new bbox estimated from polygon. To use the original bbox, set {.code refind_bbox = FALSE}")
      master_list$bbox <- lapply(master_list$polygon, as.bbox)
    }
  }
  
  if("keypoints" %in% names(x$annotations)){
    master_list <- c(
      master_list,
      list(
        "keypoints" = lapply(x$annotations$keypoints, parse_keypoints_vec)
      )
    )
  }
  master_list <- lapply( 
    seq_len(nrow(x$annotations)),
    function(i){
      as.instance(map(master_list, i))
    }
  )
  names(master_list) <- paste0("inst", x$annotations$id)
  img_meta <- as_tibble(img_meta)
  image_id <- paste0("img",image_id)
  img_meta$inlist <- lapply(image_id, function(i) {
    index <- which(image_id2 %in% i)
    as.inlist(
      master_list[index]
    )
  }) %>% 
    setNames(image_id)
  img_meta <- as.parsed_inference(img_meta)
  return(img_meta)
}


as.parsed_inference.data.frame <- function(x){
  x <- tibble::as_tibble(x)
  as.pararsed_inference(x)
}

as.parsed_inference.tbl_df <- function(x){
  class(x) <- c("parsed_inference", class(x))
  x
}


merge_parsed_inference <- function(mask_df, kp_df){
  mask_df_name <- deparse(substitute(mask_df))
  kp_df_name <- deparse(substitute(kp_df))
  if(!is.parsed_inference(mask_df)){
    cli::cli_abort("{.var {mask_df_name}} must be of class {.cls parsed_inference}")
  }
  
  if(!is.parsed_inference(kp_df)){
    cli::cli_abort("{.var {kp_df_name}} must be of class {.cls parsed_inference}")
  }
  
  assert_variable_in_df(mask_df, c("inlist", "id"))
  assert_variable_in_df(kp_df, c("inlist", "id"))
  
  n_og <- nrow(mask_df)
  n_kp <- nrow(kp_df)
  matched <- match(kp_df$id, mask_df$id)
  matched <- matched[!is.na(matched)]
  n_new <- length(matched)
  
  cli::cli_alert_info("{n_new} of {n_og} images in {.var {mask_df_name}} found in {.var {kp_df_name}} of {n_kp} images.")
  mask_df <- mask_df[matched, ,drop = FALSE]
  
  
  mask_inl <- mask_df$inlist
  kp_inl <- kp_df$inlist
  
  
  if(!all(do.call("c",lapply(mask_inl, is.inlist)))){
    cli::cli_abort("{.var {mask_df_name}} must have a column for a list of {.cls inlist}")
  }
  
  if(!all(do.call("c",lapply(kp_inl, is.inlist)))){
    cli::cli_abort("{.var {kp_df_name}} must have a column for a list of {.cls inlist}")
  }
  
  mask_df$inlist <- purrr::map2(mask_inl, kp_inl, function(x,y){
    c(x,y)
  })
  
  return(mask_df)
}


parse_tag_id <- function(x){
  x <- gsub("\\\\n|'| ", "", x)
  res <- x[grepl("[0-9]DCR[0-9]", x)]
  res <- unique(res)
  if(length(res) != 1){
    res <- NA_character_
  }
  return(res)
}


parse_tag_text <- function(x){
  x <- gsub("'| ", "", x)
  x <- gsub("\\\\n", " ", x)
  x <- stringr::str_trim(x)
  return(x)
}




