# Vectorized function that parse lists from python
parse_pylist <- function(x, coerce = as.numeric, simplify = TRUE){
  x <- gsub("c\\(|\\)","",x)
  out <- gsub("\\[|\\]","",x) %>% 
    str_split(",") %>% 
    lapply(function(x){
      coerce(x)
    })
  
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
  out <- split_n_steps(x, 3L, name = c("x","y","score"))
  class(out) <- c("keypoints", "matrix", "array")
  return(out)
}

# Turn vector of bbox values into a matrix
parse_bbox_vec <- function(x){
  # First row is the top left corner
  # Second row is the bottom right corner
  out <- split_n_steps(x, 2L, name = c("x","y"))
  class(out) <- c("bbox", "matrix", "array")
  return(out)
}

# Parse coco annotation format segmentation coordinates
parse_polygon_vec <- function(x){
  out <- split_n_steps(x, n = 2L, name = c("x","y"))
  out <- unique(out)
  out <- validate_polygon(out)
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


as.instance.list <- function(x){
  stopifnot(is.list(x))
  # Maybe add more stuff here
  class(x) <- c("instance", "list")
  x
}

as.inlist.list <- function(x){
  stopifnot(is.list(x))
  test <- vapply(x, is.instance, logical(1))
  if(any(!test)){
    o <- unique(do.all("c", lapply(x[!test], class)))
    cli::cli_abort("An object of class {.cls inlist} must be a list of {.cls instance} objects. Offending classes: {.cls o}")
  }
  class(x) <- c("inlist", "list")
  x
}

c.inlist <- function(x, ...){
  args <- lapply(as.list(substitute(list(...)))[-1L], 
                 function(x){
                   eval(x, envir = parent.frame(3))
                 })
  class(x) <- c("list")
  out <- c(x, unlist(args, recursive = FALSE))
  class(out) <- c("inlist", "list")
  return(out)
}

inlist <- function(x, ...){
  args <- lapply(as.list(substitute(list(...)))[-1L], 
                 function(x){
                   eval(x, envir = parent.frame(3))
                 })
  class(x) <- c("list")
  out <- list(x, unlist(args, recursive = FALSE))
  class(out) <- c("inlist", "list")
  return(out)
}

find_labels <- function(x){
  base::intersect(c("bbox","polygon","keypoints"), names(x))
}


as.parsed_inference.raw_inference <- function(x){
  stopifnot(is.raw_inference(x))
  img_meta <- mothzer:::.format_images_engine(x$meta, x$inference)
  
  img_meta <- img_meta %>% 
    cbind(
      vapply(x$meta$inference_info,parse_inference_info, FUN.VALUE = character(2), USE.NAMES = TRUE) %>% 
        t() %>% 
        `rownames<-`(NULL)
    )
  
  x$inference <- x$inference %>% 
    group_by(file_name) %>% 
    mutate(instance_id = seq_along(file_name)) %>% 
    ungroup()
  
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
    master_list <- c(
      master_list,
      list(
        "polygon" = parse_pylist(x$inference$polygon, simplify = FALSE) %>%
          lapply(parse_polygon_vec)
      )
    )
  }
  if(has_keypoints(x$inference)){
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
  
  return(img_meta)
}


