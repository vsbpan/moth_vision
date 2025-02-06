
match_category_id <- function(name){
  f <- function(name){
    switch(name,
           "body" = 1,
           "forewing" = 2,
           "hindwing" = 3,
           "color_checker" = 4, 
           "ruler" = 5,
           "tag" = 6)
  }
  vapply(name, f, numeric(1), USE.NAMES = FALSE)
}

match_supercategory <- function(name){
  f <- function(name){
    switch(name,
           "body" = "moth",
           "forewing" = "moth",
           "hindwing" = "moth",
           "color_checker" = "misc", 
           "ruler" = "misc",
           "tag" = "misc")
  }
  vapply(name, f, character(1), USE.NAMES = FALSE)
}

match_category_color <- function(name){
  f <- function(name){
    switch(name,
           "body" = "#a88625",
           "forewing" = "#8053dd",
           "hindwing" = "#17edc2",
           "color_checker" = "#f813de", 
           "ruler" = "#06ca9f",
           "tag" = "#2356fc")
  }
  vapply(name, f, character(1), USE.NAMES = FALSE)
}

match_keypoint_color <- function(name){
  f <- function(name){
    switch(name,
           "body" = character(0L),
           "forewing" = c("#bf5c4d", "#d99100"),
           "hindwing" = character(0L),
           "color_checker" = c("#bf5c4d", "#d99100", "#4d8068", "#0d2b80"), 
           "ruler" = character(0L),
           "tag" = character(0L))
  }
  lapply(name, f)
}

match_keypoints <- function(name){
  f <- function(name){
    switch(name,
           "body" = NULL,
           "forewing" = c("inner", "outer"),
           "hindwing" = NULL,
           "color_checker" = c("left_upper", "right_upper", "right_lower", "left_lower"), 
           "ruler" = NULL,
           "tag" = NULL)
  }
  lapply(name, f)
}

match_skeleton <- function(name){
  f <- function(name){
    switch(name,
           "body" = NULL,
           "forewing" = matrix(c(1,2), byrow = TRUE, ncol = 2),
           "hindwing" = NULL,
           "color_checker" = matrix(c(1,2,2,3,3,4), byrow = TRUE, ncol = 2), 
           "ruler" = NULL,
           "tag" = NULL)
  }
  lapply(name, f)
}

format_categories_COCO <- function(df){
  things <- unique(df$thing_class)
  
  res <- data.frame(
    "id" = match_category_id(things),
    "name" = things,
    "supercategory" = match_supercategory(things),
    "color" = match_category_color(things)
  )
  
  # Force data.frame to have list
  res$keypoint_colors <- match_keypoint_color(things)
  res$keypoints <- match_keypoints(things)
  res$skeleton <- match_skeleton(things)
  return(res)
}

.format_images_engine <- function(df_meta, df_inference){
  dim_list <- parse_pylist(df_meta$image_size, simplify = FALSE)
  empty_list_list <- lapply(seq_len(nrow(df_meta)), function(x) vector(mode = "list", length = 0))
  
  tally_d <- df_inference %>% 
    group_by(file_name) %>% 
    tally()
  
  # Sort tally_d by df_meta
  tally_d <- tally_d[match(tally_d$file_name,df_meta$file_name),]
  
  setdiff_vec <- base::setdiff(unique(df_inference$file_name),unique(df_meta$file_name))
  if(!isTRUE(length(setdiff_vec) == 0)){
    cli::cli_abort("Detected file name in {.var df_inference} that is not found in {.var df_meta}. Are you sure these are the right files?")
  }
  img_id <- assign_image_id(df_meta$file_name)
  
  images <- data.frame(
    "id" = img_id,
    "file_name" = basename(df_meta$file_name),
    "path" = df_meta$file_name,
    "height" = do.call("c", map(dim_list, 2)),
    "width" = do.call("c", map(dim_list, 1)),
    "num_annotations" = as.integer(tally_d$n)
  )
  return(images)
}


format_images_COCO <- function(df_meta, df_inference){
  images <- .format_images_engine(df_meta, df_inference)
  # images$annotated <- TRUE
  
  images$deleted <- FALSE
  images$regenerate_thumbnail <- TRUE
  
  images$category_ids <- empty_list_list
  images$events <- empty_list_list
  images$annotating <- empty_list_list
  
  return(images)
}


format_annotations_COCO <- function(df){
  df <- df %>% 
    group_by(file_name) %>% 
    mutate(instance_id = seq_along(file_name))
  img_id <- assign_image_id(df$file_name)
  thing_id <- match_category_id(df$thing_class)
  instance_id <- sprintf("%05d", df$instance_id) # add up 0 padding to length 5
  id <- as.integer(as.numeric(gsub_element_wise("00000$", instance_id, img_id)))
  
  annotations <- data.frame(
    "category_id" = thing_id,
    "id" = id,
    "image_id" = img_id,
    "iscrowd" = FALSE
  )
  
  annotations$bbox <- df$bbox %>% parse_pylist(simplify = FALSE) %>% lapply(round)
  
  if(has_mask(df)){
    seg <- parse_pylist(df$polygon, simplify = FALSE) %>% 
      lapply(function(x){
        z <- parse_polygon_vec(x)
        if(is.null(z)){
          return(
            vector(mode= "list", length = 0)
          )
        } else {
          return(
            list(as.vector(t(z)))
          )
        }
      })
    
    annotations$segmentation <- unname(seg)
  }
  
  
  if(has_keypoints(df)){
    kp <- parse_pylist(df$keypoints)
    kp_flag_index <- seq_len(ncol(kp))[seq_len(ncol(kp)) %% 3 == 0]
    kp[,kp_flag_index] <- 2 # overwrite score with flag
    kp <- apply(round(kp), 1, identity, simplify = FALSE)
    annotations$num_keypoints <- as.integer(ifelse(lapply(kp, is.null) %>%
                                                     do.call("c",.),
                                                   0,
                                                   length(kp_flag_index)))
    annotations$keypoints <- kp
  }
  return(annotations)
}


as.Json.raw_inference <- function(x){
  stopifnot(is.raw_inference(x))
  
  out <- list(
    "images" = format_images_COCO(df_meta = x$meta, df_inference = x$inference),
    "categories" = format_categories_COCO(x$inference),
    "annotations" = format_annotations_COCO(x$inference),
    "info" = list(),
    "licenses" = list()
  )
  
  class(out) <- c("COCO_Json", "list")
  
  return(out)
}



