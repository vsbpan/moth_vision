parse_pytracks <- function(x){
  bboxes <- x$bboxes %>% 
    parse_pylist(simplify = FALSE) %>% 
    lapply(
      function(x){
        if(all(is.na(x))){
          return(list())
        }
        split(x, ceiling(seq_along(x) / 4)) %>% 
          lapply(parse_bbox_vec)
      }
    )
  
  track_ids <- x$track_id %>% 
    parse_pylist(simplify = FALSE)
  
  inl <- purrr::map(seq_along(x$frame), function(i){
    purrr::map(seq_along(bboxes[[i]]),function(j){
      list(
        "image_id" = paste0("frame",x$frame[i]),
        "instance_id" = track_ids[[i]][[j]],
        "thing_class" = "beetle",
        "bbox" = bboxes[[i]][[j]],
        "score" = NA) %>% 
        as.instance()
    }) %>% 
      as.inlist()
  })
  
  names(inl) <- paste0("frame",x$frame)
  inl <- lapply(inl, function(x){
    if(length(x) == 0){
      return(x)
    }
    names(x) <- lapply(x, function(x){
      x$instance_id
    }) %>% 
      do.call("c", .) %>% 
      paste0("inst", .)
    x
  })
  
  nanno <- lapply(inl, function(x){
    length(
      purrr::keep(x, purrr::negate(is_empty_instance))
    )
  }) %>% 
    do.call("c", .)
  
  res <- tibble(
    "image_id" = x$frame,
    "path" = x$path,
    "height" = x$height,
    "width" = x$width,
    "fps" = x$fps,
    "num_annotations" = nanno, 
    "inlist" =  inl
  )
  
  as.parsed_inference(res)
}

format_tracks <- function(x, min_detections = 100){
  cli::cli_alert("Removed beetles with fewer than {min_detections} detection{?s}.")
  tracks <- x$inlist %>% 
    lapply(function(w){
      lapply(w, function(y){
        bbox <- as.bbox(y)
        if(is.null(bbox)){
          return(NULL)
        }
        data.frame(t(centroid(bbox)), "id" = y$instance_id, "frame" = y$image_id)
      }) %>% 
        do.call("rbind", .)
    }) %>% 
    do.call("rbind", .)
  
  tracks %>% 
    dplyr::mutate(
      time = as.numeric(gsub("[a-z]","",frame)),
      beetle_id = paste0("beetle", id)
    ) %>% 
    dplyr::group_by(beetle_id) %>% 
    filter(dplyr::n() > min_detections) %>% 
    dplyr::mutate(
      new_beetle_id = paste0("beetle", dplyr::cur_group_id())
    )
}
