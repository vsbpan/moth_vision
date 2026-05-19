vmisc::load_all2("mothr")

parsed_full <- list.files("cleaned_data/all_batches/", full.names = TRUE, pattern = ".rds") %>% 
  lapply(function(x){
    readr::read_rds(x)
  }) %>% 
  do.call("rbind", .)



pb_par_lapply(
  1:nrow(parsed_full),
  function(i, parsed_full){
    if(parsed_full[i, "empty_instance", drop = TRUE]){
      return(invisible(NULL))
    }
    
    tryCatch({
      path <- parsed_full[i, "path", drop = TRUE]
      new_fn <- gsub("img_moth","mini_moth",basename(path))
      write_path <- paste(get_mini_moth_path("D:"),get_batch(new_fn), new_fn, sep = "/")
      img <- fast_load_image(path)
      img <- bbox_crop(img, moth_bbox(parsed_full[i, "inlist", drop = TRUE][[1]]))
      write_jpg(img, write_path)
    }, error = function(e){
      message(e$message)
      return(NULL)
    })
    return(invisible(NULL))
  }, cores = 4, inorder = FALSE, 
  parsed_full = parsed_full
)









