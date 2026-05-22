vmisc::load_all2("mothr")
options(
  "database_path" = "D:"
)

parsed_full <- list.files("cleaned_data/all_batches/", full.names = TRUE, pattern = ".rds") %>% 
  lapply(function(x){
    readr::read_rds(x)
  }) %>% 
  do.call("rbind", .)


l <- readRDS("invisible/googlesheets_tables.rds")
l[!grepl("not_spread", names(l))] %>% 
  do.call("rbind.fill", .) %>% 
  filter(
    MONA %in% c("8170", "8169", "8196")
  ) %>% 
  dplyr::select(
    tag_id, date, MONA, location
  ) %>% 
  mutate(
    tag_id = reformat_tag_id(tag_id)
  ) %>% 
  filter(
    !is.na(tag_id)
  ) %>% 
  .$tag_id -> ids


basename(list.files(get_mini_moth_path(), recursive = TRUE)) %>% 
  gsub("mini_","img_", .) -> mini_moths_haves

parsed_full %>% 
  update_tag_id() %>% 
  mutate(
    tag_id = split_tag_id(tag_id)
  ) %>% 
  mutate(
    a = file_name %in% mini_moths_haves,
    b = tag_id %in% ids
  ) %>% 
  mutate(z = a &!b) %>% 
  .$z %>% 
  which() -> ids2


pb_par_lapply(
  ids2,
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
  }, cores = 3, inorder = FALSE, 
  parsed_full = parsed_full
)





