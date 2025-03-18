vmisc::load_all2("mothr")

d <- import_raw_inference(path_meta = "inference/all_batches_image_meta_mask.csv", 
                          path_inference = "inference/all_batches_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "inference/all_batches_image_meta_keypoint.csv", 
                           path_inference = "inference/all_batches_inference_keypoint.csv")


register_image_id(d$meta$file_name)

parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)


parsed_full <- merge_parsed_inference(parsed_mask, parsed_kp)

# saveRDS(parsed_full, "cleaned_data/parsed_full.rds")

parsed_full <- readRDS("cleaned_data/parsed_full.rds")


plot(imfill(dim = c(1500, 1000, 1, 1)))
plot(parsed_full$inlist[[56]], shrink = 4, bbox = TRUE, bbox_moth = TRUE)



pb_par_lapply(
  5001:nrow(parsed_full),
  function(i, parsed_full){
    if(parsed_full[i, "empty_instance", drop = TRUE]){
      return(invisible(NULL))
    }
    
    tryCatch({
      path <- parsed_full[i, "path", drop = TRUE]
      new_fn <- gsub("img_moth","mini_moth",basename(path))
      write_path <- paste(get_mini_moth_path("D:"), new_fn,sep = "/")
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



flag_inference_error(paste0("mini_moth_00013371",".jpg"))

flag_image_exclude("img_moth_00013371.jpg")



mothr::set_verified_tag_id("img_moth_00006205.jpg", "2023DCR7838")


z <- parsed_full %>% 
  dplyr::select(tag_id_guess, id, file_name, tick_size, path) %>% 
  mutate(
    image_id = paste0("img", id),
    tag_id = tag_id_guess
  ) %>% 
  filter(
    !file_name %in% unlist(fetch_exclude_flags())
  ) %>% 
  mothr::update_tag_id() %>%
  mutate(
    tag_id = split_tag_id(tag_id)
  ) %>% 
  group_by(tag_id) %>% 
  mutate(
    n = n()
  ) %>% 
  filter(!is.na(tag_id)) %>% 
  filter(n > 1) %>% 
  select(path) %>% 
  arrange(tag_id); launch_photo(z$path[1]);launch_photo(z$path[2]);print(z$tag_id[1])

flag_image_exclude(z$path[1])


update_tag_id(parsed_full)




