
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

