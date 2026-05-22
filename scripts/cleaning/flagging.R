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
  .$tag_id %>% 
  reformat_tag_id() %>% 
  na.omit() %>% 
  as.character() -> valid_ids



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
  select(tag_id, path) %>% 
  arrange(tag_id); launch_photo(z$path[1]);launch_photo(z$path[2]);print(z$tag_id[1]);print(basename(z$path[1]))


z$path[1]
flag_verified_tag_id(z$path[2], "WPC19122")
flag_image_exclude(z$path[1])









