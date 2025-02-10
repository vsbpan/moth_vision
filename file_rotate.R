library(tidyverse)
vmisc::load_all2("mothr")

"C:/R_projects/mothz/coco-annotator/datasets/mothz_sample1/" %>% 
  list.files(full.names = TRUE) %>% 
  file_orientation()


"C:/R_projects/mothz/coco-annotator/datasets/mothz_sample1/" %>% 
  auto_rotate_dir()

debug(auto_rotate_dir)

auto_rotate_dir("C:/Users/vsbpa/Desktop/2024_08_14")

"C:/Users/vsbpa/Desktop/2024_08_14" %>% 
  list.files(full.names = TRUE) %>% 
  file_orientation()
