vmisc::load_all2("mothr")

list.files("inference", pattern = "batch_[0-9][0-9]_image_meta_mask.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_image_meta_mask.csv")


list.files("inference", pattern = "batch_[0-9][0-9]_inference_mask.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_inference_mask.csv")



list.files("inference", pattern = "batch_[0-9][0-9]_image_meta_keypoint.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_image_meta_keypoint.csv")


list.files("inference", pattern = "batch_[0-9][0-9]_inference_keypoint.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_inference_keypoint.csv")