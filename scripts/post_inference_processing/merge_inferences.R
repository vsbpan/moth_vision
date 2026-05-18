vmisc::load_all2("mothr")

list.files("inference", pattern = "all_batches/batch_[0-9][0-9]_image_meta_mask.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_image_meta_mask.csv")


list.files("inference", pattern = "all_batches/batch_[0-9][0-9]_inference_mask.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_inference_mask.csv")



list.files("inference", pattern = "all_batches/batch_[0-9][0-9]_image_meta_keypoint.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_image_meta_keypoint.csv")


list.files("inference", pattern = "all_batches/batch_[0-9][0-9]_inference_keypoint.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  do.call("rbind", .) %>% 
  write_csv("inference/all_batches_inference_keypoint.csv")



d <- import_raw_inference(path_meta = "inference/full_mothz_sample1_image_meta_mask.csv", 
                          path_inference = "inference/full_mothz_sample1_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "inference/full_mothz_sample1_image_meta_keypoint.csv", 
                           path_inference = "inference/full_mothz_sample1_inference_keypoint.csv")

parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)


parsed_full <- merge_parsed_inference(parsed_mask, parsed_kp)