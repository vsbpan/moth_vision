d <- import_raw_inference(path_meta = "inference/allie_historic_image_meta_mask.csv", 
                          path_inference = "inference/allie_historic_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "inference/allie_historic_image_meta_keypoint.csv", 
                           path_inference = "inference/allie_historic_inference_keypoint.csv")


register_image_id(d$meta$file_name)

parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)


parsed_full <- merge_parsed_inference(parsed_mask, parsed_kp)



parsed_full %>% 
  update_tag_id()

