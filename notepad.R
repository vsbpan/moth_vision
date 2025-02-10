vmisc::load_all2("mothzer")

d <- import_raw_inference(path_meta = "inference/full_mothz_sample1_image_meta_mask.csv", 
                          path_inference = "inference/full_mothz_sample1_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "inference/full_mothz_sample1_image_meta_keypoint.csv", 
                           path_inference = "inference/full_mothz_sample1_inference_keypoint.csv")

l <- import_COCO("COCO_annotations/mothz_sample1_fullset_mask.json")
l2 <- import_COCO("COCO_annotations/mothz_sample1_fullset_keypoints.json")


register_image_id(d$meta$file_name)


parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)

plot(imfill(dim = c(1500, 1000, 1, 1)))

c(
  parsed_mask$inlist$img100000,
  parsed_kp$inlist$img100000
) %>% 
  plot(shrink = 4)


parsed_full <- merge_parsed_inference(parsed_mask, parsed_kp)


plot(imfill(dim = c(1500, 1000, 1, 1)))
plot(parsed_full$inlist[[6]], shrink = 4)

parsed_full$inlist[[5]]
parsed_full$tag_id_guess






