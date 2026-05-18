vmisc::load_all2("mothr")

d <- import_raw_inference(path_meta = "inference/full_mothz_sample1_image_meta_mask.csv", 
                          path_inference = "inference/full_mothz_sample1_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "inference/full_mothz_sample1_image_meta_keypoint.csv", 
                           path_inference = "inference/full_mothz_sample1_inference_keypoint.csv")

register_image_id(d$meta$file_name)


parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)

gt_mask <- as.parsed_inference(import_COCO("COCO_annotations/mothz_sample1_fullset_mask.json"))
gt_kp <- as.parsed_inference(import_COCO("COCO_annotations/mothz_sample1_fullset_keypoints.json"))


parsed_all <- merge_parsed_inference(parsed_mask, parsed_kp)
gt_all <- merge_parsed_inference(gt_mask, gt_kp)

mask_evaluator(parsed_all, gt_all)
bbox_evaluator(parsed_all, gt_all)
keypoint_evaluator(parsed_all, gt_all)
