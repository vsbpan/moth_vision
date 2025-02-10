vmisc::load_all2("mothzer")

d <- import_raw_inference(path_meta = "full_mothz_sample1_image_meta_mask.csv", 
                          path_inference = "full_mothz_sample1_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "full_mothz_sample1_image_meta_keypoint.csv", 
                           path_inference = "full_mothz_sample1_inference_keypoint.csv")

register_image_id(d$meta$file_name)


parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)

gt_mask <- as.parsed_inference(import_COCO("coco_mask_test.json"))
gt_kp <- as.parsed_inference(import_COCO("coco_kp_test.json"))


mask_evaluator(parsed_mask, gt_mask)
bbox_evaluator(parsed_mask, gt_mask)
mask_evaluator(parsed_kp, gt_kp)
keypoint_evaluator(parsed_kp, gt_kp)
