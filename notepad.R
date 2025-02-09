vmisc::load_all2("mothzer")

d <- import_raw_inference(path_meta = "full_mothz_sample1_image_meta_mask.csv", 
                          path_inference = "full_mothz_sample1_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "full_mothz_sample1_image_meta_keypoint.csv", 
                           path_inference = "full_mothz_sample1_inference_keypoint.csv")

l <- import_COCO("mothz_sample1_fullset_mask.json")
l2 <- import_COCO("mothz_sample1_fullset_keypoints.json")


register_image_id(d$meta$file_name)


parsed_mask <- as.parsed_inference(d)
parsed_kp <- as.parsed_inference(d2)

gt_parsed <- as.parsed_inference(l)
gt_parsed <- as.parsed_inference(import_COCO("coco_mask_test.json"))

parsed_mask <- parsed_mask[match(gt_parsed$id, parsed_mask$id), ]



mask_evaluator(parsed_mask$inlist, gt_parsed$inlist)




