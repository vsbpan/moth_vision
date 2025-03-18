library(tidyverse)
vmisc::load_all2("mothr")


l <- import_COCO("mothz_sample1_fullset_keypoints.json")
img_path_root <- "C:/R_projects/mothz/coco-annotator/datasets/mothz_sample1/"
l <- set_new_path_COCO(l,img_path_root)

l
summary(l)

l_split <- split_COCO(l, test = 0.1, val = 0.05)


export_COCO(l_split$train, "coco_kp_train.json")
export_COCO(l_split$test, "coco_kp_test.json")
export_COCO(l_split$val, "coco_kp_val.json")




l <- import_COCO("mothz_sample1_fullset_mask.json")
img_path_root <- "C:/R_projects/mothz/coco-annotator/datasets/mothz_sample1/"
l <- set_new_path_COCO(l,img_path_root)

l
summary(l)

l_split <- split_COCO(l, test = 0.1, val = 0.05)


export_COCO(l_split$train, "coco_mask_train.json")
export_COCO(l_split$test, "coco_mask_test.json")
export_COCO(l_split$val, "coco_mask_val.json")
