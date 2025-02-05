library(tidyverse)
source("mothzer/COCO_utils.R")
l <- import_COCO("mothz_sample1_keypoints.json")
img_path_root <- "C:/R_projects/mothz/coco-annotator/datasets/mothz_sample1/"
l <- set_new_path(l,img_path_root)

l_split <- split_COCO(l, test = 0.1)


export_COCO(l_split$train, "coco_train.json")
export_COCO(l_split$test, "coco_test.json")


