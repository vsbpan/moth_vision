vmisc::load_all2("mothzer")

d <- import_raw_inference(path_meta = "full_mothz_sample1_image_meta_mask.csv", 
                          path_inference = "full_mothz_sample1_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "full_mothz_sample1_image_meta_keypoint.csv", 
                           path_inference = "full_mothz_sample1_inference_keypoint.csv")

l <- import_COCO("mothz_sample1_masks.json")
l2 <- import_COCO("mothz_sample1_keypoints.json")


register_image_id(d$meta$file_name)


a <- as.parsed_inference(d)
b <- as.parsed_inference(d2)

c(b$inlist$img100000, a$inlist$img100000)

a$inlist$img100000

c.inlist(b$inlist$img100000$inst100001, b$inlist$img100000$inst100002) %>% class()


inlist(
  b$inlist$img100000$inst100002, 
  b$inlist$img100000$inst100001
)




p1 <- a$inlist[[7]]$inst700004$polygon
p2 <- a$inlist[[6]]$inst600004$polygon

polygon_IOU(p1, p2)


b$inlist[[5]]$inst500001$bbox %>% plot()
b$inlist[[5]]$inst500001$bbox %>% area()

b$inlist[[5]]$inst500001$polygon %>% area()


b$inlist[[5]]$inst500001$polygon 
b$inlist[[5]]$inst500001$bbox %>% as.polygon()



a$inlist$img100000



prediction = a$inlist$img200000
ground_truth = a$inlist$img100000

mask_evaluator_engine(prediction, ground_truth, categories = c("forewing"))

keypoint_evaluator_engine(
  b$inlist$img100000,
  b$inlist$img300000
)

a$inlist[1:5]


mask_evaluator(
  a$inlist[1:5],
  a$inlist[6:10], 
  categories = "tag"
)


keypoint_evaluator(
  b$inlist[2:5],
  b$inlist[3:6], 
  thresh = seq(0.1, 0.9, by = 0.1)
)



mask_evaluator(
  a$inlist[2],
  a$inlist[3], 
  thresh = seq(0.1, 0.9, by = 0.1)
)


debug(mask_evaluator_engine)




