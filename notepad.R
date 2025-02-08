vmisc::load_all2("mothzer")

d <- import_raw_inference(path_meta = "test_auto_annotation_image_meta_mask.csv", 
                          path_inference = "test_auto_annotation_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "test_auto_annotation_image_meta_keypoints.csv", 
                           path_inference = "test_auto_annotation_inference_keypoints.csv")

l <- import_COCO("mothz_sample1_masks.json")
l2 <- import_COCO("mothz_sample1_keypoints.json")


register_image_id(d$meta$file_name)


a$inlist$img100000$inst100008$polygon %>% as.data.frame() %>% shoelace_area()

a$inlist$img100000$inst100008$bbox %>% area()




a$inlist$img100000$inst100008$polygon %>% 
  area()


d$meta

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






