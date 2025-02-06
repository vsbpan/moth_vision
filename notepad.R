vmisc::load_all2("mothzer")


d <- read_csv("test_auto_annotation_inference_mask.csv")
d2 <- read_csv("test_auto_annotation_image_meta_mask.csv")
d3 <- read_csv("test_auto_annotation_inference_keypoints.csv")

l <- import_COCO("mothz_sample1_masks.json")
l2 <- import_COCO("mothz_sample1_keypoints.json")


register_image_id(d$file_name)



d <- import_raw_inference(path_meta = "test_auto_annotation_image_meta_mask.csv", 
                          path_inference = "test_auto_annotation_inference_mask.csv")


d2 <- import_raw_inference(path_meta = "test_auto_annotation_image_meta_keypoints.csv", 
                          path_inference = "test_auto_annotation_inference_keypoints.csv")



d$meta$inference_info %>% 

format_images_COCO(d$meta, d$inference)


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


a$inlist[[5]]$inst500001$polygon

b$inlist[[5]]$inst500001$bbox

b$inlist[[5]]$inst500001








