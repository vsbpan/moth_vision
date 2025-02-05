# Mask area generic
mask_area <- function(x, ...){
  UseMethod("mask_area")
}
# Generic method
bbox_area <- function(x, ...){
  UseMethod("bbox_area")
}
as.Json <- function(x, ...){
  UseMethod("as.Json")
}

# Turn something into data_dict
as.data_dict <- function(x, ...){
  UseMethod("as.data_dict")
}


# registerS3method(genname = "c", 
#                  class = "data_dict", 
#                  method = c.data_dict)
# 
# registerS3method(genname = "as.data_dict", 
#                  class = "COCO_Json", 
#                  method = as.data_dict.COCO_Json)
# 
# registerS3method(genname = "summary", 
#                  class = "data_dict", 
#                  method = summary.data_dict)
# 
# registerS3method(genname = "print", 
#                  class = "data_dict", 
#                  method = print.data_dict)
# 
# registerS3method(genname = "[", 
#                  class = "data_dict", 
#                  method = `[.data_dict`)
# 
registerS3method(genname = "mask_area", 
                 class = "default", 
                 method = mask_area.default)

registerS3method(genname = "mask_area", 
                 class = "data_dict", 
                 method = mask_area.data_dict)

registerS3method(genname = "bbox_area", 
                 class = "default", 
                 method = bbox_area.default)

registerS3method(genname = "bbox_area", 
                 class = "data_dict", 
                 method = bbox_area.data_dict)

registerS3method(genname = "as.Json", 
                 class = "list", 
                 method = as.Json.list)

registerS3method(genname = "as.Json", 
                 class = "raw_inference", 
                 method = as.Json.raw_inference)

# registerS3method(genname = "as.Json", 
#                  class = "data_dict", 
#                  method = as.Json.data_dict)

registerS3method(genname = "print", 
                 class = "raw_inference", 
                 method = print.raw_inference)

registerS3method(genname = "print", 
                 class = "COCO_Json", 
                 method = print.COCO_Json)

registerS3method(genname = "plot", 
                 class = "imlist", 
                 method = plot.imlist)

registerS3method(genname = "plot", 
                 class = "cimg", 
                 method = plot.cimg)

registerS3method(genname = "plot", 
                 class = "pixset", 
                 method = plot.pixset)


