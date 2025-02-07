# # Mask area generic
# mask_area <- function(x, ...){
#   UseMethod("mask_area")
# }
# # Generic method
# bbox_area <- function(x, ...){
#   UseMethod("bbox_area")
# }
as.Json <- function(x, ...){
  UseMethod("as.Json")
}

as.inlist <- function(x, ...){
  UseMethod("as.inlist")
}

as.instance <- function(x, ...){
  UseMethod("as.instance")
}

as.parsed_inference <- function(x, ...){
  UseMethod("as.parsed_inference")
}

area <- function(x, ...){
  UseMethod("area")
}


registerS3method(genname = "area", 
                 class = "bbox", 
                 method = area.bbox)

registerS3method(genname = "area", 
                 class = "polygon", 
                 method = area.polygon)

registerS3method(genname = "area", 
                 class = "pixset", 
                 method = area.pixset)

registerS3method(genname = "c", 
                 class = "inlist", 
                 method = c.inlist)

registerS3method(genname = "as.parsed_inference", 
                 class = "raw_inference", 
                 method = as.parsed_inference.raw_inference)

registerS3method(genname = "as.instance", 
                 class = "list", 
                 method = as.instance.list)

registerS3method(genname = "as.inlist", 
                 class = "list", 
                 method = as.inlist.list)

registerS3method(genname = "as.Json", 
                 class = "list", 
                 method = as.Json.list)

registerS3method(genname = "as.Json", 
                 class = "raw_inference", 
                 method = as.Json.raw_inference)

registerS3method(genname = "print", 
                 class = "bbox", 
                 method = print.bbox)

registerS3method(genname = "print", 
                 class = "polygon", 
                 method = print.polygon)

registerS3method(genname = "print", 
                 class = "keypoints", 
                 method = print.keypoints)

registerS3method(genname = "print", 
                 class = "instance", 
                 method = print.instance)

registerS3method(genname = "print", 
                 class = "inlist", 
                 method = print.inlist)

registerS3method(genname = "print", 
                 class = "raw_inference", 
                 method = print.raw_inference)

registerS3method(genname = "print", 
                 class = "COCO_Json", 
                 method = print.COCO_Json)

registerS3method(genname = "summary", 
                 class = "COCO_Json", 
                 method = summary.COCO_Json)

registerS3method(genname = "plot", 
                 class = "imlist", 
                 method = plot.imlist)

registerS3method(genname = "plot", 
                 class = "cimg", 
                 method = plot.cimg)

registerS3method(genname = "plot", 
                 class = "pixset", 
                 method = plot.pixset)


