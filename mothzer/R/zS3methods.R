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

as.polygon <- function(x, ...){
  UseMethod("as.polygon")
}

as.bbox <- function(x, ...){
  UseMethod("as.bbox")
}

area <- function(x, ...){
  UseMethod("area")
}

centroid <- function(x, ...){
  UseMethod("centroid")
}

IOU <- function(x, ...){
  UseMethod("IOU")
}

registerS3method(genname = "IOU", 
                 class = "polygon", 
                 method = IOU.polygon)

registerS3method(genname = "IOU", 
                 class = "pixset", 
                 method = IOU.pixset)

registerS3method(genname = "IOU", 
                 class = "bbox", 
                 method = IOU.bbox)

registerS3method(genname = "as.bbox", 
                 class = "polygon", 
                 method = as.bbox.polygon)

registerS3method(genname = "as.polygon", 
                 class = "pixset", 
                 method = as.polygon.pixset)

registerS3method(genname = "as.polygon", 
                 class = "bbox", 
                 method = as.polygon.bbox)

registerS3method(genname = "as.pixset", 
                 class = "polygon", 
                 method = as.pixset.polygon)

registerS3method(genname = "as.pixset", 
                 class = "bbox", 
                 method = as.pixset.bbox)

registerS3method(genname = "centroid", 
                 class = "bbox", 
                 method = centroid.bbox)

registerS3method(genname = "centroid", 
                 class = "polygon", 
                 method = centroid.polygon)

registerS3method(genname = "centroid", 
                 class = "pixset", 
                 method = centroid.pixset)

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

registerS3method(genname = "[", 
                 class = "inlist", 
                 method = `[.inlist`)

registerS3method(genname = "as.parsed_inference", 
                 class = "data.frame", 
                 method = as.parsed_inference.data.frame)

registerS3method(genname = "as.parsed_inference", 
                 class = "tbl_df", 
                 method = as.parsed_inference.tbl_df)

registerS3method(genname = "as.parsed_inference", 
                 class = "raw_inference", 
                 method = as.parsed_inference.raw_inference)

registerS3method(genname = "as.parsed_inference", 
                 class = "COCO_Json", 
                 method = as.parsed_inference.COCO_Json)

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
                 class = "bbox", 
                 method = plot.bbox)

registerS3method(genname = "plot", 
                 class = "polygon", 
                 method = plot.polygon)

registerS3method(genname = "plot", 
                 class = "keypoints", 
                 method = plot.keypoints)

registerS3method(genname = "plot", 
                 class = "instance", 
                 method = plot.instance)

registerS3method(genname = "plot", 
                 class = "inlist", 
                 method = plot.inlist)

registerS3method(genname = "plot", 
                 class = "imlist", 
                 method = plot.imlist)

registerS3method(genname = "plot", 
                 class = "cimg", 
                 method = plot.cimg)

registerS3method(genname = "plot", 
                 class = "pixset", 
                 method = plot.pixset)


