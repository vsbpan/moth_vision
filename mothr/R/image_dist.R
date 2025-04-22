fetch_masked_img <- function(path, inlist, things = c("forewing", "hindwing"), 
                             grayscale = FALSE, 
                             background = 0, 
                             separate_mask = FALSE){
  img <- fast_load_image(mini_moth_path(path))
  dim_xy <- dim(img)
  if(grayscale){
    img <- grayscale(img)
  }
  p <- as.pixset(select_things(as_relative(inlist), things), dim_xy)
  if(separate_mask){
    return(list("img" = img, "mask" = p))
  } else {
    return(immask(img, !p, background = background))
  }
  
}


fetch_masked_wing <- function(path, inlist, final_dim = c(99,149)){
  img <- fast_load_image(mini_moth_path(path))
  w <- select_side(as_relative(inlist)) %>% 
    select_things(c("forewing", "hindwing"))
  dim_xy <- dim(img)
  
  p <- as.pixset(w, dim_xy)
  img <- immask(img, !p, background = "black")
  bb <- moth_bbox(w, c("forewing", "hindwing"))
  img <- bbox_crop(img, bb)
  resize2target(img, final_dim)
}


img_dist <- function(model, img1, img2, n = 100){
  v1 <- predict(model, matrix(c(img1), nrow = 1))[1:n]
  v2 <- predict(model, matrix(c(img2), nrow = 1))[1:n]
  sqrt(mean((v1 - v2)^2))
}