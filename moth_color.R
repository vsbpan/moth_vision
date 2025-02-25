vmisc::load_all2("mothr")


parsed_full <- readRDS("cleaned_data/parsed_full.rds")


# Drop mini-moths with known problems
parsed_full <- parsed_full %>% 
  filter(
    !file_name %in% (fetch_inference_error() %>% unlist(FALSE, FALSE))
  )








parsed_full$file_name[1] %>% 
  mini_moth_path(root_path = "D:/") %>% 
  fast_load_image() -> img



parsed_full$inlist[[1]] %>% 
  as_relative() %>% 
  select_things(c("forewing","hindwing", "body")) %>% 
  lapply(function(x){
    img[!as.pixset(x$polygon,dim_xy = dim(img)[1:2])] <- NA_real_
    img
  }) %>% 
  as.imlist() %>% 
  plot()






















