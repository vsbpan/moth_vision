vmisc::load_all2("mothr")


parsed_full <- readRDS("cleaned_data/parsed_full.rds")


# Drop mini-moths with known problems
parsed_full <- parsed_full %>% 
  filter(
    !file_name %in% (fetch_inference_error() %>% unlist(FALSE, FALSE))
  )






parsed_full$inlist[[1]] %>% 
  moth_bbox()

parsed_full$inlist[[1]]

parsed_full$inlist[[1]] %>% 
  as_relative() %>% 
  plot()


parsed_full$file_name[1] %>% 
  mini_moth_path(root_path = "D:/") %>% 
  fast_load_image() %>% 
  plot()









