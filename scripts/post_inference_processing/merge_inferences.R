vmisc::load_all2("mothr")

batches <- list.files("inference/all_batches", pattern = "batch_[0-9][0-9]_image_meta_mask", full.names = TRUE) %>% 
  basename() %>% 
  drop_extn() %>% 
  gsub("_image_meta_mask","",.)


lapply(batches, function(b){
  root <- "inference/all_batches/"
  kp_meta_path <- sprintf("%s%s_image_meta_keypoint.csv", root, b)
  kp_inf_path <- sprintf("%s%s_inference_keypoint.csv", root, b)
  msk_meta_path <- sprintf("%s%s_image_meta_mask.csv", root, b)
  msk_inf_path <- sprintf("%s%s_inference_mask.csv", root, b)
  pkp <- as.parsed_inference(import_raw_inference(kp_meta_path,kp_inf_path))
  pmsk <- as.parsed_inference(import_raw_inference(msk_meta_path,msk_inf_path))
  pmerged <- merge_parsed_inference(pmsk, pkp)
  readr::write_rds(pmerged, sprintf("cleaned_data/all_batches/%s_parsed_inference.rds", b), compress = "xz", compression = 9L)
  return(NULL)
})



