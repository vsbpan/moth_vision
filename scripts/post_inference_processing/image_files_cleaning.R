vmisc::load_all2("mothr")
# testthat::compare_file_binary() # check for duplicated files


#### Rename and reorganize the photos ####

# files <- list.files("D:/Moth Specimen Photos/", pattern = ".JPG", full.names = TRUE, recursive = TRUE)
# 
# new_dir <- "D:/moth_specimen_photos_cleaned"
# 
# new_fn <- sprintf("img_moth_%08d.jpg", seq_along(files))
# 
# 
# new_dirs <- list.dirs(new_dir, recursive = FALSE)
# 
# for(i in cli::cli_progress_along(new_dirs, 
#                                  format = "Processing item {cli::pb_current} of {cli::pb_total} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}")){
#   indices <- (i - 1) * 2000 + 1:2000
#   indices <- indices[indices %in% seq_along(files)]
#   
#   new_path <- paste(new_dirs[i],new_fn[indices], sep = "/")
#   file.copy(files[indices], new_path)
# }


#### Check for duplicates ####

# Combinatorially hard, so can only apply to subsets
files <- list.files("D:/moth_specimen_photos_cleaned/batch_06", pattern = ".jpg", full.names = TRUE, recursive = TRUE)


d_grid <- combn(files, 2) %>% t() %>% as.data.frame()

dup_test <- pb_par_lapply(
  seq_len(nrow(d_grid)), 
  function(i, d_grid){
    f1 <- d_grid[i,1]
    f2 <- d_grid[i,2]
    res <- testthat::compare_file_binary(f1, f2)
    return(c("f1" = f1, "f2" = f2, "dup" = res))
  }, 
  d_grid = d_grid, 
  cores = 1, 
  inorder = FALSE)











