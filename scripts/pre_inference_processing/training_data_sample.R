library(tidyverse)

origin_dir <- "D:/Moth Specimen Photos/"
has_files <- list.files("D:/Moth Specimen Photos/", recursive = TRUE, full.names = TRUE)
file_sample <- sample(has_files, 300)
new_dir <- "C:/R_projects/mothz/coco-annotator/datasets"


new_fn <- gsub("/","__subdir__",gsub(origin_dir,"",file_sample))

new_path <- paste(new_dir, new_fn, sep ="/")

file.copy(
  file_sample, new_path
)



