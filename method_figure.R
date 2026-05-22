vmisc::load_all2("mothr")
options(
  "database_path" = "D:" # You might need to change
)
parsed_full <- list.files("cleaned_data/all_batches/", full.names = TRUE, pattern = ".rds") %>% 
  lapply(function(x){
    readr::read_rds(x)
  }) %>% 
  do.call("rbind", .)

# Pick any ol' photo
z <- parsed_full[11334,]
img <- z$path %>% 
  switch_root() %>% 
  fast_load_image() %>% 
  thin(4) # Reduce resolution by 4^2 times 


jpeg("graphs/method_figure.jpg", height = 8, width = 6, res = 600, quality = 99, units = "in")
par(mfrow = c(2,1), mar = c(0,0,0,0))
img %>% 
  plot(axes = FALSE)
img %>% 
  plot(axes = FALSE)
plot(z$inlist[[1]], shrink = 4)
dev.off()