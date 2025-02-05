fetch_image_database <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                   "assets/image_database.csv", sep = "/")){
  database <- try(suppressMessages(read_csv(database_path, 
                                            progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    stop("Cannot find image registration database!")
  }
  database
}