fetch_image_database <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                   "assets/image_database.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                            progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find image registration database! Expected path: {.file database_path}.")
  }
  database
}

fetch_exclude_flags <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                       "assets/exclude_flags.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                            progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find exclude flags database! Expected path: {.file database_path}.")
  }
  database
}