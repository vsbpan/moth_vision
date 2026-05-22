fetch_image_database <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                   "assets/image_database.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                            progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find image registration database! Expected path: {.file {database_path}}.")
  }
  database
}

fetch_exclude_flags <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                       "assets/exclude_flags.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                            progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find exclude flags database! Expected path: {.file {database_path}}.")
  }
  database
}


fetch_inference_error <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                      "assets/inference_error.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                                   progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find inference error database! Expected path: {.file {database_path}}.")
  }
  database
}

fetch_verified_tag_id <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                       "assets/real_tag_id.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                                   progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find verified tag id database! Expected path: {.file {database_path}}.")
  }
  
  test <- database %>% 
    dplyr::group_by(file_name) %>% 
    dplyr::summarise(n = vmisc::unique_len(tag_id)) %>% 
    dplyr::filter(n > 1)
  
  if(nrow(test) > 0L){
    cli::cli_abort("Detected {nrow(test)} instance{?s} where multiple unique {.var tag_id} are assigned to a single image. Something is wrong.")
  }
  
  database
}

fetch_verified_tick_size <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                        "assets/real_tick_size.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                                   progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find verified tick size database! Expected path: {.file {database_path}}.")
  }
  
  test <- database %>% 
    dplyr::group_by(file_name) %>% 
    dplyr::summarise(n = vmisc::unique_len(tick_size)) %>% 
    dplyr::filter(n > 1)
  
  if(nrow(test) > 0L){
    cli::cli_abort("Detected {nrow(test)} instance{?s} where multiple unique {.var tick_size} are assigned to a single image. Something is wrong.")
  }
  
  database
}

fetch_historic_flag <- function(database_path = paste(pkgload::pkg_path(vmisc::fake_pkg()), 
                                                      "assets/historic_specimen.csv", sep = "/")){
  database <- try(suppressMessages(readr::read_csv(database_path, 
                                                   progress = FALSE)))
  if(!isTRUE(is.data.frame(database))){
    cli::cli_abort("Cannot find historic flag database! Expected path: {.file {database_path}}.")
  }
  
  database
}


