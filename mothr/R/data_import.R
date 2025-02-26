# Imported function to covert URL to shorter sheet ID
as_sheetid <- function(x){
  googlesheets4:::as_id.sheets_id(x)
}

# Imported function to read google sheets
import_sheets <- function(url, sheet, ...){
  googlesheets4::range_read(url, sheet = sheet, col_names = TRUE, col_types = "c", na = "", ...)
}

reformat_tag_id <- function(x){
  str_split(x, "-") %>% 
    lapply(function(x){
      paste0(c(x[2],x[1], x[3]), collapse = "")
    }) %>% 
    do.call("c", .)
}