# Imported function to covert URL to shorter sheet ID
as_sheetid <- function(x){
  googlesheets4:::as_id.sheets_id(x)
}

# Imported function to read google sheets
import_sheets <- function(url, sheet, ...){
  googlesheets4::range_read(url, sheet = sheet, col_names = TRUE, col_types = "c", na = "", ...)
}

reformat_tag_id <- function(x){
  warning("Currently does not accomedate WPC codes. Proceed with caution!")
  str_split(x, "-") %>% 
    lapply(function(x){
      if(length(x) == 4){
        x[3] <- paste(x[3], x[4], sep = "")
      }
      paste0(c(x[2],x[1], x[3]), collapse = "")
    }) %>% 
    lapply(split_tag_id) %>% 
    do.call("c", .)
}

split_tag_id <- function(x, collapse = TRUE){
  warning("Currently does not accomedate WPC codes. Proceed with caution!")
  f <- function(x, collapse){
    w <- strsplit(x,"DCR")[[1]]
    if(length(w) != 2){
      return(NA_character_)
    }
    if(isTRUE(base::startsWith(w[2],"C"))){
      w[2] <- paste0(
        "C", 
        as.numeric(gsub("[A-Z]","",w[2]))
      )
    } else {
      w[2] <- tryCatch(as.numeric(w[2]),
                       warning = function(e){
                         NA_real_
                       })
      if(is.na(w[2])){
        return(NA_character_)
      }
    }
    res <- c(w[1], "DCR", w[2])
    if(collapse){
      res <- paste0(res, collapse = "")
    }
    return(res)
  }
  lapply(x, function(z,collapse){
    f(z, collapse = collapse)
  },collapse = collapse) %>% 
    do.call("c", .)
}
