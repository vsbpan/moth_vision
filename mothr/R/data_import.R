# Imported function to covert URL to shorter sheet ID
as_sheetid <- function(x){
  googlesheets4:::as_id.sheets_id(x)
}

# Imported function to read google sheets
import_sheets <- function(url, sheet, ...){
  googlesheets4::range_read(url, sheet = sheet, col_names = TRUE, col_types = "c", na = "", ...)
}

reformat_tag_id <- function(x){
  x %>% 
    lapply(function(w){
      if(!grepl("-", w)){
        return(w)
      }
      v <- str_split(w, "-")[[1]]
      
      if(is.historic(w)){
        return(paste0(v[1], v[2], collapse = ""))
      } else {
        if(length(v) == 4){
          v[3] <- paste(v[3], v[4], sep = "")
        }
        paste0(c(v[2],v[1], v[3]), collapse = "") 
      }
    }) %>% 
    lapply(split_tag_id) %>% 
    do.call("c", .)
}

split_tag_id <- function(x, collapse = TRUE){
  f <- function(x, collapse){
    if(is.historic(x)){
      w <- strsplit(x,"WPC")[[1]]
      w <- w[w != ""]
      if(length(w) != 1){
        return(NA_character_)
      }
      res <- c("WPC", w)
    } else {
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
      if(!isTRUE(w[1] %in% c("2022", "2023", "2024", "2025", "2026", "2027", "2028", "2029", "2030"))){
        return(NA_character_)
      }
      res <- c(w[1], "DCR", w[2])
    }
    
    if(collapse){
      res <- paste0(res, collapse = "")
    }
    return(res)
  }
  
  res <- lapply(x, function(z,collapse){
    f(z, collapse = collapse)
  }, collapse = collapse)
  
  if(collapse){
    res <- do.call("c", res)
  }
    
  return(res)
}
