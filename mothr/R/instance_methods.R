find_things.inlist <- function(x, ...){
  lapply(x, find_things) %>% do.call("c",.) %>% unique()
}

find_things.instance <- function(x, ...){
  x$thing_class
}


find_labels.instance <- function(x, ...){
  base::intersect(c("bbox","polygon","keypoints"), names(x))
}

find_labels.inlist <- function(x, ...){
  lapply(x, find_labels) %>% do.call("c",.) %>% unique()
}

select_things <- function(x, things = c("body", "forewing", "hindwing", "color_checker",
                                              "ruler", "tag")){
  things <- match.arg(things, several.ok = TRUE)
  
  cats <- do.call("c",purrr::map(x, "thing_class"))
  x[cats %in% things]
}


as.instance.list <- function(x){
  stopifnot(is.list(x))
  # Maybe add more stuff here
  class(x) <- c("instance", "list")
  x
}

as.inlist.list <- function(x){
  stopifnot(is.list(x))
  test <- vapply(x, is.instance, logical(1))
  if(any(!test)){
    o <- unique(do.all("c", lapply(x[!test], class)))
    cli::cli_abort("An object of class {.cls inlist} must be a list of {.cls instance} objects. Offending classes: {.cls o}")
  }
  class(x) <- c("inlist", "list")
  x
}

c.inlist <- function(x, ...){
  args <- lapply(as.list(substitute(list(...)))[-1L], 
                 function(x){
                   eval(x, envir = parent.frame(3))
                 })
  class(x) <- c("list")
  out <- c(x, unlist(args, recursive = FALSE))
  class(out) <- c("inlist", "list")
  return(out)
}

`[.inlist` <- function(x, ...){
  args <- lapply(as.list(substitute(list(...)))[-1L], 
                 function(x){
                   eval(x, envir = parent.frame(3))
                 })
  class(x) <- c("list")
  out <- do.call("[", c(list(x), args))
  class(out) <- c("inlist", "list")
  return(out)
}


inlist <- function(x, ...){
  args <- lapply(as.list(substitute(list(...)))[-1L], 
                 function(x){
                   eval(x, envir = parent.frame(3))
                 })
  out <- do.call("list", c(list(x), args))
  class(out) <- c("inlist", "list")
  return(out)
}


moth_bbox <- function(x, expected_things = c("body", "forewing", "hindwing"), ...){
  if(!is.inlist(x)){
    cli::cli_alert_warning("{.var x} should be an object of class {.cls inlist}. No garunetee the output makes sense.")
  }
  l <- select_things(x, things = expected_things)
  things <- find_things(l)
  
  missing_things <- expected_things[!expected_things %in% things]
  if(length(missing_things) > 0){
    cli::cli_alert_warning(c(
      "Missing {length(missing_things)} expected thing{?s} in {.var x}: {cli::col_blue(missing_things)}",
      cli::col_yellow("\nbbox might be wrong!")
    ))
  }
  
  return(as.bbox(l))
}

make_empty_instance <- function(image_id, labels = c("polygon", "keypoints")){
  inst <- list(
    "instance_id" = paste0("inst",gsub_element_wise("00000$", sprintf("%05d", 1), 
                                                    gsub("[a-z]", "",image_id ))),
    "image_id" = image_id,
    "bbox" = NULL,
    "score" = NA,
    "thing_class" = NA
  )
  if("polygon" %in% labels){
    inst <- c(inst, list("polygon" = NULL))
  }
  if("keypoints" %in% labels){
    inst <- c(inst, list("keypoints" = NULL))
  }
  as.instance.list(inst)
}


