#### Purpose ####
# Installing packages and dependencies for this project

# Run this only when you first start working on this or when you need to update
# packages. This is just to help you get set up.

# Some custom helper funs 
parse_dependencies <- function(description){
  l <- list(
    "imports" = strsplit(description$Imports, split = ",\n")[[1]],
    "depends" = strsplit(description$Depends, split = ",\n")[[1]],
    "suggests" = strsplit(description$Suggests, split = ",\n")[[1]]
  )
  
  l <- lapply(l, function(x){
    res <- gsub("\\(.*| ","",x)
    res[!res %in% "R"]
  })
  return(l)
}


check_installation_needed <- function(x, install = TRUE){
  owned_pkgs <- unname(installed.packages()[,1])
  x <- unlist(x)
  out <- x[!x %in% owned_pkgs]
  if(length(out) == 0){
    message("You are all set!")
    return(invisible())
  } else {
    if(install){
      message(sprintf("Trying to install missing packages: %s", 
                      paste0(out, collapse = ", ")))
      
      if("herbivar" %in% out){
        out <- out[!c("herbivar") %in% out]
        message("You do not have 'herbivar' installed. Follow instructions from the installation section of the package repository (https://github.com/vsbpan/herbivar) to install from GitHub.")
      }
      
      if("vmisc" %in% out){
        out <- out[!c("vmisc") %in% out]
        message("You do not have 'vmisc' installed. Follow instructions from the installation section of the package repository (https://github.com/vsbpan/vmisc) to install from GitHub.")
      }
      
      if("rtrees" %in% out){
        out <- out[!c("rtrees") %in% out]
        message("You do not have 'rtrees' installed. Follow instructions from the installation section of the package repository (https://github.com/daijiang/rtrees) to install from GitHub.")
      }
      
      install.packages(out)
      return(invisible())
    } else {
      return(out)
    }
  }
}

# Read dependencies in the simulated package
pkg_discription <- packageDescription("mothr", lib.loc = ".")
check_installation_needed(parse_dependencies(pkg_discription), install = TRUE)
