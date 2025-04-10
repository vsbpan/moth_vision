launch_photo <- function(path){
  shell(sprintf("Open %s", path))
}

git_upload <- function(message = NULL){
  if(is.null(message)){
    cli::cli_abort("Please provide an informative message for the commit.")
  }
  shell("git add .")
  shell(sprintf("git commit -a -m %s", message))
  shell("git push")
}
