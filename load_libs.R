#' Automatic library load/install
#' 
#' Load a list of required packages with automatic installation if needed. 
#' 
#' @examples 
#' req_libs <- c("ggplot2", "dplyr")
#' load_libs(req_libs)
load_libs <- function(to_load) {
  for(lib in to_load){
    if (!lib %in% installed.packages()[, 1]) {
      message("\nInstalling ", lib)
      install.packages(pkgs = lib, repos="https://cran.rstudio.com", 
                       dependencies = TRUE)
    }
    message("\nLoading ", lib)
    library(lib, character.only = TRUE)
  }
  invisible(NULL)
}