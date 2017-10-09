#' Automatic library load/install
#' 
#' Load a list of required packages with automatic installation if needed. 
#' 
#' @examples 
#' need <- c("ggplot2", "dplyr")
#' load_packs(need)
load_packs <- function(to_load) {
  install_packs(to_load)
  sapply(to_load, library, character.only = TRUE)
  invisible(NULL)
}

#' Install packages if not installed alreayd
#' 
#' @examples 
#' need <- c("dplyr", "ggplot2")
#' install_packs(need)
install_packs <- function(required_packages) {
  status <- data.frame(
    package = required_packages,
    installed = required_packages %in% installed.packages()[, "Package"],
    stringsAsFactors = FALSE
  )
  
  if (all(status$installed)) {
    message("All required packages are installed.")
  } else {
    need <- status[!status$installed, "package"]
    message("Attempting to install ", length(need), " packages")
    for (x in need) {  
      tryCatch(install.packages(x, repos = repo, dependencies = TRUE))
    }
    status$installed <- status$package %in% installed.packages()[, "Package"]
    if (all(status$installed)) {
      message("All required packages are installed")
    } else {
      probs <- status[!status$installed, "package"]
      stop("Problems installing the following packages: ", paste(probs, collapse = ", "))
    }
  }
  invisible(NULL)
}
