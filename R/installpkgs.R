#' Function for Packages
#' 
#' To install and load multiple packages at once.
#' @param Want to install multiple packages ?, Defaults to pkg.
#' @keywords pkg
#' @export
#' @examples 
#' installpkgs()


installpkgs <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
