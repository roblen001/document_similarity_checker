#' @title installPackages
#'
#' @description A function for installing all required packages and dependencies.
#' @author Roberto Lentini
#'
#' @export
#'
installPackages <- function(){

  install.packages(c("shiny","shinyFiles", "shinyjs", "shinycssloaders", "rmarkdown", "tidytext",
                     "tidyverse", "pluralize", "data.table"))

}

# [END]
