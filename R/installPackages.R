#' @title installPackages
#'
#' @description A function for installing all required packages and dependencies.
#' @author Roberto Lentini
#'
#' @export
#'
installPackages <- function(){

  if (Sys.which(('pdflatex') == "")){
    install.packages('tinytex')
    tinytex::install_tinytex()
  }

  utils::install.packages(c("shiny","shinyFiles", "shinyjs", "shinycssloaders", "rmarkdown", "tidytext",
                     "tidyverse", "pluralize", "data.table", "dplyr", "tidyr", "tibble", "utils", "fs", "readr"))

}

# [END]
