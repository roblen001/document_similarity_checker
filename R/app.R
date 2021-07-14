
#' @title runApp
#'
#' @description A function for running the ONDRI Proposal Similarity Checker ShinyApp.
#' @author Roberto Lentini
#'
#' @import shiny shinyFiles shinyjs shinycssloaders rmarkdown tidytext tidyverse
#' @import pluralize data.table
#' @export
#'


runApp <- function(options = list()){
  shiny::addResourcePath("www", system.file("www", package = "ProposalSimilarityChecker"))

  ui <- shiny::fluidPage(
    style='background-color:  #F5F5F5; height: 100vh;',
    # Create title with logo.
    shiny::titlePanel(title = shiny::tags$div(
      shiny::img(style="width: 278px; height: 100px;",
                 src = "www/ONDRI_full-logo_web.png"),
      "ONDRI Proposal Checker Application"),
      windowTitle = "ONDRI Proposal Similarity Checker Application"),

    frontend()

  )

  # Define server logic (back end).
  server <- function(input, output, session) {

    session$onSessionEnded(stopApp)
    backend(input, output, session)

  }

    shiny::shinyApp(ui = ui,
                    server = server,
                    options = options)

    # # Run the application.
    # shiny::shinyApp(ui, server, options = list(display.mode = "normal", launch.browser = TRUE))
}



