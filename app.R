library(shiny)
library(shinyFiles)
library(shinyjs)
library(shinycssloaders)

source('frontend.R')
source('backend.R')

# FUNCTIONS THAT ARE NOT BEING USED IN THIS VERSION OF THE APP:
# These fuctions can be used to check similar pdf files
# checkWithKeyWords_pubResearch()
# getPDFContent()
# getSimilarProposal()

ui <- fluidPage(
                style='background-color: #F5F5F5; height: 100vh',
                # Create title with logo.
                titlePanel(title = tags$div(
                  img(style="width: 278px; height: 100px;", 
                      src = "ONDRI_full-logo_web.png"), 
                  "ONDRI Proposal Checker Application"),
                  windowTitle = "www/ONDRI Proposal Checker Application"),
                
                frontend()
                
)
                
# logic for backend
server <- function(input, output, session) {
  backend(input, output, session)
}

shinyApp(ui, server)

# [END]
