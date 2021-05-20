# frontend.R
#
# Purpose: The front end of the application
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-20
#
# ========================================================================================

frontend <- function(){
  fluidPage(
    tags$h2('Selection Section'),
    # Sidebar panel for inputs
    fluidRow(
    column(   
       style='height: 61vh;
              overflow-y: auto;
              background-color: white;
              border-top: 3px solid #0072B2;
              padding-top: 5vh',
        width = 6,
          # Select directory with all the proposals
          shinyDirButton(id = "dirProposals", 
                         label = "Click to select proposal directory.",
                         title = "Please select a proposal folder.",
                         icon = icon("folder")),
          
          verbatimTextOutput("dirProposalsOutput", placeholder = TRUE),
          
          br(),
          
          # Select file with the proposal to be checked
          shinyFilesButton(id = "proposalFile",
                           label = "Click to select a proposal file.",
                           title = "Please select a proposal file.",
                           multiple = FALSE,
                           icon = icon("file")),
          
          verbatimTextOutput("proposalFileOutput", placeholder = TRUE),
          
          br(),
          
          actionButton(inputId = "BeginCheck",
                       label = "Begin Check",
                       style = "color: white; background-color: #81D3EA;"),
        ),
      
    
    # Main panel for displaying outputs 
    column(
      style='height: 61vh;
            overflow-y: auto;
            padding-left: 5vh;
            padding-right: 5vh',
      width = 6,
      # pdf information container
      fluidRow( style='height: 30%;',
                tags$h4('Most Similar Proposal Information:'),
                tags$div(style='background-color: white; height: 100%; border: 1px solid grey;')
      ),
      # keywords container
      fluidRow( style='height: 40%; margin-top: 5vh;',
                tags$h4('Common Keywords:'),
                tags$div(style='background-color: white; height: 100%; border: 1px solid grey;')
      ),
      )
    )
  )
  
             
}

# [END]