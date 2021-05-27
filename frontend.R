# frontend.R
#
# Purpose: The front end of the application
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-20
#
# ========================================================================================
 
# show keywords when getSimlarProposal() is done running
load_data <- function() {
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
  }

# Options for Spinner (shows while the data is loading)
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

frontend <- function(){
  fluidPage(
    useShinyjs(),
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
            # User Selects if they want to test vs prior proposals or prior research
            # selectInput(
            #   inputId = 'selectionType', 'Test Proposal Against:',
            #   c('--Please Make A Selection--' = 'default', 
            #     'Other Proposals' = 'OtherProposals', 
            #     'Published Research' = 'PublishedResearch')
            # ),
         
         # UI for input for "Published Research" selection
         # conditionalPanel("input.selectionType == 'PublishedResearch'",
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
            
         # ),
         # UI for selection input for other proposal selection type
         # conditionalPanel("input.selectionType == 'OtherProposals'",
                          # Select file with the proposal dataframe
                          shinyFilesButton(id = "proposalDataFrame",
                                           label = "Click to select a proposal dataframe file.",
                                           title = "Please select a proposal dataframe file.",
                                           multiple = FALSE,
                                           icon = icon("file")),
                          
                          verbatimTextOutput("proposalFileOutput", placeholder = TRUE),
                          
                          br(),
                          # ),
         # Action button will only be showed once and input selection is made
         # conditionalPanel("input.selectionType == 'PublishedResearch' | input.selectionType == 'OtherProposals'",
                          actionButton(inputId = "BeginCheck",
                                       label = "Begin Check",
                                       style = "color: white; background-color: #81D3EA;"),
                          # ),
        ),
      

    
    column(
      style='height: 61vh;',
      width = 6,
      # Main panel for displaying outputs
      hidden(
        div(
          id = "loading_page",
          withSpinner(tableOutput('tb'), type = 4)
        )
      ),
      # pdf information container
      hidden(
        div(id='main_content',
            style='height: 100%;
            width: 100%;',
            tags$h3('Most Similar Proposal Information:'),
                tags$div(style='height: 100%;',
                         p('Press to view paper:'),
                         actionButton(inputId ="openPDF", style="background-color: #81D3EA; width: 100%; display: flex;
                                  justify-content: flex-start; padding-top: 1%; padding-bottom: 1%; border-radius: 25px; margin-bottom: 10px",
                                      label = uiOutput("pdfFileName")
                         ),
                         p('Similarity meter:'),
                         div(
                           style='width: 100%; height: 40px; background-color: white;',
                           div(
                             style='height: 100%; width: 30%; background-color: yellow;'
                           )
                         ),
                         p('Common Keywords:'),
                         tags$div(style='height: 50%; border: 1px solid grey; overflow: scroll; background-color: white;',
                                  uiOutput('Keywords')
                         )
                ),
               
      )
      )
    )
  )
)
             
}

# [END]