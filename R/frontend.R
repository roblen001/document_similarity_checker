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
  shiny::fluidPage(
    shinyalert::useShinyalert(),
    shinyjs::useShinyjs(),
    shiny::tags$h2('Selection Section'),
    # Sidebar panel for inputs
    shiny::fluidRow(
      shiny::column(
         style='height: 61vh;
                overflow-y: auto;
                background-color: white;
                border-top: 3px solid #0072B2;
                padding-top: 5vh',
          width = 6,

            # User Selects if they want to test using keywords on
            # proposal id
         shiny::selectInput(
              inputId = 'checkUsing', 'Check Using:',
              c('--Please Make A Selection--' = 'default',
                'Keywords' = 'Keywords',
                'Proposal Id' = 'ProposalId',
                'Background Information' = 'BackgroundInformation',
                'Produce a Similarity Report' = 'SimilarityReport')
            ),
          # Select file with the proposal to be checked
         shiny::conditionalPanel(
            condition = "input.checkUsing != 'default'",

            shinyFiles::shinyFilesButton(id = "DataframeProposalFile",
                             label = 'Click to select a proposal dataframe file',
                             title = 'Please select a proposal dataframe file',
                             multiple = FALSE,
                             icon = shiny::icon("file")
            ),

            shiny::verbatimTextOutput("DataframeProposalFileOutput", placeholder = TRUE),


            shiny::br(),
            shiny::conditionalPanel(condition = "input.checkUsing == 'ProposalId'",
                             # Text input where users put the id of the proposal to compare
                             shiny::textInput(inputId = 'proposalID', label = 'Input proposal id:',
                                       placeholder = 'input proposal id.'),
                             ),

          ),

          # Select file with the proposal to be checked
         shiny::conditionalPanel(
            condition = "input.checkUsing == 'Keywords'",

            shiny::p('Separate the keywords with commas for them to count
              as separate words'),

            shiny::textAreaInput(inputId = "keywordsList", label = "Input Keywords:",
                          width = "1000px", height="200px",
                          placeholder = "ex: Deep Learning,ai, ONDRI Neuropsychology Platform
                          "),

          ),

         # check using a similarity report
         shiny::conditionalPanel(
           condition = "input.checkUsing == 'SimilarityReport'",

           shinyFiles::shinyDirButton(id = "DownloadLocationReport",
                                        label = 'Click to select the location of "Proposal Similarity Checker" folder.',
                                        title = 'Please select the location of the "Proposal Similarity Checker" folder.',
                                        multiple = FALSE,
                                        icon = shiny::icon("file")
           ),

           shiny::verbatimTextOutput("DownloadLocationReportFile", placeholder = TRUE),

           shiny::p('Using this checking method will produce a pdf report of newly
             submitted proposals against approved proposals'),
                          ),

         # check using text area
         shiny::conditionalPanel(
           condition = "input.checkUsing == 'BackgroundInformation'",

           shiny::p('Input proposal background information into the text box'),

           shiny::textAreaInput(inputId = "backgroundInformation", label = "Input Background Information:",
                         width = "1000px", height="200px"),

         ),



         shiny::conditionalPanel("input.checkUsing != 'default'",
                          # Action button will only be showed once and input selection is made
                          shiny::actionButton(inputId = "BeginCheck",
                                       label = "Begin Check",
                                       style = "color: white; background-color: #81D3EA;"),
                          ),
        ),



      shiny::column(
      style='height: 61vh;',
      width = 6,

      # Main panel for displaying outputs
      shinyjs::hidden(
        shiny::div(
                           id = "loading_page",
                           shinycssloaders::withSpinner(shiny::tableOutput('tb'), type = 4)
                         )
                           ),
                       # pdf information container
      shinyjs::hidden(
        shiny::div(id='main_content',
                             style='height: 100%;
                                    width: 100%;',
                   shiny::conditionalPanel(condition = "input.checkUsing != 'SimilarityReport'",
                                           shiny::tags$h3('Most Similar Proposal Information:')
                                              ),
                             # for keywords check other proposals
                   shiny::conditionalPanel(condition = "input.checkUsing == 'Keywords'",
                                           shiny::tags$div(style='height: 50vh; border: 1px solid grey; overflow: scroll; background-color: white;',
                                                           shiny::uiOutput('proposalList')
                                              )
                                              ),
                             # for similarity report
                   shiny::conditionalPanel(condition = "input.checkUsing == 'SimilarityReport'",
                                           shiny::tags$div(style='height: 50vh; display: flex; justify-content: center; align-items: center;
                                                           display: flex; flex-direction: column',
                                                           # shiny::p('Data has been compiled you can now download the pdf file'),
                                                           shiny::tags$div(style = "display: flex; flex-direction: row; justify-content: space-evenly; align-items: center;
                                                                           width: 100%;",
                                                                           shiny::tags$h3("Preview Of Simlarity Report"),
                                                                           shiny::downloadButton('downloadReport'),
                                                                           ),
                                                           shiny::tags$div(style='height: 50vh; border: 1px solid grey; overflow: scroll; background-color: white;',
                                                                           shiny::uiOutput('similarityReportPreview')
                                                           )

                                              )
                             ),
                             # if not Keywords check
                   shiny::conditionalPanel(condition = "input.checkUsing != 'Keywords' & input.checkUsing != 'SimilarityReport'",
                                           shiny::tags$div(style='height: 100%;',
                                                           shiny::p('Proposal Title:'),
                                                           shiny::uiOutput("proposalTitle"),
                                                           shiny::p('Similarity Level:'),
                                                           shiny::uiOutput('similarityLevel'),

                                        # keyword output
                                        shiny::p('Common Keywords:'),
                                        shiny::tags$div(style='height: 30vh; border: 1px solid grey; overflow: scroll; background-color: white;',
                                                        shiny::uiOutput('KeywordsOtherProposal')
                                         )

                               ),
                             ),


                         )
                       )

    )
  )
)

}

# [END]
