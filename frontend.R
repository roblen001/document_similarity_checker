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
         
            # User Selects if they want to test using keywords on
            # proposal id
            selectInput(
              inputId = 'checkUsing', 'Check Using:',
              c('--Please Make A Selection--' = 'default',
                'Keywords' = 'Keywords',
                'Proposal Id' = 'ProposalId',
                'Background Information' = 'BackgroundInformation',
                'Produce a Similarity Report' = 'SimilarityReport')
            ),
          # Select file with the proposal to be checked
          conditionalPanel(
            condition = "input.checkUsing != 'default'",
     
            shinyFilesButton(id = "DataframeProposalFile",
                             label = 'Click to select a proposal dataframe file',
                             title = 'Please select a proposal dataframe file',
                             multiple = FALSE,
                             icon = icon("file")
            ),
            
            verbatimTextOutput("DataframeProposalFileOutput", placeholder = TRUE),
            
            br(),
            conditionalPanel(condition = "input.checkUsing == 'ProposalId'",
                             # Text input where users put the id of the proposal to compare
                             textInput(inputId = 'proposalID', label = 'Input proposal id:',
                                       placeholder = 'input proposal id.'),
                             ),
            
          ),
          
          # Select file with the proposal to be checked
          conditionalPanel(
            condition = "input.checkUsing == 'Keywords'",
            
            p('Seperate the keywords with commas for them to count
              as seperate words'),
            
            textAreaInput(inputId = "keywordsList", label = "Input Keywords:", 
                          width = "1000px", height="200px",
                          placeholder = "ex: Deep Learning,ai, ONDRI Neuropsychology Platform 
                          "),
            
          ),
         
         # check using a similarity report
         conditionalPanel(
           condition = "input.checkUsing == 'SimilarityReport'",
           
           p('Using this checking method will produce a pdf report of newly
             submitted proposals against approved proposals'),
                          ),
         
         # check using text area
         conditionalPanel(
           condition = "input.checkUsing == 'BackgroundInformation'",

           p('Input proposal background information into the text box'),

           textAreaInput(inputId = "backgroundInformation", label = "Input Background Information:",
                         width = "1000px", height="200px"),

         ),


         
         conditionalPanel("input.checkUsing != 'default'",
                          # Action button will only be showed once and input selection is made
                          actionButton(inputId = "BeginCheck",
                                       label = "Begin Check",
                                       style = "color: white; background-color: #81D3EA;"),
                          ),
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
                             conditionalPanel(condition = "input.checkUsing != 'SimilarityReport'",
                                              tags$h3('Most Similar Proposal Information:')
                                              ),
                             # for keywords check other proposals
                             conditionalPanel(condition = "input.checkUsing == 'Keywords'",
                                              tags$div(style='height: 500px; border: 1px solid grey; overflow: scroll; background-color: white;',
                                                       uiOutput('proposalList')
                                              )
                                              ),
                             # for similarity report
                             conditionalPanel(condition = "input.checkUsing == 'SimilarityReport'",
                                              tags$div(style='height: 500px; display: flex; justify-content: center; align-items: center;',
                                                       p('data has been compiled you can now download the html file'),
                                                       helpText(),
                                                       selectInput('x', 'Build a regression model of mpg against:',
                                                                   choices = names(mtcars)[-1]),
                                                       radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                                                    inline = TRUE),
                                                       downloadButton('downloadReport')
                                                       
                                              )
                             ),
                             # if not Keywords check
                             conditionalPanel(condition = "input.checkUsing != 'Keywords' & input.checkUsing != 'SimilarityReport'",
                               tags$div(style='height: 100%;',
                                        p('Proposal Title:'),
                                        uiOutput("proposalTitle"),
                                        
                                        p('Similarity Level:'),
                                        uiOutput('similarityLevel'),
                                          
                                        # keyword output 
                                         p('Common Keywords:'),
                                         tags$div(style='height: 250px; border: 1px solid grey; overflow: scroll; background-color: white;',
                                                  uiOutput('KeywordsOtherProposal')
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