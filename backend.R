# backend.R 
#
# Purpose: The back end of the application
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-20
#
# ========================================================================================
# source('helpers/getSimilarProposal.R')

# TODO: do i give people the ability to add keywords to the stop_words df

backend <- function(input, output, session){
  
  # Computer volumes depend on OS: Windows, Mac, or Linux.
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  shinyDirChoose(input, id = "dirProposals", roots = volumes,
                 restrictions = system.file(package = "base"))
  output$dirProposalsOutput <- renderText("No proposal directory selected.")
  
  shinyFileChoose(input, id = "proposalFile", roots = volumes, filetypes = c("pdf"))
  output$proposalFileOutput <- renderText("No proposal file selected.")
  
  
  # 1) Select folder containing the proposals
  observeEvent(
    input$dirProposals, {
      
      dirPath <- parseDirPath(volumes, input$dirProposals)
      
      # Reset output when selection of directory is canceled.
      output$dirProposalsOutput <- renderText("No directory selected.")
      
      # Otherwise if directory has been selected, print path of directory to screen.
      if (length(dirPath) > 0){
        # Update output by printing new directory path.
        output$dirProposalsOutput <- renderPrint(dirPath)
      }
    }
  )
  
  # 2) Proposal file selection.
  observeEvent(
    input$proposalFile, {
      proposalFileFilePath <- parseFilePaths(volumes, input$proposalFile)

      # Reset output when selection of transfer ID file is canceled.
      output$proposalFileOutput <- renderText("No transfer ID file selected.")

      # Otherwise if proposal file has been selected, print path of proposal file
      # to screen.
      if (length(proposalFileFilePath$datapath) > 0){
        # Update output by printing new proposal file path.
        output$proposalFileOutput <- renderText(proposalFileFilePath$datapath)
      }
    }
  )
  
  # submit button
  observeEvent(input$BeginCheck, {
    similar_proposal_df <- getSimilarProposal(dirPath = "proposals")
    print(similar_proposal_df)
    print('che')
  })

}