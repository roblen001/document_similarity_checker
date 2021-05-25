# backend.R 
#
# Purpose: The back end of the application
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-20
#
# ========================================================================================
source('helpers/getSimilarProposal.R')

# TODO: do I give people the ability to add keywords to the stop_words df
# This will take a certain row from similar articles dataframe and style it
getHTML <- function (pdfFileName) {
  if (nchar(pdfFileName) > 45){
    indexed <- substr(pdfFileName, start = 1, stop = 45)
    new_name <- paste(indexed, '...')
  } else {
    new_name <- pdfFileName
  }
  ptag1 = '<p>'
  pWithContent = paste(ptag1, new_name)
  html = paste(pWithContent, '</p>')
  return(html)
}



getHTML_Keywords <- function (words) {
  inner_html = '<div display: flex; flex-direction: row; flex-wrap: wrap;>'
  for (word in words) {
    ptag1 = paste('<div style="background-color: #81D3EA; border-radius: 15px; 
                  padding: 5px 3px 3px 3px; margin: 3px 3px 3px 3px; display: inline-block;"><p>', word)
    ptag2 = paste(ptag1, '</p></div>')
    inner_html = paste(inner_html, ptag2)
  }
  html = paste(inner_html, '</div>')
  return(html)
}

backend <- function(input, output, session){
  # creating empty state
  full_proposal_path <- reactiveVal()
  fileName <- reactiveVal()
  commonKeyWords <- reactiveVal('')

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
    # TODO: somehow store this path globally to be used multiple times
    # TODO: switch with parseDirPath
    # storing full proposal path as string to variable
    root = gsub("\\*|\\(|\\)","",as.character(input$dirProposals$root))
    path = input$dirProposals$path
    proposal_path = paste(path, collapse='/' )
    full_proposal_path(paste(root, proposal_path, sep = ''))
    # getting selected file name
    file_path =  parseFilePaths(volumes, input$proposalFile)
    selectedFile <- gsub("\\..*","",file_path$name)
    similar_proposal <- getSimilarProposal(dirPath = full_proposal_path(), selectedFile=selectedFile)
    fileName(similar_proposal[2])
    commonKeyWords(unlist(similar_proposal[4]))
  })
  
  # renders the most similar file name in the div button
  output$pdfFileName <- renderUI ({
    HTML(getHTML(fileName()))
  })
  
  # renders common keywords
  output$Keywords <- renderUI ({
    HTML(getHTML_Keywords(commonKeyWords()))
  })
  
  # open most similar pdf file 
  # TODO: extract the file path programatically
  observeEvent(input$openPDF, {
    # storing full proposal path as string to variable
    full_file_path = paste(paste(full_proposal_path(), fileName(), sep='/'), '.pdf', sep='')
    
    file.show(file.path(full_file_path))
  })
  
}