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
source('helpers/getSimilarProposalsFromCSV.R')

# TODO: do I give people the ability to add keywords to the stop_words df
# This will take a certain row from similar articles dataframe and style it
getHTML <- function (pdfFileName) {
  if (nchar(pdfFileName) > 30){
    indexed <- substr(pdfFileName, start = 1, stop = 30)
    new_name <- paste(indexed, '...')
  } else {
    new_name <- pdfFileName
  }
  ptag1 = '<p>'
  pWithContent = paste(ptag1, new_name)
  html = paste(pWithContent, '</p>')
  return(html)
}

# renders common keywords for selectionType == 'PublishedResearch'
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

# renders common keywords for selectionType == 'OtherProposals'
getHTML_Keywords_OtherProposals <- function (words) {
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

# dynamically render the proposal title
getHTML_ProposalTitle <- function (propsalTitle) {
  ptag1 = '<p>'
  pWithContent = paste(ptag1, propsalTitle)
  html = paste(pWithContent, '</p>')
  return(html)
}

# dynamically render the similarit level indicator for OTHERPROPOSAL
getHTML_Similarity_indicator_otherproposal <- function (commonWordCount){
  html = "<div style='display: flex; justify-content: center; align-items: center;
    background-color: white;'>"
  
  if (commonWordCount == 0) {
    ptag = paste("<p>", 'None Found</p>')
  }
  else if (commonWordCount >= 1 & commonWordCount < 6) {
    ptag = paste("<p style='color: orange;'>", 'Not Very Similar</p></div>')
  }
  else if (commonWordCount >= 6 & commonWordCount < 10) {
    ptag = paste("<p style='color: 	#ffe135;'>", 'Some Similarity</p></div>')
  } else if (commonWordCount >= 10) {
    ptag = paste("<p style='color: green;'>", 'Similar</p></div>')
  }
  html = paste(html, ptag)
  return(html)
}

# dynamically render the similarit level indicator for PUBLISHEDPAPERS
getHTML_Similarity_indicator_publishedPapers <- function (commonWordCount){
  html = "<div style='display: flex; justify-content: center; align-items: center;
    background-color: white;'>"
  
  if (commonWordCount == 0) {
    ptag = paste("<p>", 'None Found</p>')
  }
  else if (commonWordCount >= 1 & commonWordCount < 6) {
    ptag = paste("<p style='color: orange;'>", 'Not Very Similar</p></div>')
  }
  else if (commonWordCount >= 6 & commonWordCount < 10) {
    ptag = paste("<p style='color: 	#ffe135;'>", 'Some Similarity</p></div>')
  } else if (commonWordCount >= 10) {
    ptag = paste("<p style='color: green;'>", 'Similar</p></div>')
  }
  html = paste(html, ptag)
  return(html)
}

backend <- function(input, output, session){
  # creating empty state
  full_proposal_path <- reactiveVal()
  fileName <- reactiveVal()
  commonKeyWords <- reactiveVal('')
  selectedFilePath <- reactiveVal('')
  proposalTitle <- reactiveVal('')
  commonKeyWords_OtherProposals <- reactiveVal('')
  amount_of_commonWords <- reactiveVal(0)
  amount_of_commonWords_publishedPapers <- reactiveVal(0)
  
  # Computer volumes depend on OS: Windows, Mac, or Linux.
  volumes <- c(Home = fs::path_home(), getVolumes()())
  
  shinyDirChoose(input, id = "dirProposals", roots = volumes,
                 restrictions = system.file(package = "base"))
  output$dirProposalsOutput <- renderText("No proposal directory selected.")
  
  shinyFileChoose(input, id = "proposalFile", roots = volumes, filetypes = c("pdf"))
  output$proposalFileOutput <- renderText("No proposal file selected.")
  
  shinyFileChoose(input, id = "DataframeProposalFile", roots = volumes, filetypes = c("csv"))
  output$DataframeProposalFileOutput <- renderText("No proposal dataframe file selected.")

  #  FOR OTHER PROPOSAL SELECTION INPUT TYPE
  # 1) Select file containing proposal dataframe
  observeEvent(
    input$DataframeProposalFile, {
      proposalDFPath <- parseDirPath(volumes, input$DataframeProposalFile)

      # Reset output when selection of directory is canceled.
      output$DataframeProposalFileOutput <- renderText("No file selected.")

      # Otherwise if directory has been selected, print path of directory to screen.
      if (length(proposalDFPath) > 0){
        # Update output by printing new directory path.
        output$DataframeProposalFileOutput <- renderPrint(proposalDFPath)
      }
    }
  )
  
  #  FOR PUBLISHED RESEARCH SELECTION INPUT TYPE
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
      output$proposalFileOutput <- renderText("No file selected.")

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
    if (input$selectionType == 'PublishedResearch'){
      # show loading screen
      shinyjs::show("loading_page")
      # storing full proposal path as string to variable
      root = gsub("\\*|\\(|\\)","",as.character(input$dirProposals$root))
      path = input$dirProposals$path
      proposal_path = paste(path, collapse='/' )
      full_proposal_path =  parseDirPath(volumes, input$dirProposals)
      full_proposal_path(full_proposal_path)
      # getting selected file name
      file_path =  parseFilePaths(volumes, input$proposalFile)
      selectedFilePath(file_path$datapath)
      selectedFile <- gsub("\\..*","",file_path$name)
      similar_proposal <- getSimilarProposal(dirPath = full_proposal_path, selectedFile=selectedFile)
      fileName(similar_proposal[2])
      commonKeyWords(unlist(similar_proposal[4]))
      amount_of_commonWords_publishedPapers(similar_proposal$common_words_weighted)
      load_data()
    } else if (input$selectionType == 'OtherProposals'){
      # show loading screen
      shinyjs::show("loading_page2")
      proposal_df_path =  parseFilePaths(volumes, input$DataframeProposalFile)
      # getting selected file name
      similar_proposal <- getSimilarProposalsFromCSV(proposalDataFile = proposal_df_path$datapath, idForFileBeingChecked=input$proposalID)
      commonKeyWords_OtherProposals(unlist(similar_proposal[4]))
      amount_of_commonWords(similar_proposal$common_words_weighted)
      # getting the title of the most similar proposal
      corpus_raw <- read.csv(proposal_df_path$datapath)
      colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'text')
      title <- corpus_raw[corpus_raw$id == similar_proposal$most_similar_proposal ,]$proposal_title
      proposalTitle(title)
      load_data()
    }
  })
  
  # renders the most similar file name in the div button
  output$pdfFileName <- renderUI ({
    HTML(getHTML(fileName()))
  })
  
  # renders common keywords for selectionType == 'PublishedResearch'
  output$Keywords <- renderUI ({
    HTML(getHTML_Keywords(commonKeyWords()))
  })
  
  # renders common keywords for selectionType == 'OtherProposals'
  output$KeywordsOtherProposal <- renderUI ({
    HTML(getHTML_Keywords_OtherProposals(commonKeyWords_OtherProposals()))
  })
  
  # renders proposal title dynamically
  output$proposalTitle <- renderUI({
    HTML(getHTML_ProposalTitle(proposalTitle()))
  })
  
  # renders similarity level dynamically
  output$similarityLevel <- renderUI({
    HTML(getHTML_Similarity_indicator_otherproposal(amount_of_commonWords()))
  })
  
  # renders similarity level dynamically
  output$similarityLevelPublishedPapers <- renderUI({
    HTML(getHTML_Similarity_indicator_publishedPapers(amount_of_commonWords_publishedPapers()))
  })
  
  # open most similar pdf file 
  observeEvent(input$openPDF, {
    # storing full proposal path as string to variable
    if (input$selectionType == 'PublishedResearch'){
      file.show(file.path(selectedFilePath()))
    }
  })
  
}