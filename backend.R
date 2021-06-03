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
source('helpers/getHTML_pdfName.R')
source('helpers/getHTML_Keywords_pubResearch.R')
source('helpers/getHTML_Keywords_OtherProposals.R')
source('helpers/getHTML_ProposalTitle.R')
source('helpers/getHTML_Similarity_indicator_otherproposal.R')
source('helpers/getHTML_Similarity_indicator_publishedPapers.R')
source('helpers/checkWithKeywords.R')

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
  proposalDF <- reactiveVal()
  
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
      shinyjs::hide("main_content") 
      # storing full proposal path as string to variable
      root = gsub("\\*|\\(|\\)","",as.character(input$dirProposals$root))
      path = input$dirProposals$path
      proposal_path = paste(path, collapse='/' )
      full_proposal_path =  parseDirPath(volumes, input$dirProposals)
      full_proposal_path(full_proposal_path)
      # getting selected file name
      file_path =  parseFilePaths(volumes, input$proposalFile)
      selectedFile <- gsub("\\..*","",file_path$name)
      similar_proposal <- getSimilarProposal(dirPath = full_proposal_path, selectedFile=selectedFile)
      fileName(similar_proposal[2])
      selectedFilePath(paste(parseDirPath(volumes, input$dirProposals), paste(fileName(), '.pdf', sep=""), sep="/"))
      commonKeyWords(unlist(similar_proposal[4]))
      amount_of_commonWords_publishedPapers(similar_proposal$common_words_weighted)
      load_data()
    } else if (input$selectionType == 'OtherProposals'){
      # show loading screen
      shinyjs::show("loading_page")
      shinyjs::hide("main_content") 
      proposal_df_path =  parseFilePaths(volumes, input$DataframeProposalFile)
      #  checking what the user has selected for CHECK USING
      if (input$checkUsing == 'Keywords'){
        
        
        # getting selected file name
        similar_proposal <- checkWithKeywords(filePath = proposal_df_path$datapath, input = input$keywordsList)
        print(similar_proposal)
        # commonKeyWords_OtherProposals()
        # amount_of_commonWords(similar_proposal$common_words_weighted)
        # # getting the title of the most similar proposal
        # corpus_raw <- read.csv(proposal_df_path$datapath)
        # colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'text')
        # title <- corpus_raw[corpus_raw$id == similar_proposal$most_similar_proposal ,]$proposal_title
        proposalDF(similar_proposal)
        
        
        
      }else if (input$checkUsing == 'ProposalId'){
        # getting selected file name
        similar_proposal <- getSimilarProposalsFromCSV(proposalDataFile = proposal_df_path$datapath, idForFileBeingChecked=input$proposalID)
        commonKeyWords_OtherProposals(unlist(similar_proposal[4]))
        amount_of_commonWords(similar_proposal$common_words_weighted)
        # getting the title of the most similar proposal
        corpus_raw <- read.csv(proposal_df_path$datapath)
        colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'text')
        title <- corpus_raw[corpus_raw$id == similar_proposal$most_similar_proposal ,]$proposal_title
        proposalTitle(title) 
      }
      load_data()
    }
  })
  
  # renders the most similar file name in the div button
  output$pdfFileName <- renderUI ({
    # check if there is a similar file
    print(identical(fileName()$most_similar_proposal, character(0)))
    if (identical(fileName()$most_similar_proposal, character(0)) == FALSE )  {
      HTML(getHTML_pdfName(fileName()))
    } else {
      HTML('<p>NO SIMILAR FILE FOUND</p>')
    }
  })
  
  # renders common keywords for selectionType == 'PublishedResearch'
  output$Keywords <- renderUI ({
    HTML(getHTML_Keywords_pubResearch(commonKeyWords()))
  })
  
  # renders common keywords for selectionType == 'OtherProposals'
  output$KeywordsOtherProposal <- renderUI ({
    HTML(getHTML_Keywords_OtherProposals(commonKeyWords_OtherProposals()))
  })
  
  # renders proposal title dynamically
  output$proposalTitle <- renderUI({
    HTML(getHTML_ProposalTitle(proposalTitle()))
  })
  
  # renders proposal list dynamically
  output$proposalList <- renderUI({
    HTML(getHTML_ProposalTitle(proposalDF()))
  })
  
  
  # renders similarity level dynamically
  output$similarityLevel <- renderUI({
    print(fileName()$most_similar_proposal)
    if (identical(fileName()$most_similar_proposal, NULL) == FALSE )  {
      HTML(getHTML_Similarity_indicator_otherproposal(amount_of_commonWords()))
    } else {
      HTML('<p>NO SIMILAR FILE FOUND</p>')
    }
  })
  
  # renders similarity level dynamically
  output$similarityLevelPublishedPapers <- renderUI({
    # check if there is a similar file 
    if(identical(fileName()$most_similar_proposal, character(0)) == FALSE){
      HTML(getHTML_Similarity_indicator_publishedPapers(amount_of_commonWords_publishedPapers()))
    }
  })
  
  # open most similar pdf file 
  observeEvent(input$openPDF, {
    # check if there is a similar file 
    if(identical(fileName()$most_similar_proposal, character(0)) == FALSE){
      # storing full proposal path as string to variable
      if (input$selectionType == 'PublishedResearch'){
        file.show(file.path(selectedFilePath()))
      } 
    }
  })
  
}