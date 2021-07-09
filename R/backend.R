# backend.R
#
# Purpose: The back end of the application
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-20
#
# ========================================================================================

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
  similar_articles_df <- reactiveVal()
  DataframeProposalFile <- reactive({input$DataframeProposalFile})
  # getting working directory of report.rmd file
  report_path_src <- paste(getwd(), "/inst/www/ONDRI_full-logo_web.png", sep="")
  report_path <- reactive({report_path_src})
  print(report_path)
  # Computer volumes depend on OS: Windows, Mac, or Linux.
  volumes <- c(Home = fs::path_home(), shinyFiles::getVolumes()())

  shinyFiles::shinyFileChoose(input, id = "proposalFile", roots = volumes, filetypes = c("pdf"))
  output$proposalFileOutput <- renderText("No proposal file selected.")

  shinyFiles::shinyFileChoose(input, id = "DataframeProposalFile", roots = volumes, filetypes = c("csv"))
  output$DataframeProposalFileOutput <- renderText("No proposal dataframe file selected.")

  #  FOR OTHER PROPOSAL SELECTION INPUT TYPE
  # 1) Select file containing proposal dataframe
  observeEvent(
    input$DataframeProposalFile, {
      proposalDFPath <- shinyFiles::parseDirPath(volumes, input$DataframeProposalFile)

      # Reset output when selection of directory is canceled.
      output$DataframeProposalFileOutput <- renderText("No file selected.")

      # Otherwise if directory has been selected, print path of directory to screen.
      if (length(proposalDFPath) > 0){
        # Update output by printing new directory path.
        output$DataframeProposalFileOutput <- renderPrint(proposalDFPath)
      }
    }
  )

  # detects when the select input is changed
  observeEvent(input$checkUsing, {
    shinyjs::hide("main_content")
  })

  # submit button
  observeEvent(input$BeginCheck, {
      # show loading screen
      shinyjs::show("loading_page")
      shinyjs::hide("main_content")
      proposal_df_path =  shinyFiles::parseFilePaths(volumes, input$DataframeProposalFile)
      #  checking what the user has selected for CHECK USING
      if (input$checkUsing == 'Keywords'){
        # getting selected file name
        similar_proposal <- checkWithKeywords(filePath = proposal_df_path$datapath, input = input$keywordsList)
        proposalDF(similar_proposal)
      }else if (input$checkUsing == 'ProposalId'){
        # getting selected file name
        similar_proposal <- getSimilarProposalsFromCSV(proposalDataFile = proposal_df_path$datapath,
                                                       idForFileBeingChecked=input$proposalID)
        commonKeyWords_OtherProposals(unlist(similar_proposal[4]))
        amount_of_commonWords(similar_proposal$common_words_weighted)
        # getting the title of the most similar proposal
        corpus_raw <- read.csv(proposal_df_path$datapath)
        colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'text')
        title <- corpus_raw[corpus_raw$id == similar_proposal$most_similar_proposal ,]$proposal_title
        proposalTitle(title)
      } else if (input$checkUsing == "BackgroundInformation") {
        # getting selected file name
        similar_proposal <- getSimilarProposalsFromCSV(proposalDataFile = proposal_df_path$datapath,
                                                       idForFileBeingChecked='TEMPORARYID16352',
                                                       background_info=input$backgroundInformation)
        commonKeyWords_OtherProposals(unlist(similar_proposal[4]))
        amount_of_commonWords(similar_proposal$common_words_weighted)
        # getting the title of the most similar proposal
        corpus_raw <- read.csv(proposal_df_path$datapath)
        colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'text')
        title <- corpus_raw[corpus_raw$id == similar_proposal$most_similar_proposal ,]$proposal_title
        proposalTitle(title)
      } else if (input$checkUsing == "SimilarityReport"){
        proposal_df_path =  shinyFiles::parseFilePaths(volumes, input$DataframeProposalFile)
        similar_articles_df(getSimilarProposalsFromCSV(proposalDataFile = proposal_df_path$datapath,
                                   type = 'SimilarityReport'))
      }
      load_data()
  })


  output$downloadReport <- downloadHandler(
    filename = 'similarity-report.pdf',

    content = function(file) {
      print(paste(getwd(), '/inst/www/ONDRI_full-logo_web.png', sep = ""))
      src <- normalizePath(paste(getwd(), '/R/report.rmd', sep = ""))
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      out <- rmarkdown::render('report.Rmd', rmarkdown::pdf_document())
      file.rename(out, file)
    }
  )

  # renders common keywords
  output$KeywordsOtherProposal <- renderUI ({
    HTML(getHTML_Keywords_OtherProposals(commonKeyWords_OtherProposals()))
  })

  # renders proposal title dynamically
  output$proposalTitle <- renderUI({
    HTML(getHTML_ProposalTitle(proposalTitle()))
  })

  # renders proposal list dynamically
  output$proposalList <- renderUI({
    # check if there were any common words found
    if (proposalDF() == "NO WORDS IN COMMON"){
      HTML('<p>NO WORDS IN COMMON</p>')
    }else{
      HTML(getHTML_proposalList(proposalDF()))
    }
  })

  # renders similarity level dynamically
  output$similarityLevel <- renderUI({
    HTML(getHTML_Similarity_indicator_otherproposal(amount_of_commonWords()))
  })

}