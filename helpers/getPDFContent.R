# getPDFContent.R 
#
# Purpose: Extracts text from pdf files and returns a dataframe containing,
#   text and article titrles
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-27
#
# ========================================================================================

getPDFContent <- function(proposalPath) {
  file.list <- list.files(path = proposalPath)
  #  grabs files with .pdf extensions
  grepl(".pdf", file.list)
  
  # going through proposals and extracting text
  corpus_raw <- data.frame("proposal_title" = c(),"text" = c())
  for (i in 1:length(file.list)){
    document_page_list <-pdf_text(paste("proposals/", file.list[i],sep = "")) 
    document_page_list_no_num <- gsub('[0-9]+', '', document_page_list)
    document <- paste(document_page_list_no_num, collapse=', ' )%>% strsplit("\n")-> document_text
    data.frame("proposal_title" = gsub(x =file.list[i],pattern = ".pdf", replacement = ""), 
               "text" = document_text, stringsAsFactors = FALSE) -> document
    
    colnames(document) <- c("proposal_title", "text")
    corpus_raw <- rbind(corpus_raw,document) 
  }
  return(corpus_raw)
}
