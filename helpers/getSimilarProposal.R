# getSimilarProposal.R
#
# Purpose: Gets a proposal with the most matching key words to the chosen
# proposal file.
#
# Output: A list containing the name of the most simlar pdf and a list of
# keywords that are common between both proposals
# 
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-20
#
# ========================================================================================
library(pdftools)
require(readtext)
library(textstem)
library(tidytext)
library(tidyverse)
library(pluralize)
library(naniar)
library(data.table)
library(purrr)


getSimilarProposal <- function(dirPath='', selectedFile='') {
  if (dirPath == '') {
    # do nothing
  } else {
  corpus_raw <- getPDFContent(dirPath)
  
  custom_bigram_stop_words <- c('university press', 'citing article', 'https www.tandfonline.com')
  custom_stop_words <- c('copyright', 'https', 'NA', 'doi')
  
  # Will return a wide format dataframe with proposals as rows, words as columns
  # and occurence of keywords in the proposal (NA means none)
  corpus_cleaned <- getCleanedAndTokenizedData(corpus_raw, custom_bigram_stop_words, custom_stop_words)
  #  giving the amount of keywords a weight
  visualize_matrix <- subset(corpus_cleaned, select = -c(proposal_title) )
  visualize_matrix[is.na((visualize_matrix))] <- 0
  visualize_matrix[1 < visualize_matrix & visualize_matrix <= 5] <- 1
  visualize_matrix[5 < visualize_matrix & visualize_matrix <= 10] <- 2
  visualize_matrix[10 < visualize_matrix & visualize_matrix <= 20] <- 3
  visualize_matrix[20 < visualize_matrix & visualize_matrix] <- 4
  
  visualize_matrix <- as.matrix(visualize_matrix)
  contengency_table_matrix <- visualize_matrix %*%  t(visualize_matrix)
  # set diagonal to -1
  diag(contengency_table_matrix) <- -1
  # list of max values
  max_values <- apply(contengency_table_matrix,1,max)
  
  # get list of max values ie. most similar 
  diag(contengency_table_matrix) <- NA
  # for visual
  contengency_table <- as.tibble(contengency_table_matrix)
  contengency_table  <- setnames(contengency_table, old = colnames(contengency_table), corpus_cleaned$proposal_title)
  
  # list of most related articles
  most_related_articles <- colnames(contengency_table)[apply(contengency_table,1,which.max)]
  similar_articles <- tibble(corpus_cleaned$proposal_title, most_related_articles, max_values)
  
  colnames(similar_articles) <- c("proposal_title", "most_similar_proposal", "common_words_weighted")

  similar_articles_with_common_word_lst <- getCommonKeywords(corpus_cleaned, similar_articles)
  results <- similar_articles_with_common_word_lst %>% filter(proposal_title == selectedFile)
  return(results)
  }
}


# ALL HELPER FUNCTIONS IN getSimilarProposals ARE BELOW ========================

# GET PDF CONTENT ===================================================================
# Helper Function: 
#   -Extract content from proposal pdfs
#   -Returns: Dataframe with proposal titles and text as a string 
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

# TOKENIZE AND CLEAN TEXT ===================================================================
# Helper Function: 
#   -Takes a dataframe with srting of content (ie proposal text) and turns it into usable data
#   -Returns: Dataframe with proposal titles and the amount of key words in the
#    keywords found in each article (NA means none were found)
getCleanedAndTokenizedData <- function(corpus_raw ,custom_bigram_stop_words, custom_stop_words) {
  data(stop_words)
  
  corpus_clean <- corpus_raw %>%
    unnest_tokens(word, text, token = "ngrams", n = 2)%>%
    separate(word, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% c(stop_words$word, custom_stop_words)) %>%
    filter(!word2 %in% c(stop_words$word, custom_stop_words)) %>% 
    unite(word, word1, word2, sep = " ") %>%
    filter(!word %in% custom_bigram_stop_words)
  

  corpus_clean$word <- singularize(corpus_clean$word)
  
  corpus_clean <- corpus_clean %>%
    group_by(proposal_title) %>%
    count(word, sort = TRUE) 
  
  corpus_clean <- corpus_clean %>%
    pivot_wider(names_from = word, values_from = n)
    # set keyword amount to NA if it is below a certain threshold
  corpus_clean <- corpus_clean %>% replace_with_na_all(condition = ~.x == 1 )
  # remove cols with less then 2 NAs 
  visualize <- corpus_clean[, which(colMeans(!is.na(corpus_clean)) > 1/length(corpus_clean$proposal_title))]
  
  return(visualize)
}

# CREATE LIST OF COMMON KEYWORDS ===================================================================
# Helper Function: 
#   -Takes results from getCleanedAndTokenizedData() 
#   -Returns: The dataframe of similar articles but with common keywords list appended
#   to it
getCommonKeywords <- function(visualize, similar_articles) {
  visualize_long <- visualize %>% 
    pivot_longer(!proposal_title, names_to = "keywords", values_to = "count") %>%
    drop_na() %>%
    select(proposal_title, keywords)
  
  visualize_long <- visualize_long %>% group_by(proposal_title) %>%
    summarise(
      alltypes = paste(keywords, collapse=", "))
  
  results <- merge(similar_articles, visualize_long, by='proposal_title' )
  results <- merge(results, visualize_long, by.x = "most_similar_proposal", by.y = "proposal_title" )
  
  s <- strsplit(results$alltypes.x , split = ", ")
  a <- strsplit(results$alltypes.y , split = ", ")
  
  common_words_list = vector('list', length(s))
  for (i in 1:length(s)) {
    common_words <- pmap(list(s[i], a[i]), intersect)
    common_words_list[[i]] <- common_words
  }
  
  results <- results %>%
    add_column(common_words_list)
  
  results <- results %>% 
    select(proposal_title, most_similar_proposal, common_words_weighted, common_words_list)
 
  return(results)
}
































