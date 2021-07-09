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

getSimilarProposal <- function(dirPath='', selectedFile='') {
  if (dirPath == '') {
    # do nothing
  } else {
  corpus_raw <- getPDFContent(dirPath)

  custom_bigram_stop_words <- c('university press', 'citing article', 'https www.tandfonline.com', 'NA NA')
  custom_stop_words <- c('copyright', 'https', 'NA', 'doi')

  # Will return a wide format dataframe with proposals as rows, words as columns
  # and occurence of keywords in the proposal (NA means none)
  corpus_cleaned <- getCleanedAndTokenizedData(corpus_raw, custom_bigram_stop_words, custom_stop_words, type='PublishedPapers')
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
  contengency_table  <- data.table::setnames(contengency_table, old = colnames(contengency_table), corpus_cleaned$proposal_title)

  # list of most related articles
  most_related_articles <- colnames(contengency_table)[apply(contengency_table,1,which.max)]
  similar_articles <- tibble(corpus_cleaned$proposal_title, most_related_articles, max_values)

  colnames(similar_articles) <- c("proposal_title", "most_similar_proposal", "common_words_weighted")

  similar_articles_with_common_word_lst <- getCommonKeywords(corpus_cleaned, similar_articles)
  results <- similar_articles_with_common_word_lst %>% filter(proposal_title == selectedFile)
  return(results)
  }
}




























