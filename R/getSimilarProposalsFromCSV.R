# getSimilarProposalsFromCSV.R
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

getSimilarProposalsFromCSV <- function(proposalDataFile='', idForFileBeingChecked='',
                                       background_info='', type='') {
  if (proposalDataFile == '') {
    # do nothing
  } else {
    corpus_raw <- readr::read_csv(proposalDataFile)
    # the inputed dataframe should always have the same ordering ID, Author, Title, Background information
    colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'background', 'hypothesis', 'variables', 'analysis', 'significance',
                              'Keyword 1', 'Keyword 2', 'Keyword 3', 'Keyword 4', 'Keyword 5', 'status')
    corpus_raw$text<- with(corpus_raw, paste0(background, hypothesis, significance))
    corpus_raw <- corpus_raw %>%
      dplyr::select('id', 'author', 'proposal_title', 'text', 'status')

    if (type != 'SimilarityReport'){
      corpus_raw <- corpus_raw %>%
        dplyr::filter(status == "1" | id == idForFileBeingChecked)
    }
    # assumes the checkUsing "backgroundinfo" was selected
    if (background_info != ''){
      corpus_raw <- corpus_raw %>% dplyr::select(-status)
      df <- data.frame("TEMPORARYID16352","TEMPORARYAUTHOR16352", "TEMPORARYTITLE16352", background_info)
      names(df) <- c('id', 'author', 'proposal_title', 'text')
      corpus_raw <- rbind(corpus_raw, df)
    }
    custom_bigram_stop_words <- c('university press', 'citing article', 'https www.tandfonline.com', 'NA NA')
    custom_stop_words <- c('copyright', 'https', 'NA', 'doi')
    # Will return a wide format dataframe with proposals as rows, words as columns
    # and occurence of keywords in the proposal (NA means none)
    corpus_cleaned <- getCleanedAndTokenizedData(corpus_raw, custom_bigram_stop_words, custom_stop_words, type='OtherProposals')
    #  giving the amount of keywords a weight
    visualize_matrix <- subset(corpus_cleaned, select = -c(id) )
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
    contengency_table  <- data.table::setnames(contengency_table, old = colnames(contengency_table), corpus_cleaned$id)
    # list of most related articles
    most_related_articles <- colnames(contengency_table)[apply(contengency_table,1,which.max)]
    similar_articles <- tibble(corpus_cleaned$id, most_related_articles, max_values)
    colnames(similar_articles) <- c("id", "most_similar_proposal", "common_words_weighted")
    similar_articles_with_common_word_lst <- getCommonKeywords(corpus_cleaned, similar_articles)

    if (type == 'SimilarityReport'){
      # reformatting results dataframe for similarity report
      results <- similar_articles_with_common_word_lst
      results <- merge(x=results, y=corpus_raw, by.x='proposal_title', by.y='id')
      results <- results %>% dplyr::filter(is.na(status))
      results <- results %>% dplyr::select(-text, -proposal_title, -status)
      colnames(results) <- c("most_similar_proposal", "common_words_weighted", "common_words_list",
                             "author_of_proposal_title", "proposal_title")
      results <- merge(x=results, y=corpus_raw, by.x='most_similar_proposal', by.y='id')
      results <- results %>% dplyr::select(-text, -most_similar_proposal)
      colnames(results) <- c("common_words_weighted", "common_words_list",
                             "author_of_proposal_title", "proposal_title",
                             "author_of_most_similar_proposal", "most_similar_proposal_title")
    }else{
      results <- similar_articles_with_common_word_lst %>% dplyr::filter(proposal_title == idForFileBeingChecked)
    }

    return(results)
  }
}
