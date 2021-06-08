# getCleanedAndTokenizedData.R 
#
# Purpose: Will tokenize text data from dataframe and return a wide format
#   of the dataframe with columns as words and values are the amount of times
#   the keywords are found in each article
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-27
#
# ========================================================================================

getCleanedAndTokenizedData <- function(corpus_raw ,custom_bigram_stop_words, custom_stop_words, type) {
  data(stop_words)
  
  if (type == 'PublishedPapers'){
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
      dplyr::count(word, sort = TRUE) 
    
    corpus_clean <- corpus_clean %>%
      pivot_wider(names_from = word, values_from = n)
    # set keyword amount to NA if it is below a certain threshold
    corpus_clean <- corpus_clean %>% replace_with_na_all(condition = ~.x == 1 )
    # remove cols with less then 2 NAs 
    visualize <- corpus_clean[, which(colMeans(!is.na(corpus_clean)) > 1/length(corpus_clean$proposal_title))]  
    
  } else if (type == 'OtherProposals'){
    corpus_clean <- corpus_raw %>%
      unnest_tokens(word, text, token = "ngrams", n = 3)%>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      filter(!word1 %in% c(stop_words$word, custom_stop_words)) %>%
      filter(!word2 %in% c(stop_words$word, custom_stop_words)) %>%
      filter(!word3 %in% c(stop_words$word, custom_stop_words)) %>% 
      unite(word, word1, word2, word3, sep = " ") %>%
      filter(!word %in% custom_bigram_stop_words)
    
    
    corpus_clean$word <- singularize(corpus_clean$word)
    
    corpus_clean <- corpus_clean %>%
      group_by(id) %>%
      dplyr::count(word, sort = TRUE) 
    
    corpus_clean <- corpus_clean %>%
      pivot_wider(names_from = word, values_from = n)

    # remove cols with less then 2 NAs 
    visualize <- corpus_clean[, which(colMeans(!is.na(corpus_clean)) > 1/length(corpus_clean$id))]
  }
  
  
  return(visualize)
}
