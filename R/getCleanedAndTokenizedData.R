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
    # Unsure how to import the stop_words data without library()
    library(tidytext)
    library(pluralize)
    data(stop_words)
    corpus_clean <- corpus_raw %>%
      tidytext::unnest_tokens(word, text, token = "ngrams", n = 3)%>%
      tidyr::separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      dplyr::filter(!word1 %in% c(stop_words$word, custom_stop_words)) %>%
      dplyr::filter(!word2 %in% c(stop_words$word, custom_stop_words)) %>%
      dplyr::filter(!word3 %in% c(stop_words$word, custom_stop_words)) %>%
      tidyr::unite(word, word1, word2, word3, sep = " ") %>%
      dplyr::filter(!word %in% custom_bigram_stop_words)

    corpus_clean$word <- singularize(corpus_clean$word)
    corpus_clean <- corpus_clean %>%
      group_by(id) %>%
      dplyr::count(word, sort = TRUE)

    corpus_clean <- corpus_clean %>%
      tidyr::pivot_wider(names_from = word, values_from = n)

    # remove cols with less then 2 NAs
    visualize <- corpus_clean[, which(colMeans(!is.na(corpus_clean)) > 1/length(corpus_clean$id))]


  return(visualize)
}
