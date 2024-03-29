# checkWithKeywords.R
#
# Purpose: Will find the proposals containing the largest amount of the inputed
#   keywords
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-03
#
# ========================================================================================

checkWithKeywords <- function(filePath, input){
  data <- read.csv(filePath)
  colnames(data) <- c('id','author', 'title', 'background', 'hypothesis', 'variables', 'analysis', 'significance',
                      'Keyword 1', 'Keyword 2', 'Keyword 3', 'Keyword 4', 'Keyword 5', 'status')
  data$text<- with(data, paste0(background, hypothesis, significance))
  corpus_raw <- data %>% dplyr::select('id', 'author', 'title', 'text', 'status')
  corpus_raw <- corpus_raw %>%
    dplyr::filter(status == "1")

  # separating input into list of words
  input_word_list <- as.list(strsplit(input, ","))
  # TODO: this is a temporary fix to the bug which occurs if only one
  # one word is inputted
  input_word_list <- c(input_word_list, "ANSI3456DFBEEU1")
  # add pluralized and apostrophe to words
  new_input_word_lst <- c()
  for (word in input_word_list) {
    word <- trimws(word, which = "both")
    pluralize <- paste(word, "s", sep = "")
    apostrophe <- paste(word, "'s", sep = "")
    singularize <- substr(word,1,nchar(word)-1)
    remove_apastrophe <- substr(word,1,nchar(word)-2)
    new_input_word_lst <- c(new_input_word_lst, word, pluralize, apostrophe,
                            singularize, remove_apastrophe)
  }
  new_input_word_lst <- unique(new_input_word_lst)
  for (word in new_input_word_lst) {
    word <- trimws(word, which = "both")
    # adding white space to make sure it finds individual word and not substring
    word <- paste("", paste(word, ""))
    # adding white space to the front of the text value so the first word
    # can be counted as a word
    corpus_raw$text <- paste(' ', corpus_raw$text)
    found <- sapply(tolower(word), grepl, tolower(corpus_raw$text))
    corpus_raw <- cbind(corpus_raw,found)
  }
  # counting the amount of common keywords found in each proposal
  corpus_clean <- subset(corpus_raw, select = -c(id, text, title, author, status) )
  amount_of_commonWords <- apply(corpus_clean,MARGIN=1,table)
  corpus_raw$amount_of_commonWords <- amount_of_commonWords
  if (typeof(corpus_raw$amount_of_commonWords) != 'list'){
    final_results <- "NO WORDS IN COMMON"
  }else{
    corpus_raw <- corpus_raw %>% tidyr::unnest_auto(amount_of_commonWords) %>% tidyr::unnest(cols = c(`FALSE`, `TRUE`))
    corpus_raw[is.na(corpus_raw)] <- 0
    colnames(corpus_raw)[length(corpus_raw)] <- 'amount_of_wordsCommon'
    results <- corpus_raw[order(corpus_raw$amount_of_wordsCommon, decreasing = TRUE),]
    final_results <- results %>%
      top_n(5)
    ##Go through each row and determine if a value is zero
    row_sub = apply(final_results, 1, function(row) all(row !=0 ))
    ##Subset as usual
    final_results <- final_results[row_sub,]

    # TODO: Optimize this
    common_words <- c()
    for (row in final_results$text) {
      str_commonWords <- ''
      for (word in new_input_word_lst) {
        word <- trimws(word, which = "both")
        # adding white space to make sure it finds individual word and not substring
        word <- paste("", paste(word, ""))
        if (grepl(tolower(word), tolower(row), fixed = TRUE)) {
          str_commonWords <- paste(str_commonWords, word, sep = ", ")
        }
      }
      common_words <- c(common_words, str_commonWords)
    }

    final_results$common_words <- common_words
  }

  return(final_results)
}


