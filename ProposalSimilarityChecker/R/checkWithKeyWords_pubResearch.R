# checkWithKeyWords_pubResearch.R
#
# Purpose: Gets a proposal with the most matching key words to the chosen
# proposal file for published research.
#
# Output: A list containing the name of the most simlar pdf and a list of
# keywords that are common between both proposals
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-07
#
# ========================================================================================

checkWithKeyWords_pubResearch <- function(dirPath='', input) {
  if (dirPath == '') {
    # do nothing
  } else {
    file.list <- list.files(dirPath)
    #  checking if files are pdf ext
    grepl(".pdf", file.list)

    # testing for a single article
    corpus_raw <- data.frame("file_name" = c(),"text" = c())

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
    corpus_raw <- ddply(corpus_raw, .(proposal_title), summarize, text=paste(text, collapse=""))

    colnames(corpus_raw) <- c('title', 'text')
    # separating input into list of words
    input_word_list <- as.list(strsplit(input, ","))
    # TODO: this is a temporary fix to the bug which occurs if only one
    # one word is inputted
    input_word_list <- c(input_word_list, "ANSI3456DFBEEU1", "ANSI342345g56DFBEEU1")

    for (word in input_word_list) {
      word <- trimws(word, which = "both")
      # adding white space to make sure it finds individual word and not substring
      word <- paste("", paste(word, ""))
      found <- sapply(tolower(word), grepl, tolower(corpus_raw$text))
      corpus_raw <- cbind(corpus_raw,found)
    }

    # counting the amount of common keywords found in each proposal
    corpus_clean <- subset(corpus_raw, select = -c(title, text))
    amount_of_commonWords <- apply(corpus_clean,MARGIN = 1,table)
    corpus_raw$amount_of_commonWords <- amount_of_commonWords

    if (typeof(corpus_raw$amount_of_commonWords) != 'list'){
      # if only a single word has been inputted
      final_results <- "NO WORDS IN COMMON"
    }else{
      corpus_raw <- corpus_raw %>% unnest_auto(amount_of_commonWords) %>% unnest(cols = c(`FALSE`, `TRUE`))
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
        for (word in input_word_list[[1]]) {
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
}
