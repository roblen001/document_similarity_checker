# getHTML_similarityReportPreview.R
#
# Purpose: Dynamically renders list of similar proposals that will be in the
#  report
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-08-03
#
# ========================================================================================

getHTML_similarityReportPreview <- function (df) {
  df <- df[order(-df$common_words_weighted),]
  innerHTML <- '<div>'
  for (i in 1:nrow(df)) {
    container <- "<div style='border: 1px solid grey; margin: 5px; display: flex; flex-direction: column; padding-left: 5px;'>"

    title <- paste('<p>Proposal Title: ', paste(df[i,]$proposal_title, '</p>'))
    author <- paste('<p>Author: ', paste(df[i,]$author_of_proposal_title, '</p>'))
    content1 <- paste(title, author)
    content3 <- paste('<p>Similar Proposal Title:', paste(df[i,]$most_similar_proposal_title, '</p>'))
    count_common_words <- paste('<p>Number of Common Words:', paste(length(unlist(df[i,]$common_words_list)),'</p>'))
    if (length(df$common_words_weighted[i]) == 0) {
      similarity_index <- paste("<p>Similarity Indicator:","NOT SIMILAR</p>")
    }
    else if (df$common_words_weighted[i] >= 1 & df$common_words_weighted[i] < 6) {
      similarity_index <- paste("<p>Similarity Indicator:","NOT VERY SIMILAR</p>")
    }
    else if (df$common_words_weighted[i] >= 6 & df$common_words_weighted[i] < 10) {
      similarity_index <- paste("<p>Similarity Indicator:","SOME SIMILARITY</p>")
    } else if (df$common_words_weighted[i] >= 10) {
      similarity_index <- paste("<p>Similarity Indicator:","SIMILAR</p>")
    }
    similar_proposal_author <- paste('<p>Author of Similar Proposal: ', paste(df[i,]$author_of_most_similar_proposal, '</p>'))
    common_words <- paste("<p>Common Words List:", paste(unlist(df[i,]$common_words_list), collapse=', '), '</p>')
    # content2 <- paste(count_common_words, common_words)

    contentList <- c(content1, count_common_words, content3, similarity_index, similar_proposal_author,
                     common_words)

    content <- paste(contentList, collapse="")

    container <- paste(container, paste(content, '</div>'))

    innerHTML <- paste(innerHTML, container)
  }
  html <- paste(innerHTML, "</div></div>")
  return(html)
}
