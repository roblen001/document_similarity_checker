# getHTML_simalarityReportPreview.R
#
# Purpose: Dynamically renders list of similar proposals that will be in the
#  report
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-08-03
#
# ========================================================================================

getHTML_simalarityReportPreview <- function (df) {
  df <- df[order(-df$common_words_weighted),]
  innerHTML <- '<div>'
  for (i in 1:nrow(df)) {
    container <- "<div style='border: 1px solid grey; margin: 5px; display: flex; flex-direction: column; padding-left: 5px;'>"

    title <- paste('<p>Proposal Title: ', paste(df[i,]$proposal_title, '</p>'))
    author <- paste('<p>Author: ', paste(df[i,]$author_of_proposal_title, '</p>'))
    content1 <- paste(title, author)
    content3 <- paste('<p>Similar Proposal Title:', paste(df[i,]$most_similar_proposal_title, '</p>'))
    count_common_words <- paste('<p>Number of Common Words:', paste(df[i,]$common_words_weighted,'</p>'))
    if (length(df$common_words_weighted[i]) == 0) {
      similarity_index <- paste("Similarity Indicator:","NOT SIMILAR", "\n")
    }
    else if (df$common_words_weighted[i] >= 1 & df$common_words_weighted[i] < 6) {
      similarity_index <- paste("Similarity Indicator:","NOT VERY SIMILAR", "\n")
    }
    else if (df$common_words_weighted[i] >= 6 & df$common_words_weighted[i] < 10) {
      similarity_index <- paste("Similarity Indicator:","SOME SIMILARITY", "\n")
    } else if (df$common_words_weighted[i] >= 10) {
      similarity_index <- paste("Similarity Indicator:","SIMILAR", "\n")
    }
    similar_proposal_author <- paste('<p>Proposal Title: ', paste(df[i,]$author_of_most_similar_proposal, '</p>'))
    common_words <- paste('<p>Common Words List: ', paste(sub(',', '', df[i,]$common_words), '</p>'))
    # content2 <- paste(count_common_words, common_words)

    contentList <- c(content1, count_common_words, content3, similarity_index,
                     common_words, similar_proposal_author)
    content <- paste(contentList, collapse="")

    container <- paste(container, paste(content, '</div>'))

    innerHTML <- paste(innerHTML, container)
  }
  html <- paste(innerHTML, "</div></div>")
  return(html)
}
