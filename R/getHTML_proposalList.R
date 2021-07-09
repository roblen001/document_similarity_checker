# getHTML_proposalList.R 
#
# Purpose: Dynamically renders list of similar proposals
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-03
#
# ========================================================================================

getHTML_proposalList <- function (df) {
  innerHTML <- '<div>'
  for (i in 1:nrow(df)) {
    container <- "<div style='border: 1px solid grey; margin: 5px; display: flex; flex-direction: column; padding-left: 5px;'>"
    
    title <- paste('<p>Proposal title: ', paste(df[i,]$title, '</p>'))
    author <- paste('<p>Author: ', paste(df[i,]$author, '</p>'))
    content1 <- paste(title, author)
    count_common_words <- paste('<p>Amount of words in common:', paste(df[i,]$amount_of_wordsCommon,'</p>'))
    common_words <- paste('<p>Common Words: ', paste(sub(',', '', df[i,]$common_words), '</p>'))
    content2 <- paste(count_common_words, common_words)
    
    content <- paste(content1, content2)
    
    container <- paste(container, paste(content, '</div>'))
    
    innerHTML <- paste(innerHTML, container)
  }
  html <- paste(innerHTML, "</div></div>")
  return(html)
}