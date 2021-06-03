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
  inner_html = '<div>'
  for (i in nrow(df)) {
    ptag1 = paste('<div style="background-color: #81D3EA; border-radius: 15px; 
                  padding: 5px 3px 3px 3px; margin: 3px 3px 3px 3px; display: inline-block;"><p>', 
                  df$title[i])
    ptag2 = paste(ptag1, '</p></div>')
    inner_html = paste(inner_html, ptag2)
  }
  html = paste(inner_html, '</div>')
  return(html)
}