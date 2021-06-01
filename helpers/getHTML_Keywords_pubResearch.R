# getHTML_Keywords_pubResearch.R 
#
# Purpose: Dynamically renders keywords for selection type published reasearch
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-01
#
# ========================================================================================

getHTML_Keywords_pubResearch <- function (words) {
  inner_html = '<div display: flex; flex-direction: row; flex-wrap: wrap;>'
  for (word in words) {
    ptag1 = paste('<div style="background-color: #81D3EA; border-radius: 15px; 
                  padding: 5px 3px 3px 3px; margin: 3px 3px 3px 3px; display: inline-block;"><p>', word)
    ptag2 = paste(ptag1, '</p></div>')
    inner_html = paste(inner_html, ptag2)
  }
  html = paste(inner_html, '</div>')
  return(html)
}