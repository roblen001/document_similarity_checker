# getHTML_ProposalTitle.R 
#
# Purpose: Dynamically renders the proposal title ui
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-01
#
# ========================================================================================

getHTML_ProposalTitle <- function (propsalTitle) {
  ptag1 = '<p>'
  pWithContent = paste(ptag1, propsalTitle)
  html = paste(pWithContent, '</p>')
  return(html)
}