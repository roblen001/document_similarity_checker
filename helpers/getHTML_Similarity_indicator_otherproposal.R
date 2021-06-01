# getHTML_Similarity_indicator_otherproposal.R 
#
# Purpose: Dynamically renders similarity indicator level for selection type
#  other proposal
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-01
#
# ========================================================================================

getHTML_Similarity_indicator_otherproposal <- function (commonWordCount){
  html = "<div style='display: flex; justify-content: center; align-items: center;
    background-color: white;'>"
  print(commonWordCount)
  if (commonWordCount == 0) {
    ptag = paste("<p>", 'None Found</p>')
  }
  else if (commonWordCount >= 1 & commonWordCount < 6) {
    ptag = paste("<p style='color: orange;'>", 'Not Very Similar</p></div>')
  }
  else if (commonWordCount >= 6 & commonWordCount < 10) {
    ptag = paste("<p style='color: 	#ffe135;'>", 'Some Similarity</p></div>')
  } else if (commonWordCount >= 10) {
    ptag = paste("<p style='color: green;'>", 'Similar</p></div>')
  }
  html = paste(html, ptag)
  return(html)
}
