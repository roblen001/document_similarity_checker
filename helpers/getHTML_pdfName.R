# getHTML_pdfName.R 
#
# Purpose: Dynamically renders the file name for selection type
#  of published research.
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-06-01
#
# ========================================================================================

getHTML_pdfName <- function (pdfFileName) {
  if (nchar(pdfFileName) > 30){
    indexed <- substr(pdfFileName, start = 1, stop = 30)
    new_name <- paste(indexed, '...')
  } else {
    new_name <- pdfFileName
  }
  ptag1 = '<p>'
  pWithContent = paste(ptag1, new_name)
  html = paste(pWithContent, '</p>')
  return(html)
}
