# getCommonKeywords.R 
#
# Purpose: Takes the results from getCleanedAndTokenizedData() and returns the 
#  dataframe of similar articles
#
# Author: Roberto Lentini (rlentini@research.baycrest.org)
#
# Date: 2021-05-27
#
# ========================================================================================

getCommonKeywords <- function(visualize, similar_articles) {
  #  temporary fix to the naming issues
  colnames(visualize)[1] <- "proposal_title"
  colnames(similar_articles)[1] <- "proposal_title"
  
  visualize_long <- visualize %>% 
    pivot_longer(!proposal_title, names_to = "keywords", values_to = "count") %>%
    drop_na() %>%
    select(proposal_title, keywords)
  

  visualize_long <- visualize_long %>% group_by(proposal_title) %>%
    dplyr::summarise(
      alltypes = paste(keywords, collapse=", "))
  
  print(colnames(visualize_long))
  
  results <- merge(similar_articles, visualize_long, by='proposal_title' )
  print('here')
  results <- merge(results, visualize_long, by.x = "most_similar_proposal", by.y = "proposal_title" )
  s <- strsplit(results$alltypes.x , split = ", ")
  a <- strsplit(results$alltypes.y , split = ", ")
  
  common_words_list = vector('list', length(s))
  for (i in 1:length(s)) {
    common_words <- pmap(list(s[i], a[i]), intersect)
    common_words_list[[i]] <- common_words
  }
  
  results <- results %>%
    add_column(common_words_list)
  
  results <- results %>% 
    select(proposal_title, most_similar_proposal, common_words_weighted, common_words_list)

  return(results)
}




