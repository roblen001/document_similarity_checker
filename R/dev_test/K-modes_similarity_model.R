# fitting kmeans for the proposal similarity data frame

library(klaR)

# Pre-processing function in the similarity checker app
getCleanedAndTokenizedData <- function(corpus_raw, custom_bigram_stop_words, custom_stop_words, type) {
  # Unsure how to import the stop_words data without library()
  library(tidytext)
  library(pluralize)

  data(stop_words)
  corpus_clean <- corpus_raw %>%
    tidytext::unnest_tokens(word, text, token = "ngrams", n = 3)%>%
    tidyr::separate(word, c("word1", "word2", "word3"), sep = " ") %>%
    dplyr::filter(!word1 %in% c(stop_words$word, custom_stop_words)) %>%
    dplyr::filter(!word2 %in% c(stop_words$word, custom_stop_words)) %>%
    dplyr::filter(!word3 %in% c(stop_words$word, custom_stop_words)) %>%
    tidyr::unite(word, word1, word2, word3, sep = " ") %>%
    dplyr::filter(!word %in% custom_bigram_stop_words)

  corpus_clean$word <- singularize(corpus_clean$word)

  corpus_clean <- corpus_clean %>%
    group_by(id) %>%
    dplyr::count(word, sort = TRUE)

  corpus_clean <- corpus_clean %>%
    tidyr::pivot_wider(names_from = word, values_from = n)

  # remove cols with less then 2 NAs
  visualize <- corpus_clean[, which(colMeans(!is.na(corpus_clean)) > 1/length(corpus_clean$id))]


  return(visualize)
}

DATA_PATH <- "C:/Users/Roberto/Downloads/ONDRISubmissionPorta_DATA_2021-07-14_1025.csv"

corpus_raw <- readr::read_csv(DATA_PATH)
# the inputed dataframe should always have the same ordering ID, Author, Title, Background information
colnames(corpus_raw) <- c('id', 'author', 'proposal_title', 'background', 'hypothesis', 'variables', 'analysis', 'significance',
                          'Keyword 1', 'Keyword 2', 'Keyword 3', 'Keyword 4', 'Keyword 5', 'status')

corpus_raw$text<- with(corpus_raw, paste0(background, hypothesis, significance))
corpus_raw <- corpus_raw %>%
  dplyr::select('id', 'author', 'proposal_title', 'text', 'status')

# Stop words, Derek had "ONDRI" and "Ontario Neurodegenerative Disease Research Ininative"
#  as stop words

custom_bigram_stop_words <- c('university press', 'citing article', 'https www.tandfonline.com', 'NA NA')
custom_stop_words <- c('copyright', 'https', 'NA', 'doi')

corpus_cleaned <- getCleanedAndTokenizedData(corpus_raw, custom_bigram_stop_words, custom_stop_words, type='OtherProposals')

corpus_cleaned_noID <- subset(corpus_cleaned, select = -c(id) )

corpus_cleaned_noID[is.na(corpus_cleaned_noID)] <- 0

corpus_cleaned_noID[corpus_cleaned_noID > 0] <- 1

# corpus_cleaned <- cbind(corpus_cleaned_noID, corpus_cleaned$id)

# do I fit my kmodes by the count of the word found in each article or just by the words
# do I keep the count to 1 or do we want each occurence within the proposal
# the second option might make certain proposals more closely related if they both have
# higher occurences of certain words which I believe makes sense
model <- kmodes(corpus_cleaned_noID, 10)


## and visualize with some jitter:
plot(jitter(as.matrix(corpus_cleaned_noID)), col = model$cluster)
points(model$modes, col = 1:10, pch = 8)


# # NOT RUN {
# ### a 5-dimensional toy-example:
#
# ## generate data set with two groups of data:
# set.seed(1)
# x <- rbind(matrix(rbinom(250, 2, 0.25), ncol = 5),
#            matrix(rbinom(250, 2, 0.75), ncol = 5))
# colnames(x) <- c("a", "b", "c", "d", "e")
#
# ## run algorithm on x:
# (cl <- kmodes(x, 2))
#
# ## and visualize with some jitter:
# plot(jitter(x), col = cl$cluster)
# points(cl$modes, col = 1:5, pch = 8)
# # }
