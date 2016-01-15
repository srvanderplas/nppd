library(tm)
library(stringr)
library(magrittr)
library(dplyr)
library(wordcloud)

#' Modify stem completion to go for the shortest word
#' @param x vector of strings
#' @param dict dictionary
#' @param type Defaults to "shortest"
#' @return PlainTextDocument
#' @export
stemCompletion_mod <- function(x, dict, type = "shortest") {
  x %>% as.character() %>%
    strsplit(split = " ") %>%
    paste() %>%
    tm::stemCompletion(dictionary = dict, type = type) %>%
    paste(sep = "", collapse = " ") %>%
    tm::stripWhitespace() %>%
    tm::PlainTextDocument
}

#' Create a word frequency table from a list of words or phrases
#' @param wordlist vector of strings
#' @param stem T/F - stem?
#' @param rm.stopwords T/F - remove english stopwords?
#' @param stopword.list List of stopwords to remove in addition to english stopwords (only matters if rm.stopwords is T)
#' @return word frequency list
#' @export
MakeWordFreq <- function(wordlist, stem = T, rm.stopwords = T,
                         stopword.list = NULL){
  # Function to make a word frequency table
  wordlist %<>%
    paste(., collapse = " ") %>%
    tm::VectorSource() %>%
    tm::Corpus() %>%
    tm::tm_map(., tm::removeNumbers) %>%
    tm::tm_map(., tm::removePunctuation)

  if (rm.stopwords) {
    wordlist %<>%
      tm::tm_map(., tm::removeWords, tm::stopwords("en"))
  }

  wordlist %<>%
    tm::TermDocumentMatrix %>%
    as.matrix() %>%
    as.data.frame() %>%
    magrittr::set_names("freq")

  if (stem) {
    wordlist %<>%
      dplyr::mutate(original = row.names(.), stems = tm::stemDocument(row.names(.)))
  } else {
    wordlist %<>%
      dplyr::mutate(original = row.names(.), stems = row.names(.))
  }

  wordlist %<>%
    group_by(stems) %>%
    summarize(freq = sum(freq), original = original[which.min(nchar(original))]) %>%
    ungroup() %>%
    select(-stems) %>%
    magrittr::set_names(c("freq", "word")) %>%
    select(word, freq)

  if (rm.stopwords) {
    wordlist %<>%
      subset(!word %in% c(tm::stopwords("en"), stopword.list))
  }

  wordlist %<>%
    dplyr::arrange(desc(freq))
}

#' Function to make a wordcloud
#' @param x word frequency data frame (from MakeWordFreq), with columns "word" and "freq"
#' @param color.set vector of colors
#' @param max.words maximum words
#' @param ... arguments to wordcloud()
#' @return wordcloud
#' @export
MakeWordcloud <- function(x, color.set = brewer.pal(6, "Dark2"), max.words = 50, random.order = F, rot.per = .3, colors = color.set, random.color = T, ...){
  wordcloud::wordcloud(x$word, x$freq, colors = colors, random.color = random.color, max.words = max.words, random.order = random.order, rot.per = rot.per, ...)
}