library(nppd)
library(dplyr)
library(stringr)

text <- readLines("VelveteenRabbit.txt")
text.list <- strsplit(text, " ") %>%
  unlist() %>%
  tolower() %>%
  stringr::str_replace_all("[[:punct:]]", "") %>%
  stringr::str_replace_all("\\d", "")



context("Tests MakeWordFreq() function.")

test_that(
  "MakeWordFreq() returns values as expected",
  {
    expect_equal(
      as.character(MakeWordFreq(text.list)[1,1]),
      "rabbit"
    )
    expect_equal(
      as.numeric(MakeWordFreq(text.list)[1,2]),
      75
    )
    }
  )

test_that(
  "MakeWordFreq() returns values of the correct type",
  {
    expect_equal(
      mode(MakeWordFreq(text.list)),
      "list"
    )
    expect_equal(
      class(MakeWordFreq(text.list)),
      "data.frame"
    )
  }
)

test_that(
  "MakeWordFreq() issues warning correctly on type mismatch",
  expect_warning(
    MakeWordFreq(wordlist = factor(c("a", "b", "c"))),
    "Attempting to convert wordlist to character vector"
  )
)

test_that(
  "MakeWordFreq() issues error if vector is empty after cleaning.",
    expect_error(
      MakeWordFreq(wordlist = c(".", ",", " ")),
      "Wordlist has no words after removing punctuation, numbers, and extra spaces."
    )
)


test_that(
  "MakeWordFreq() stems words.",
  {
    expect_equal(
      MakeWordFreq(c("jump", "jumping", "jumped"), stem = T)$word,
      "jump"
    )
    expect_equal(
      MakeWordFreq(c("jump", "jumping", "jumped"), stem = T)$freq,
      3
    )
  }
)

test_that(
  "MakeWordFreq() removes stopwords.",
  {
    expect_equal(
      MakeWordFreq(c("jump", "jumping", "jumped", "a"), rm.stopwords = T)$word,
      "jump"
    )
    expect_equal(
      MakeWordFreq(c("jump", "jumping", "jumped", "a"), rm.stopwords = F)$word,
      c("jump", "a")
    )
    expect_equal(
      MakeWordFreq(c("jump", "jumping", "jumped", "engineer", "engineering"), stem = T, rm.stopwords = T, stopword.list = "engineer")$word,
      "jump"
    )
    expect_equal(
      MakeWordFreq(c("jump", "jumping", "jumped", "engineer", "engineering"), stem = F, rm.stopwords = T, stopword.list = "engineer")$word,
      c("engineering", "jump", "jumped", "jumping")
    )
  }
)
