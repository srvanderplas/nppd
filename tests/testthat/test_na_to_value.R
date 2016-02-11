library(nppd)
library(dplyr)

context("Tests na_to_value() function.")

test_that(
  "na_to_value() converts NA to supplied (or empty) string",
  {
    expect_equal(
      sum(na_to_value(c(NA, "helloworld")) == c("", "helloworld")),
      2
    )
    expect_equal(
      sum(na_to_value(c(NA, "helloworld"), "helloworld") == "helloworld"),
      2
    )
  }
)

test_that(
  "na_to_value() issues warning correctly on type mismatch",
  {
    expect_warning(
      na_to_value(c(NA, 3), "a"),
      "vector will be coerced to character"
    )
    expect_warning(
      na_to_value(c(NA, "A"), 3),
      "vector will be coerced to numeric"
    )
  }
)

test_that(
  "na_to_value() removes all NAs",
  {
    expect_equal(
      na_to_value(c(1:5, NA, 7:10), 6),
      1:10
    )
    expect_equal(
      na_to_value(c(letters[1:10], NA), "k"),
      letters[1:11]
    )
    expect_equal(
      na_to_value(c(letters[1:10], NA), "k") %>%
        is.na() %>% sum(),
      0
    )
  }
)

test_that(
  "na_to_value issues warning correctly when multiple replacement values are provided.",
  {
    expect_warning(
      na_to_value(c(1:5, NA), 10:12),
      "Multiple replacement vals provided. This may not work out as anticipated."
    )
  }
)