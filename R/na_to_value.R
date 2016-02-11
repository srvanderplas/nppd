#' Function to convert all NAs in a vector to a specified value
#'
#' @param vec vector that potentially has NAs
#' @param val value to replace NAs with
#' @return vector with no NAs
#' @export
#' @examples
#' na_to_value(c(1:5, NA, 7:10), 6)
na_to_value <- function(vec, val = "") {
  if (mode(vec) != mode(val) & sum(is.na(vec)) > 0) {
    warning(sprintf("vector will be coerced to %s", mode(val)))
  }

  if (length(val) > 1) {
    warning("Multiple replacement vals provided. This may not work out as anticipated.")
  }

  vec[is.na(vec)] <- rep(val, sum(is.na(vec)))[1:sum(is.na(vec))]

  return(vec)
}

