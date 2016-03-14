#' Safe (type maintained) version of ifelse
#' @param cond logical condition
#' @param yes replacement if condition is true
#' @param no replacement if condition is false
#' @details From http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
#' @export
safe_ifelse <- function(cond, yes, no) {
  structure(ifelse(cond, yes, no), class = class(yes))
}