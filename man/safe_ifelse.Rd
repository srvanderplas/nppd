% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safeIfElse.R
\name{safe_ifelse}
\alias{safe_ifelse}
\title{Safe (type maintained) version of ifelse}
\usage{
safe_ifelse(cond, yes, no)
}
\arguments{
\item{cond}{logical condition}

\item{yes}{replacement if condition is true}

\item{no}{replacement if condition is false}
}
\description{
Safe (type maintained) version of ifelse
}
\details{
From http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects
}

