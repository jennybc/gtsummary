% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-continuous_digits_guess.R
\name{continuous_digits_guess}
\alias{continuous_digits_guess}
\title{Guesses how many digits to use in rounding continuous variables
or summary statistics}
\usage{
continuous_digits_guess(data, variable, summary_type, class,
  digits = NULL)
}
\arguments{
\item{x}{vector containing the values of a continuous variable. This can be
raw data values or a vector of summary statistics themselves}
}
\value{
the rounded values
}
\description{
Guesses how many digits to use in rounding continuous variables
or summary statistics
}
\keyword{internal}