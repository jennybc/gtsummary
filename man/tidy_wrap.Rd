% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-tbl_regression.R
\name{tidy_wrap}
\alias{tidy_wrap}
\title{Tidies regression object based on class}
\usage{
tidy_wrap(x, exponentiate, conf.level)
}
\arguments{
\item{x}{regression model object}

\item{exponentiate}{logical argument passed directly to \code{broom::tidy}
or \code{broom.mixed::tidy}.}

\item{conf.level}{confidence level passed directly to \code{broom::tidy}
or \code{broom.mixed::tidy}.}
}
\description{
The \code{tidy_wrap()} function has two primary functions.  First, using either
\code{broom::tidy} or \code{broom.mixed::tidy} (depending on model input class)
the regression model object is converted into a data frame. It then adjusts the
output for use in the rest of \code{\link{tbl_regression}}.
}
\details{
The output of \code{broom::tidy} or \code{broom.mixed::tidy} will often include additional information
that will not be included in a printed table from \code{tbl_regression()}
(e.g. scale parameters, random effects, etc.).  This
simple helper function deletes extraneous rows from the output.
It also adds a column of NAs if the model does not calculate p.values, since
the rest of \code{tbl_regression()} depends on having a column called p.value.
}
\keyword{internal}
