% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_summary.R
\name{tbl_summary}
\alias{tbl_summary}
\title{Create a table of summary statistics}
\usage{
tbl_summary(data, by = NULL, label = NULL, type = NULL,
  value = NULL, statistic = NULL, digits = NULL, group = NULL,
  missing = c("ifany", "always", "no"), missing_text = "Unknown",
  sort = NULL, row_percent = FALSE)
}
\arguments{
\item{data}{a data frame}

\item{by}{a character vector specifying a column in data.
Summary statistics will be calculated separately for each level of the \code{by}
variable. If \code{NULL}, summary statistics
are calculated using all observations.}

\item{label}{list of formulas specifying variables labels,
e.g. \code{list("age" ~ "Age, yrs", "ptstage" ~ "Path T Stage")}.  If \code{NULL}, the
function will take the label attribute (\code{attr(data$age, "label")}).  If
attribute label is \code{NULL}, the variable name will be used.}

\item{type}{list of formulas specifying variable types. Accepted values
are \code{c("continuous", "categorical", "dichotomous")},
e.g. \code{type = list(contains(age) ~ "continuous", "female" ~ "dichotomous")}.
If type not specified for a variable, the function
will default to an appropriate summary type.  See below for details.}

\item{value}{list of formulas specifying the value to display for dichotomous
variables.  See below for details.}

\item{statistic}{list of formulas specifying types of summary statistics to display
for each variable.  The default is
\code{list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}\%)")}.
See below for details.}

\item{digits}{list of formulas specifying the number of decimal
places to round continuous summary statistics. If not specified, \code{tbl_summary} guesses an
appropriate number of decimals to round statistics. When multiple statistics
are displayed for a single variable, supply a vector
rather than an integer.  For example, if the
statistic being calculated is \code{"{mean} ({sd})"} and you want the mean rounded
to 1 decimal place, and the SD to 2 use \code{digits = list("age" ~ c(1, 2))}.}

\item{group}{character vector of an ID or grouping variable for which summary
statistics should not be printed. The column may be used in \link{add_p} to
calculate p-values with correlated data. Default is \code{NULL}}

\item{missing}{indicates whether to include counts of \code{NA} values in the table.
Allowed values are \code{"no"} (never display NA values),
\code{"ifany"} (only display NA if the count is positive), and \code{"always"}
(includes NA count row for all variables). Default is \code{"ifany"}.}

\item{missing_text}{String to display label for count of missing observations.
Default is \code{"Unknown"}.}

\item{sort}{list of formulas specifying the type of sorting to perform. Default is NULL.
Options are 'frequency' where results are sorted in
descending order of frequency and 'alphanumeric',
e.g. \code{sort = list(everything() ~ "frequency")}}

\item{row_percent}{logical value indicating whether to calculate
percentages within column or across rows.  Default is to calculate
percentages within columns: \code{row_percent = FALSE}}
}
\description{
The \code{tbl_summary} function calculates descriptive statistics for
continuous, categorical, and dichotomous variables.  Review the
\href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
for detailed examples.
}
\section{select helpers}{

\href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#select_helpers}{Select helpers}
from the \{tidyselect\} package and \{gtsummary\} package are available to
modify default behavior for groups of variables.
For example, by default continuous variables are reported with the median
and IQR.  To change all continuous variables to mean and standard deviation use
\code{statistic = list(all_continuous() ~ "{mean} ({sd})")}.

All columns with class logical are displayed as dichotomous variables showing
the proportion of events that are \code{TRUE} on a single row. To show both rows
(i.e. a row for \code{TRUE} and a row for \code{FALSE}) use
\code{type = list(all_logical() ~ "categorical")}.

The select helpers are available for use in any argument that accepts
a list of formulas (e.g. \code{statistic}, \code{type}, \code{digits}, \code{value}, \code{sort}, etc.)
}

\section{statistic argument}{

The statistic argument specifies the statistics presented in the table. The
input is a list of formulas that specify the statistics to report. For example,
\code{statistic = list("age" ~ "{mean} ({sd})")} would report the mean and
standard deviation for age; \code{statistic = list(all_continuous() ~ "{mean} ({sd})")}
would report the mean and standard deviation for all continuous variables.
A statistic name that appears between curly brackets
will be replaced with the numeric statistic (see \link[glue:glue]{glue::glue}).

For categorical variables the following statistics are available to display.
\itemize{
\item \code{{n}} frequency
\item \code{{N}} denominator, or cohort size
\item \code{{p}} percent formatted by \link{style_percent}
}
For continuous variables the following statistics are available to display.
\itemize{
\item \code{{median}} median
\item \code{{mean}} mean
\item \code{{sd}} standard deviation
\item \code{{var}} variance
\item \code{{min}} minimum
\item \code{{max}} maximum
\item \code{{p##}} any integer percentile, where \code{##} is an integer from 0 to 100
\item \code{{foo}} any function of the form \code{foo(x)} is accepted where \code{x} is a numeric vector
}
}

\section{type argument}{

tbl_summary displays summary statistics for three types of data:
continuous, categorical, and dichotomous. If the type is not specified,
tbl_summary will do its best to guess the type.  Dichotomous variables
are categorical variables that are displayed on a single row in the
output table, rather than one row per level of the variable.
Variables coded as TRUE/FALSE, 0/1, or yes/no are assumed to be dichotomous,
and the TRUE, 1, and yes rows
will be displayed.  Otherwise, the value to display must be specified in
the \code{value} argument, e.g. \code{value = list("varname" ~ "level to show")}
}

\section{Example Output}{

\if{html}{Example 1}

\if{html}{\figure{tbl_summary_ex1.png}{options: width=31\%}}

\if{html}{Example 2}

\if{html}{\figure{tbl_summary_ex2.png}{options: width=45\%}}
}

\examples{
tbl_summary_ex1 <-
  trial \%>\%
  dplyr::select(age, grade, response) \%>\%
  tbl_summary()

tbl_summary_ex2 <-
  trial \%>\%
  dplyr::select(age, grade, response, trt) \%>\%
  tbl_summary(
    by = "trt",
    label = list("age" ~ "Patient Age"),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    digits = list(vars(age) ~ c(0, 1))
  )
}
\seealso{
See tbl_summary \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed examples

Other tbl_summary tools: \code{\link{add_n}},
  \code{\link{add_overall}}, \code{\link{add_p}},
  \code{\link{add_q.tbl_summary}},
  \code{\link{add_stat_label}},
  \code{\link{bold_italicize_labels_levels}},
  \code{\link{cols_label_summary}},
  \code{\link{inline_text.tbl_summary}},
  \code{\link{sort_p.tbl_summary}},
  \code{\link{tab_style_bold_p.tbl_summary}}
}
\author{
Daniel D. Sjoberg
}
\concept{tbl_summary tools}
