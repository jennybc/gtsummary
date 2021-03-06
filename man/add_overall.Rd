% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_overall.R
\name{add_overall}
\alias{add_overall}
\title{Add column with overall summary statistics}
\usage{
add_overall(x, last = FALSE)
}
\arguments{
\item{x}{object with class \code{tbl_summary} from the \link{tbl_summary} function}

\item{last}{logical indicator to display overall column last in table.
Default is \code{FALSE}, which will display overall column first.}
}
\description{
Adds a column with overall summary statistics to tables
created by \code{tbl_summary}.
}
\section{Example Output}{

\if{html}{\figure{tbl_overall_ex.png}{options: width=50\%}}
}

\examples{
tbl_overall_ex <-
  trial \%>\%
  dplyr::select(age, response, grade, trt) \%>\%
  tbl_summary(by = "trt") \%>\%
  add_overall()
}
\seealso{
Other tbl_summary tools: \code{\link{add_n}},
  \code{\link{add_p}}, \code{\link{add_q.tbl_summary}},
  \code{\link{add_stat_label}},
  \code{\link{bold_italicize_labels_levels}},
  \code{\link{cols_label_summary}},
  \code{\link{inline_text.tbl_summary}},
  \code{\link{sort_p.tbl_summary}},
  \code{\link{tab_style_bold_p.tbl_summary}},
  \code{\link{tbl_summary}}
}
\author{
Daniel D. Sjoberg
}
\concept{tbl_summary tools}
