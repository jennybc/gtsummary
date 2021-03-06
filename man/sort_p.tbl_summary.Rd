% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_p.R
\name{sort_p.tbl_summary}
\alias{sort_p.tbl_summary}
\title{Sort variables in table by ascending p-values}
\usage{
\method{sort_p}{tbl_summary}(x, q = FALSE, ...)
}
\arguments{
\item{x}{an object created using \code{tbl_summary} function}

\item{q}{logical argument. When TRUE will sort by the q-value column rather than the p-values}

\item{...}{not used}
}
\description{
Sort variables in tables created by \code{\link{tbl_summary}} by ascending p-values
}
\section{Example Output}{

\if{html}{\figure{tbl_sum_sort_p_ex.png}{options: width=50\%}}
}

\examples{
tbl_sum_sort_p_ex <-
  trial \%>\%
  dplyr::select(age, grade, response, trt) \%>\%
  tbl_summary(by = "trt") \%>\%
  add_p() \%>\%
  sort_p()
}
\seealso{
Other tbl_summary tools: \code{\link{add_n}},
  \code{\link{add_overall}}, \code{\link{add_p}},
  \code{\link{add_q.tbl_summary}},
  \code{\link{add_stat_label}},
  \code{\link{bold_italicize_labels_levels}},
  \code{\link{cols_label_summary}},
  \code{\link{inline_text.tbl_summary}},
  \code{\link{tab_style_bold_p.tbl_summary}},
  \code{\link{tbl_summary}}
}
\author{
Karissa Whiting
}
\concept{tbl_summary tools}
