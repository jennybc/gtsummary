% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tbl_stack.R
\name{tbl_stack}
\alias{tbl_stack}
\title{Stacks two or more gtsummary regression objects}
\usage{
tbl_stack(tbls)
}
\arguments{
\item{tbls}{list of gtsummary regression objects}
}
\description{
Assists in patching together more complex tables. \code{tbl_stack()} appends two
or more \code{tbl_regression} or \code{tbl_merge} objects.
{gt} attributes from the first regression object are utilized for output table.
If combining \code{tbl_stack()} and \code{tbl_merge()}, merge first then stack, or stack
first and then merge.
}
\section{Example Output}{

\if{html}{Example 1}

\if{html}{\figure{tbl_stack_ex1.png}{options: width=50\%}}

\if{html}{Example 2}

\if{html}{\figure{tbl_stack_ex2.png}{options: width=80\%}}
}

\examples{
# Example 1 - stacking two tbl_regression objects
t1 <-
  glm(response ~ trt, trial, family = binomial) \%>\%
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t2 <-
  glm(response ~ trt + grade + stage + marker, trial, family = binomial) \%>\%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )

tbl_stack_ex1 <- tbl_stack(list(t1, t2))

# Example 2 - stacking two tbl_merge objects
library(survival)
t3 <-
  coxph(Surv(ttdeath, death) ~ trt, trial) \%>\%
  tbl_regression(
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (unadjusted)")
  )

t4 <-
  coxph(Surv(ttdeath, death) ~ trt + grade + stage + marker, trial) \%>\%
  tbl_regression(
    include = "trt",
    exponentiate = TRUE,
    label = list("trt" ~ "Treatment (adjusted)")
  )


# first merging, then stacking
row1 <- tbl_merge(list(t1, t3), tab_spanner = c("Tumor Response", "Death"))
row2 <- tbl_merge(list(t2, t4))
tbl_stack_ex2 <-
  tbl_stack(list(row1, row2)) \%>\%
  as_gt() \%>\%
  tab_footnote(
    footnote = "Adjusted for cancer grade, state, and marker level.",
    locations = cells_data(columns = "label", rows = 4)
  )
}
\seealso{
\link{tbl_merge}

Other tbl_regression tools: \code{\link{add_global_p.tbl_regression}},
  \code{\link{add_nevent.tbl_regression}},
  \code{\link{bold_italicize_labels_levels}},
  \code{\link{inline_text.tbl_regression}},
  \code{\link{sort_p.tbl_regression}},
  \code{\link{tab_style_bold_p.tbl_regression}},
  \code{\link{tbl_merge}}, \code{\link{tbl_regression}}

Other tbl_uvregression tools: \code{\link{add_global_p.tbl_uvregression}},
  \code{\link{add_nevent.tbl_uvregression}},
  \code{\link{add_q.tbl_uvregression}},
  \code{\link{bold_italicize_labels_levels}},
  \code{\link{inline_text.tbl_uvregression}},
  \code{\link{sort_p.tbl_uvregression}},
  \code{\link{tab_style_bold_p.tbl_uvregression}},
  \code{\link{tbl_merge}}, \code{\link{tbl_uvregression}}
}
\author{
Daniel D. Sjoberg
}
\concept{tbl_regression tools}
\concept{tbl_uvregression tools}
