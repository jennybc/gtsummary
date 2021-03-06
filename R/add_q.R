#' Add a column of q values to account for
#' multiple comparisons
#'
#' @param x `tbl_summary` or `tbl_uvregression` object
#' @param ... further arguments passed to or from other methods.
#' @author Esther Drill, Daniel D. Sjoberg
#' @seealso \code{\link{add_q.tbl_summary}}, \code{\link{add_q.tbl_uvregression}},
#' \code{\link{tbl_summary}}, \code{\link{tbl_uvregression}}
#' @export
add_q <- function(x, ...) UseMethod("add_q")

#' Add a column of q values to account for
#' multiple comparisons
#'
#' The adjustments to the p-values is performed with
#' [stats::p.adjust].
#'
#' @param x `tbl_summary` object
#' @param method character argument indicating method to be used for p-value
#' adjustment. Methods from
#' [stats::p.adjust] are accepted.  Default is `method = 'fdr'`.
#' @inheritParams tbl_regression
#' @param ...	further arguments passed to or from other methods
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_summary tools
#' @export
#' @examples
#' tbl_sum_q_ex <-
#'   trial %>%
#'   dplyr::select(trt, age, grade, response) %>%
#'   tbl_summary(by = "trt") %>%
#'   add_p() %>%
#'   add_q()
#' @section Example Output:
#' \if{html}{\figure{tbl_sum_q_ex.png}{options: width=50\%}}

add_q.tbl_summary <- function(x, method = "fdr", pvalue_fun = x$pvalue_fun, ...) {

  # This adjusts p-values for multiple testing. Default method is fdr.
  if (!("add_p" %in% names(x$call_list))) {
    stop(glue(
      "There are no p-values yet. You need to use the function add_p(), ",
      "after tbl_summary() and before add_q()"
    ))
  }

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate(
      q.value = stats::p.adjust(.data$p.value, method = method)
    )

  # adding q value to summary table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "q.value")) %>%
        mutate(row_type = "label"),
      by = c("variable", "row_type")
    )

  # keep track of what functions have been called
  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # returning qvalue method
  x$qvalue_method <- method

  # footnote text
  footnote_text <-
    method %>%
    {
      filter(add_q_method_lookup, !!parse_expr(glue("method == '{.}'")))
    } %>%
    pull("method_label")

  x$qvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt_qvalue"]] <-
    "fmt(columns = vars(q.value), rows = !is.na(q.value), fns = x$qvalue_fun)"
  # column headers
  x[["gt_calls"]][["cols_label_qvalue"]] <-
    "cols_label(q.value = md('**q-value**'))"
  # column headers abbreviations footnote
  x[["gt_calls"]][["footnote_q_method"]] <- glue(
    "tab_footnote(",
    "  footnote = '{footnote_text}',",
    "  locations = cells_column_labels(",
    "    columns = vars(q.value))",
    ")"
  )

  # Returns the table 1 object
  return(x)
}


#' Add a column of q values to account for
#' multiple comparisons
#'
#' The adjustments to the p-values is performed with
#' [stats::p.adjust].
#'
#' @param x `tbl_uvregression` object
#' @param method character argument indicating method to be used for p-value adjustment.
#' Methods from [stats::p.adjust] are accepted. Default is `method = 'fdr'`.
#' @inheritParams tbl_regression
#' @param ...	further arguments passed to or from other methods
#' @author Esther Drill, Daniel D. Sjoberg
#' @family tbl_uvregression tools
#' @export
#' @examples
#' tbl_uvr_q_ex <-
#'   trial %>%
#'   dplyr::select(age, marker, grade, response) %>%
#'   tbl_uvregression(
#'     method = lm,
#'     y = age
#'   ) %>%
#'   add_global_p() %>%
#'   add_q()
#' @section Example Output:
#' \if{html}{\figure{tbl_uvr_q_ex.png}{options: width=50\%}}

add_q.tbl_uvregression <- function(x, method = "fdr",
                                   pvalue_fun = x$inputs$pvalue_fun, ...) {

  # This adjusts p-values for multiple testing but only when the global approach is used.
  # Default method is fdr.
  if (!("p.value_global" %in% colnames(x$meta_data))) {
    stop(glue(
      "You need global p-values first. Use the function add_global_p() after ",
      "tbl_uvregression() and before add_q()"
    ))
  }

  # checking pvalue_fun are functions
  if (!is.function(pvalue_fun)) {
    stop("Input 'pvalue_fun' must be a function.")
  }

  # adding exact and printable q value to meta_data
  x$meta_data <-
    x$meta_data %>%
    mutate(
      q.value_global = stats::p.adjust(.data$p.value_global, method = method)
    )

  # adding q value to display table
  x$table_body <-
    x$table_body %>%
    left_join(
      x$meta_data %>%
        select(c("variable", "q.value_global")) %>%
        set_names(c("variable", "q.value")) %>%
        mutate(row_type = "label"),
      by = c("variable", "row_type")
    )

  x$call_list <- c(x$call_list, list(add_q = match.call()))

  # returning qvalue method
  x$qvalue_method <- method

  # footnote text
  footnote_text <-
    method %>%
    {
      filter(add_q_method_lookup, !!parse_expr(glue("method == '{.}'")))
    } %>%
    pull("method_label")

  x$qvalue_fun <- pvalue_fun
  # adding p-value formatting
  x[["gt_calls"]][["fmt_qvalue"]] <-
    "fmt(columns = vars(q.value), rows = !is.na(q.value), fns = x$qvalue_fun)" %>%
    glue()
  # column headers
  x[["gt_calls"]][["cols_label_qvalue"]] <-
    "cols_label(q.value = md('**q-value**'))" %>%
    glue()
  # column headers abbreviations footnote
  x[["gt_calls"]][["footnote_q_method"]] <- glue(
    "tab_footnote(",
    "footnote = '{footnote_text}',",
    "locations = cells_column_labels(",
    "columns = vars(q.value))",
    ")"
  )

  return(x)
}


# match method input to display name
add_q_method_lookup <- tibble::tribble(
  ~method, ~method_label,
  "holm", "Holm correction for multiple testing",
  "hochberg", "Hochberg correction for multiple testing",
  "hommel", "Hommel correction for multiple testing",
  "bonferroni", "Bonferroni correction for multiple testing",
  "BH", "Benjamini & Hochberg correction for multiple testing",
  "BY", "Benjamini & Yekutieli correction for multiple testing",
  "fdr", "False discovery rate correction for multiple testing",
  "none", "No correction for multiple testing"
)
