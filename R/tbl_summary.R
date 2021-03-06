#' Create a table of summary statistics
#'
#' The `tbl_summary` function calculates descriptive statistics for
#' continuous, categorical, and dichotomous variables.  Review the
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{tbl_summary vignette}
#' for detailed examples.
#'
#' @param data a data frame
#' @param by a character vector specifying a column in data.
#' Summary statistics will be calculated separately for each level of the `by`
#' variable. If `NULL`, summary statistics
#' are calculated using all observations.
#' @param label list of formulas specifying variables labels,
#' e.g. `list("age" ~ "Age, yrs", "ptstage" ~ "Path T Stage")`.  If `NULL`, the
#' function will take the label attribute (`attr(data$age, "label")`).  If
#' attribute label is `NULL`, the variable name will be used.
#' @param type list of formulas specifying variable types. Accepted values
#' are `c("continuous", "categorical", "dichotomous")`,
#' e.g. `type = list(contains(age) ~ "continuous", "female" ~ "dichotomous")`.
#' If type not specified for a variable, the function
#' will default to an appropriate summary type.  See below for details.
#' @param value list of formulas specifying the value to display for dichotomous
#' variables.  See below for details.
#' @param statistic list of formulas specifying types of summary statistics to display
#' for each variable.  The default is
#' `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
#' See below for details.
#' @param digits list of formulas specifying the number of decimal
#' places to round continuous summary statistics. If not specified, `tbl_summary` guesses an
#' appropriate number of decimals to round statistics. When multiple statistics
#' are displayed for a single variable, supply a vector
#' rather than an integer.  For example, if the
#' statistic being calculated is `"{mean} ({sd})"` and you want the mean rounded
#' to 1 decimal place, and the SD to 2 use `digits = list("age" ~ c(1, 2))`.
#' @param group character vector of an ID or grouping variable for which summary
#' statistics should not be printed. The column may be used in [add_p] to
#' calculate p-values with correlated data. Default is `NULL`
#' @param missing indicates whether to include counts of `NA` values in the table.
#' Allowed values are `"no"` (never display NA values),
#' `"ifany"` (only display NA if the count is positive), and `"always"`
#' (includes NA count row for all variables). Default is `"ifany"`.
#' @param missing_text String to display label for count of missing observations.
#' Default is `"Unknown"`.
#' @param sort list of formulas specifying the type of sorting to perform. Default is NULL.
#' Options are 'frequency' where results are sorted in
#' descending order of frequency and 'alphanumeric',
#' e.g. `sort = list(everything() ~ "frequency")`
#' @param row_percent logical value indicating whether to calculate
#' percentages within column or across rows.  Default is to calculate
#' percentages within columns: `row_percent = FALSE`
#'
#' @section select helpers:
#' \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html#select_helpers}{Select helpers}
#' from the \\{tidyselect\\} package and \\{gtsummary\\} package are available to
#' modify default behavior for groups of variables.
#' For example, by default continuous variables are reported with the median
#' and IQR.  To change all continuous variables to mean and standard deviation use
#' `statistic = list(all_continuous() ~ "{mean} ({sd})")`.
#'
#' All columns with class logical are displayed as dichotomous variables showing
#' the proportion of events that are `TRUE` on a single row. To show both rows
#' (i.e. a row for `TRUE` and a row for `FALSE`) use
#' `type = list(all_logical() ~ "categorical")`.
#'
#' The select helpers are available for use in any argument that accepts
#' a list of formulas (e.g. `statistic`, `type`, `digits`, `value`, `sort`, etc.)
#'
#' @section statistic argument:
#' The statistic argument specifies the statistics presented in the table. The
#' input is a list of formulas that specify the statistics to report. For example,
#' `statistic = list("age" ~ "{mean} ({sd})")` would report the mean and
#' standard deviation for age; `statistic = list(all_continuous() ~ "{mean} ({sd})")`
#' would report the mean and standard deviation for all continuous variables.
#'  A statistic name that appears between curly brackets
#' will be replaced with the numeric statistic (see [glue::glue]).
#'
#' For categorical variables the following statistics are available to display.
#' \itemize{
#'   \item `{n}` frequency
#'   \item `{N}` denominator, or cohort size
#'   \item `{p}` percent formatted by [style_percent]
#' }
#' For continuous variables the following statistics are available to display.
#' \itemize{
#'   \item `{median}` median
#'   \item `{mean}` mean
#'   \item `{sd}` standard deviation
#'   \item `{var}` variance
#'   \item `{min}` minimum
#'   \item `{max}` maximum
#'   \item `{p##}` any integer percentile, where `##` is an integer from 0 to 100
#'   \item `{foo}` any function of the form `foo(x)` is accepted where `x` is a numeric vector
#' }
#'
#' @section type argument:
#' tbl_summary displays summary statistics for three types of data:
#' continuous, categorical, and dichotomous. If the type is not specified,
#' tbl_summary will do its best to guess the type.  Dichotomous variables
#' are categorical variables that are displayed on a single row in the
#' output table, rather than one row per level of the variable.
#' Variables coded as TRUE/FALSE, 0/1, or yes/no are assumed to be dichotomous,
#' and the TRUE, 1, and yes rows
#' will be displayed.  Otherwise, the value to display must be specified in
#' the `value` argument, e.g. `value = list("varname" ~ "level to show")`
#' @export
#' @family tbl_summary tools
#' @seealso See tbl_summary \href{http://www.danieldsjoberg.com/gtsummary/articles/tbl_summary.html}{vignette} for detailed examples
#' @author Daniel D. Sjoberg
#' @examples
#' tbl_summary_ex1 <-
#'   trial %>%
#'   dplyr::select(age, grade, response) %>%
#'   tbl_summary()
#'
#' tbl_summary_ex2 <-
#'   trial %>%
#'   dplyr::select(age, grade, response, trt) %>%
#'   tbl_summary(
#'     by = "trt",
#'     label = list("age" ~ "Patient Age"),
#'     statistic = list(all_continuous() ~ "{mean} ({sd})"),
#'     digits = list(vars(age) ~ c(0, 1))
#'   )
#' @section Example Output:
#' \if{html}{Example 1}
#'
#' \if{html}{\figure{tbl_summary_ex1.png}{options: width=31\%}}
#'
#' \if{html}{Example 2}
#'
#' \if{html}{\figure{tbl_summary_ex2.png}{options: width=45\%}}
#'
tbl_summary <- function(data, by = NULL, label = NULL, type = NULL, value = NULL,
                        statistic = NULL, digits = NULL, group = NULL,
                        missing = c("ifany", "always", "no"),
                        missing_text = "Unknown", sort = NULL,
                        row_percent = FALSE) {
  missing <- match.arg(missing)
  # ungrouping data
  data <- data %>% ungroup()

  # will return call, and all object passed to in tbl_summary call
  # the object func_inputs is a list of every object passed to the function
  tbl_summary_inputs <- as.list(environment())

  # removing variables with unsupported variable types from data
  classes_expected <- c("character", "factor", "numeric", "logical", "integer")
  var_to_remove <-
    map_lgl(data, ~ class(.x) %in% classes_expected %>% any()) %>%
    discard(. == TRUE) %>%
    names()
  data <- data %>% dplyr::select(-var_to_remove)
  if (length(var_to_remove) > 0) {
    var_to_remove_quoted <- paste0("'", var_to_remove, "'")
    classes_expected_quoted <- paste0("'", classes_expected, "'")
    message(glue(
      "Column(s) {glue_collapse(var_to_remove_quoted, sep = ', ', last = ', and ')} ",
      "omitted from output. ",
      "Expecting class {glue_collapse(classes_expected_quoted, sep = ', ', last = ', or ')}."
    ))
  }

  # checking function inputs
  tbl_summary_input_checks(
    data, by, label, type, value, statistic,
    digits, missing, missing_text, group, sort
  )

  # converting tidyselect formula lists to named lists
  type <- tidyselect_to_list(data, type)
  value <- tidyselect_to_list(data, value)

  # creating a table with meta data about each variable
  meta_data <- tibble(
    variable = names(data),
    # assigning class, if entire var is NA, then assigning class NA
    class = assign_class(data, .data$variable),
    summary_type = assign_summary_type(
      data, .data$variable, .data$class, type, value
    )
  )
  # excluding by variable
  if (!is.null(by)) meta_data <- meta_data %>% filter(!!parse_expr("variable != by"))
  # excluding id variable
  if (!is.null(group)) meta_data <- meta_data %>% filter(!!parse_expr("!variable %in% group"))

  # converting tidyselect formula lists to named lists
  label <- tidyselect_to_list(data, label, .meta_data = meta_data)
  statistic <- tidyselect_to_list(data, statistic, .meta_data = meta_data)
  digits <- tidyselect_to_list(data, digits, .meta_data = meta_data)
  sort <- tidyselect_to_list(data, sort, .meta_data = meta_data)

  # assigning variable characteristics
  meta_data <-
    meta_data %>%
    mutate(
      dichotomous_value = assign_dichotomous_value(data, .data$variable, .data$summary_type, .data$class, value),
      var_label = assign_var_label(data, .data$variable, label),
      stat_display = assign_stat_display(.data$variable, .data$summary_type, statistic),
      stat_label = stat_label_match(.data$stat_display),
      digits = continuous_digits_guess(
        data, .data$variable, .data$summary_type, .data$class, digits
      ),
      sort = assign_sort(.data$variable, .data$summary_type, sort)
    )

  # calculating summary statistics
  table_body <-
    meta_data %>%
    mutate(
      # creating summary stat table formatted properly
      stat_table = pmap(
        list(
          .data$variable, .data$summary_type, .data$dichotomous_value,
          .data$var_label, .data$stat_display, .data$digits, .data$class,
          .data$sort
        ),
        ~ calculate_summary_stat(
          data,
          variable = ..1, by = get("by"), summary_type = ..2,
          dichotomous_value = ..3, var_label = ..4, stat_display = ..5,
          digits = ..6, class = ..7, missing = missing,
          missing_text = missing_text, sort = ..8,
          row_percent = row_percent
        )
      )
    ) %>%
    select(c("variable", "summary_type", "stat_table")) %>%
    unnest(!!sym("stat_table"))

  # returning all results in a list
  results <- list(
    gt_calls = eval(gt_tbl_summary),
    table_body = table_body %>% select(-.data$summary_type),
    meta_data = meta_data,
    inputs = tbl_summary_inputs,
    call_list = list(tbl_summary = match.call())
  )

  if (!is.null(by)) {
    results[["by"]] <- by
    results[["df_by"]] <- df_by(data, by)

    # if there are 10 or more by levels, they are sorted incorrectly...fixing order
    stat_var_sort <- results[["df_by"]]$by_col
    results[["table_body"]] <-
      results[["table_body"]] %>%
      select(-stat_var_sort, stat_var_sort)
  }

  # assigning a class of tbl_summary (for special printing in Rmarkdown)
  class(results) <- "tbl_summary"

  # adding headers
  if (is.null(by)) {
    results <- cols_label_summary(results, stat_overall = md("**N = {N}**"))
  } else {
    results <- cols_label_summary(results, stat_by = md("**{level}**, N = {n}"))
  }

  return(results)
}

# gt function calls ------------------------------------------------------------
# quoting returns an expression to be evaluated later
gt_tbl_summary <- quote(list(
  # first call to the gt function
  gt = glue("gt(data = x$table_body)"),

  # column headers
  cols_label_label = glue("cols_label(label = md('**Characteristic**'))"),

  # label column indented and left just
  cols_align = glue(
    "cols_align(align = 'center') %>% ",
    "cols_align(align = 'left', columns = vars(label))"
  ),

  # do not print columns variable or row_type columns
  cols_hide = glue("cols_hide(columns = vars(variable, row_type))"),

  # NAs do not show in table
  fmt_missing = glue("fmt_missing(columns = everything(), missing_text = '')"),

  # indenting levels and missing rows
  tab_style_text_indent = glue(
    "tab_style(",
    "style = cell_text(indent = px(10), align = 'left'),",
    "locations = cells_data(",
    "columns = vars(label),",
    "rows = row_type != 'label'",
    "))"
  ),

  # adding footnote listing statistics presented in table
  footnote_stat_label = glue(
    "tab_footnote(",
    "footnote = '{footnote_stat_label(meta_data)}',",
    "locations = cells_column_labels(",
    "columns = vars(label))",
    ")"
  )
))
