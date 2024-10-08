#' Select columns of the sample information tibble
#'
#' @description
#' Select columns of the sample information tibble of an [Experiment] object
#' using <[`tidy-select`][dplyr_tidy_select]> syntax.
#' For example, `select_samples(exp, group)` will return a new [Experiment] object
#' with a `sample_info` tibble having only the `sample` and `group` columns.
#' (Note that the `sample` column is always included automatically.)
#'
#' @details
#' Instead of `Experiment$select_samples()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with `sample_info` tibble updated.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
#'
#' You should not, and don't need to, select or de-select the `sample` column.
#' The `sample` column is always included automatically.
#'
#' @param exp The [Experiment] object.
#' @param ... <[`tidy-select`][dplyr_tidy_select]> syntax for selecting columns.
#'
#' @return A new [Experiment] object.
#' @export
select_samples <- function(exp, ...) {
  new_exp <- exp$clone()
  new_exp$select_samples(...)
}


#' Select columns of the variable information tibble
#'
#' @description
#' Select columns of the variable information tibble of an [Experiment] object
#' using <[`tidy-select`][dplyr_tidy_select]> syntax.
#' For example, `select_variables(exp, type)` will return a new [Experiment] object
#' with a `var_info` tibble having only the `variable` and `type` columns.
#' (Note that the `variable` column is always included automatically.)
#'
#' @details
#' Instead of `Experiment$select_variables()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with `var_info` tibble updated.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
#'
#' You should not, and don't need to, select or de-select the `variable` column.
#' The `variable` column is always included automatically.
#'
#' @param exp The [Experiment] object.
#' @param ... <[`tidy-select`][dplyr_tidy_select]> syntax for selecting columns.
#'
#' @return A new [Experiment] object.
#' @export
select_variables <- function(exp, ...) {
  new_exp <- exp$clone()
  new_exp$select_variables(...)
}
