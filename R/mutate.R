#' Mutate samples based on conditions
#'
#' @description
#' Mutate the sample information tibble using the [dplyr::mutate()] function.
#' For example, `mutate_samples(exp, new_group = if_else(new_col = 1)`
#' will add a new column "new_group" to the sample information tibble of `exp`.
#'
#' @details
#' Instead of `Experiment$mutate_samples()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with sample_info tibble updated.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
#'
#' The columns "sample" is protected from being modified or renamed.
#' It is essential for keeping the linkage between the sample information and the expression matrix.
#' You may add new columns by transforming the "sample" column.
#'
#' @param exp The [Experiment] object.
#' @param ... Name-value pairs, passed to [dplyr::mutate()].
#'
#' @return The new [Experiment] object.
#' @export
mutate_samples <- function(exp, ...) {
  new_exp <- exp$clone()
  new_exp$mutate_samples(...)
}


#' Mutate variables based on conditions
#'
#' @description
#' Mutate the variable information tibble using the [dplyr::mutate()] function.
#' For example, `mutate_variables(exp, new_group = if_else(new_col = 1)`
#' will add a new column "new_group" to the variable information tibble of `exp`.
#'
#' @details
#' Instead of `Experiment$mutate_variables()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with variable_info tibble updated.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
#'
#' The columns "variable" is protected from being modified or renamed.
#' It is essential for keeping the linkage between the variable information and the expression matrix.
#' You may add new columns by transforming the "variable" column.
#'
#' @param exp The [Experiment] object.
#' @param ... Name-value pairs, passed to [dplyr::mutate()].
#'
#' @return The new [Experiment] object.
#' @export
mutate_variables <- function(exp, ...) {
  new_exp <- exp$clone()
  new_exp$mutate_variables(...)
}
