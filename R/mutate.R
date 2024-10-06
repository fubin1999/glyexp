#' Mutate samples based on conditions
#'
#' @description
#' Mutate the sample information tibble using the [dplyr::mutate()] function.
#' For example, `mutate_samples(new_group = if_else(new_col = 1)`
#' will add a new column "new_group" to the sample information tibble.
#'
#' @details
#' Instead of `Experiment$mutate_samples()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with sample_info tibble updated.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
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
#' For example, `mutate_variables(new_group = if_else(new_col = 1)`
#' will add a new column "new_group" to the variable information tibble.
#'
#' @details
#' Instead of `Experiment$mutate_variables()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with variable_info tibble updated.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
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
