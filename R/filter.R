#' Filter samples based on conditions
#'
#' @description
#' Filter samples based on conditions specified in the [dplyr::filter()] function.
#' For example, `filter_samples(exp, group == "A")` will keep samples in `exp`
#' with "group" equal to "A".
#'
#' @details
#' Instead of `Experiment$filter_samples()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with samples filtered.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
#'
#' @param exp The [Experiment] object.
#' @param ... Conditions for filtering samples, passed to [dplyr::filter()].
#'
#' @return The [Experiment] object.
#' @export
filter_samples <- function(exp, ...) {
  new_exp <- exp$clone()
  new_exp$filter_samples(...)
}


#' Filter variables based on conditions
#'
#' @description
#' Filter variables based on conditions specified in the [dplyr::filter()] function.
#' For example, `filter_variables(exp, type == "B")` will keep variables in `exp`
#' with "type" equal to "B".
#'
#' @details
#' Instead of `Experiment$filter_variables()`, which updates the [Experiment] object in place,
#' this function returns a new `Experiment` object with variables filtered.
#' The original `Experiment` object will not be altered.
#' Besides, as the first argument is the `Experiment` object,
#' this function could be used in a `magrittr` pipe.
#'
#' @param exp The [Experiment] object.
#' @param ... Conditions for filtering variables, passed to [dplyr::filter()].
#'
#' @return The [Experiment] object.
#' @export
filter_variables <- function(exp, ...) {
  new_exp <- exp$clone()
  new_exp$filter_variables(...)
}
