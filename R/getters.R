#' Get the expression matrix
#'
#' @description
#' Get the expression matrix from an [Experiment] object.
#'
#' @details
#' A copy, instead of the original matrix, will be returned.
#' This prevents the original matrix from being altered.
#' Alteration of data in an [Experiment] object is strictly managed
#' by the class, so that the expression matrix, sample information,
#' and variable information are always consistent.
#'
#' @param exp The [Experiment] object.
#'
#' @return The expression matrix.
#' @export
get_expr_mat <- function(exp) {
  exp$get_expr_mat()
}


#' Get the sample information
#'
#' @description
#' Get the sample information from an [Experiment] object.
#'
#' @details
#' A copy, instead of the original tibble, will be returned.
#' This prevents the original tibble from being altered.
#' Alteration of data in an [Experiment] object is strictly managed
#' by the class, so that the expression matrix, sample information,
#' and variable information are always consistent.
#'
#' @param exp The [Experiment] object.
#'
#' @return The sample information tibble.
#' @export
get_sample_info <- function(exp) {
  exp$get_sample_info()
}


#' Get the variable information
#'
#' @description
#' Get the variable information from an [Experiment] object.
#'
#' @details
#' A copy, instead of the original tibble, will be returned.
#' This prevents the original tibble from being altered.
#' Alteration of data in an [Experiment] object is strictly managed
#' by the class, so that the expression matrix, sample information,
#' and variable information are always consistent.
#'
#' @param exp The [Experiment] object.
#'
#' @return The variable information tibble.
#' @export
get_var_info <- function(exp) {
  exp$get_var_info()
}
