#' R6 Class for a Glycoproteomics or Glycomics Experiment
#'
#' @description
#' A class managing expression matrix, sample information, and variable information.
#'
#' @details
#' This class synchronizes three data types commonly used in a glycproteomics
#' or glycomics research: the expression matrix, the sample information, and
#' the variable information. It helps ensure that the three datasets altered
#' in sync. For example, filtering samples based on some conditions will
#' automatically affect both the expression matrix and the sample information.
Experiment <- R6::R6Class(
  "Experiment",

  public = list(

    #' @field name Name of the experiment.
    name = NULL,

    #' @description
    #' Create a new experiment object.
    #' @param name Name of the experiment.
    #' @param expr_mat Expression matrix. Rows are variables and columns are samples.
    #' @param sample_info Sample information, a tibble or a data.frame.
    #'    The first column should be "sample", corresponding to the column names of `expr_mat`.
    #'    The order of samples doesn't matter.
    #' @param var_info Variable information, a tibble or a data.frame.
    #'    The first column should be "variable", corresponding to the row names of `expr_mat`.
    #'    The order of variables doesn't matter.
    #' @param rownames Whether rownames of `sample_info` and `var_info` should be used.
    #'    If TRUE, the row names of `sample_info` and `sample_info` will be
    #'    converted to the first column (with names of "sample" and "variable", respectively).
    initialize = function(name = NA, expr_mat = NA, sample_info = NA, var_info = NA, rownames = FALSE) {
      prepared_sample_info <- prepare_info(sample_info, rownames = rownames, col_name = "sample")
      prepared_var_info <- prepare_info(var_info, rownames = rownames, col_name = "variable")
      sanity_check(expr_mat, prepared_sample_info, prepared_var_info)
      prepared_expr_mat <- expr_mat[prepared_var_info$variable, prepared_sample_info$sample]
      show_data_info(prepared_sample_info, prepared_var_info)

      self$name <- name
      private$expr_mat <- prepared_expr_mat
      private$sample_info <- prepared_sample_info
      private$var_info <- prepared_var_info
    },

    #' @description
    #' Get a copy of the expression matrix.
    #' The rows are variables and the columns are samples.
    #' A function [get_expr_mat()] is also provided for this purpose.
    #' @return A copy of the expression matrix.
    get_expr_mat = function() {
      as.matrix(private$expr_mat)
    },

    #' @description
    #' Get a copy of the sample information tibble.
    #' This is a tibble with the first conserved column "sample",
    #' and the rest columns as meta data of samples,
    #' e.g. "group", "batch", etc.
    #' The "sample" column equals to the column names of `expr_mat`.
    #' A function [get_sample_info()] is also provided for this purpose.
    #' @return A copy of the sample information tibble.
    get_sample_info = function() {
      tibble::as_tibble(private$sample_info)
    },

    #' @description
    #' Get a copy of the variable information tibble.
    #' This is a tibble with the first conserved column "variable",
    #' and the rest columns as meta data of variables,
    #' e.g. "peptide", "protein", "glycan_composition", etc.
    #' The "variable" column equals to the row names of `expr_mat`.
    #' A function [get_var_info()] is also provided for this purpose.
    #' @return A copy of the variable information tibble.
    get_var_info = function() {
      tibble::as_tibble(private$var_info)
    },

    #' @description
    #' Filter samples based on conditions.
    #' This function filters samples based on conditions specified in the [dplyr::filter()] function.
    #' For example, `filter_samples(group == "A")` will keep samples with "group" equal to "A".
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #' @param ... Conditions for filtering samples, passed to [dplyr::filter()].
    #' @return The [Experiment] object.
    filter_samples = function(...) {
      selected_samples <- private$sample_info %>%
        dplyr::filter(...) %>%
        dplyr::pull(sample)
      if (length(selected_samples) == 0) {
        cli::cli_alert_warning("No sample meets the condition(s). An empty Experiment object is returned.")
      } else {
        cli::cli_alert_info("{.val {length(selected_samples)}} samples are selected.")
      }
      private$expr_mat <- private$expr_mat[, selected_samples]
      private$sample_info <- private$sample_info %>%
        dplyr::filter(sample %in% selected_samples)
      invisible(self)
    }
  ),

  private = list(
    expr_mat = NULL,
    sample_info = NULL,
    var_info = NULL
  )
)


#' Function that Creates a New [Experiment] Object
#'
#' @param name Name of the experiment.
#' @param expr_mat Expression matrix. Rows are variables and columns are samples.
#' @param sample_info Sample information, a tibble or a data.frame.
#'    The first column should be "sample", corresponding to the column names of `expr_mat`.
#'    The order of samples doesn't matter.
#' @param var_info Variable information, a tibble or a data.frame.
#'    The first column should be "variable", corresponding to the row names of `expr_mat`.
#'    The order of variables doesn't matter.
#' @param rownames Whether rownames of `sample_info` and `var_info` should be used.
#'    If TRUE, the row names of `sample_info` and `sample_info` will be
#'    converted to the first column (with names of "sample" and "variable", respectively).
#'
#' @return An [Experiment] object.
#' @export
create_experiment <- function(name, expr_mat, sample_info, var_info, rownames = FALSE) {
  Experiment$new(
    name = name,
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    rownames = rownames
  )
}


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


prepare_info <- function(data, rownames, col_name) {
  if (rownames) {
    return(tibble::as_tibble(tibble::rownames_to_column(data, var = col_name)))
  } else {
    return(tibble::as_tibble(data))
  }
}


sanity_check <- function(expr_mat, sample_info, var_info) {
  pass_check <- TRUE
  if (!setequal(colnames(expr_mat), sample_info$sample)) {
    cli::cli_alert_danger("Samples are not consistent in {.field expr_mat} and {.field sample_info}.")
    pass_check <- FALSE
  }
  if (!setequal(rownames(expr_mat), var_info$variable)) {
    cli::cli_alert_danger("Variables are not consistent in {.field expr_mat} and {.field var_info}.")
    pass_check <- FALSE
  }
  if (!pass_check) cli::cli_abort("Please check the data consistency.")
}


show_data_info <- function(sample_info, var_info) {
  cli::cli_alert_info("No of Samples: {.val {nrow(sample_info)}}")
  cli::cli_alert_info("No of Variables: {.val {nrow(var_info)}}")
  cli::cli_alert_info("Meta-data fields for samples: {.field {setdiff(colnames(sample_info), 'sample')}}")
  cli::cli_alert_info("Meta-data fields for variables: {.field {setdiff(colnames(var_info), 'variable')}}")
}
