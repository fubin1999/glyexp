# Experiment Class Definition-------------------------------------------------
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
    #' This method filters samples based on conditions specified in the [dplyr::filter()] function.
    #' For example, `filter_samples(group == "A")` will keep samples with "group" equal to "A".
    #' @details
    #' This will affect both the expression matrix and the sample information tibble.
    #' If no samples meet the condition(s), an error will be thrown.
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #' @param ... Conditions for filtering samples, passed to [dplyr::filter()].
    #' @return The [Experiment] object.
    filter_samples = function(...) {
      private$filter(..., info = "sample")
    },

    #' @description
    #' This method filters variables based on conditions specified in the [dplyr::filter()] function.
    #' For example, `filter_variables(type == "B")` will keep variables with "type" equal to "B".
    #' @details
    #' This will affect both the expression matrix and the variable information tibble.
    #' If no variables meet the condition(s), an error will be thrown.
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #' @param ... Conditions for filtering variables, passed to [dplyr::filter()].
    #' @return The [Experiment] object.
    filter_variables = function(...) {
      private$filter(..., info = "variable")
    },

    #' @description
    #' This function mutates the sample information tibble using [dplyr::mutate()].
    #' For example, `mutate_samples(new_group = if_else(new_col = 1)`
    #' will add a new column "new_group" to the sample information tibble.
    #' @details
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #'
    #' The columns "sample" is protected from being modified or renamed.
    #' It is essential for keeping the linkage between the sample information and the expression matrix.
    #' You may add new columns by transforming the "sample" column.
    #' @param ... Mutations for the sample information tibble, passed to [dplyr::mutate()].
    #' @return The [Experiment] object.
    mutate_samples = function(...) {
      private$mutate(..., info = "sample")
    },

    #' @description
    #' This function mutates the variable information tibble using [dplyr::mutate()].
    #' For example, `mutate_variables(new_type = if_else(new_col = 1)`
    #' will add a new column "new_type" to the variable information tibble.
    #' @details
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #'
    #' The columns "variable" is protected from being modified or renamed.
    #' It is essential for keeping the linkage between the variable information and the expression matrix.
    #' You may add new columns by transforming the "variable" column.
    #' @param ... Mutations for the variable information tibble, passed to [dplyr::mutate()].
    #' @return The [Experiment] object.
    mutate_variables = function(...) {
      private$mutate(..., info = "variable")
    },

    #' @description
    #' This function selects columns in the sample information tibble.
    #' You don't need to select the "sample" column explicitly, as it is always selected.
    #' For example, `exp$select_samples(group)` will result in a tibble
    #' with the "sample" and "group" columns.
    #' @details
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #'
    #' When the user tries to select or deselect the "sample" column explicitly,
    #' an error will be thrown.
    #' @param ... <[`tidy-select`][dplyr_tidy_select]> for selecting columns, passed to [dplyr::select()].
    #' @return The [Experiment] object.
    select_samples = function(...) {
      private$select(..., info = "sample")
    },

    #' @description
    #' This function selects columns in the variable information tibble.
    #' You don't need to select the "variable" column explicitly, as it is always selected.
    #' For example, `exp$select_variables(peptide, protein)` will result in a tibble
    #' with the "variable", "peptide", and "protein" columns.
    #' @details
    #' The [Experiment] object will be updated in place.
    #' The [Experiment] object will be returned invisibly to allow chaining.
    #'
    #' When the user tries to select or deselect the "variable" column explicitly,
    #' an error will be thrown.
    #' @param ... <[`tidy-select`][dplyr_tidy_select]> for selecting columns, passed to [dplyr::select()].
    #' @return The [Experiment] object.
    select_variables = function(...) {
      private$select(..., info = "variable")
    },

    #' @description
    #' Print the basic information of the Experiment object.
    #' @param ... Ignored.
    print = function(...) {
      cli::cli_alert_info("===== Experiment Object =====")
      cli::cli_alert_info("Name: {.val {self$name}}")
      cli::cli_alert_info("Expression Matrix: {.val {ncol(private$expr_mat)}} samples, {.val {nrow(private$expr_mat)}} variables")
      cli::cli_alert_info("Sample Information Fields: {.field {setdiff(colnames(private$sample_info), 'sample')}}")
      cli::cli_alert_info("Variable Information Fields: {.field {setdiff(colnames(private$var_info), 'variable')}}")
    }
  ),

  private = list(
    expr_mat = NULL,
    sample_info = NULL,
    var_info = NULL,

    filter = function(..., info = NA) {
      info_df <- private$get_info(info)
      # Get samples/variables that meet the condition(s).
      try_dplyr(
        selected <- info_df |> dplyr::filter(...) |> dplyr::pull(dplyr::all_of(info)),
        info_df = info_df, info = info
      )
      # Show information about the filtering results.
      if (length(selected) == 0) {
        # If no samples/variables meet the condition(s), abort the operation.
        # The rationale of this behavior instead of returning an empty object is that,
        # when no samples/variables meet the condition(s),
        # storing the empty object in a variable or passing it to downstream functions
        # is meaningless and may lead to unexpected results.
        # Besides, most of the time when this happens,
        # it is likely that the user made a mistake in the condition(s).
        condition_text <- purrr::map_chr(rlang::enexprs(...), rlang::expr_label)
        cli::cli_abort("No {info} meets the condition(s): {.expr {condition_text}}.")
      } else {
        cli::cli_alert_info("{.val {length(selected)}} {info}s are selected.")
      }
      # Update the Experiment object.
      if (info == "sample") {
        private$expr_mat <- private$expr_mat[, selected]
        private$sample_info <- private$sample_info |>
          dplyr::filter(sample %in% selected)
      } else {
        private$expr_mat <- private$expr_mat[selected, ]
        private$var_info <- private$var_info |>
          dplyr::filter(variable %in% selected)
      }
      # Return the Experiment object invisibly.
      invisible(self)
    },

    mutate = function(..., info = NA) {
      # This function mutates the tibble while protecting some columns.
      # The protection is implemented by checking if the protected columns
      # were modified after the mutation.
      # This is not a perfect solution, certainly with performance issues.
      # But as `dplyr::mutate()` uses non-standard evaluation,
      # along with the `across` syntax,
      # it is hard to know if a column will be modified beforehand.

      info_df <- private$get_info(info)
      # Record the protected column before mutation.
      protected_col <- dplyr::if_else(info == "sample", "sample", "variable")
      protected_before <- info_df[[protected_col]]
      # Mutate the information data frame.
      try_dplyr(
        new_info_df <- dplyr::mutate(info_df, ...),
        info_df = info_df, info = info
      )
      # Check if any protected columns were modified.
      if (!identical(protected_before, new_info_df[[protected_col]])) {
        cli::cli_abort("Column {.field {protected_col}} is protected and cannot be modified.")
      }
      # Update the Experiment object.
      if (info == "sample") {
        private$sample_info <- new_info_df
      } else {
        private$var_info <- new_info_df
      }
      # Return the Experiment object invisibly.
      invisible(self)
    },

    select = function(..., info = NA) {
      # The "sample" column in `sample_info` and the "variable" column in `var_info`
      # will always be selected, without explicit declaration.

      info_df <- private$get_info(info)
      # Select the columns.
      new_info_df <- tryCatch(
        info_df |> dplyr::select(-all_of(info)) |> dplyr::select(...),
        error = function(e) {
          if (stringr::str_detect(conditionMessage(e), "Column `.*` doesn't exist")) {
            missing_column <- stringr::str_extract(conditionMessage(e), "`(.*)`", group = 1)
            if (missing_column == info) {
              cli::cli_abort(c(
                "You cannot select or deselect the {.field {info}} column explicitly.",
                "i" = "The {.field {info}} column is always selected automatically."
                ))
            } else {
              cli::cli_abort(c(
                "Column {.field {missing_column}} does not exist in the {info} information.",
                "i" = "Available column(s): {.field {colnames(info_df)}}. (The {.field {info}} column is always selected automatically.)"
              ))
            }
          } else {
            rlang::abort(e)
          }
        }
      )
      new_info_df <- dplyr::mutate(new_info_df, !!info := info_df[[info]], .before = 1)
      # Update the Experiment object.
      if (info == "sample") {
        private$sample_info <- new_info_df
      } else {
        private$var_info <- new_info_df
      }
      # Return the Experiment object invisibly.
      invisible(self)
    },

    get_info = function(info) {
      if (info == "sample") {
        return(private$sample_info)
      } else if (info == "variable") {
        return(private$var_info)
      } else {
        cli::cli_abort("Unknown {info} type.")
      }
    }
  )
)


# Helper Functions for Experiment Class -----------------------------------------
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


# This is a hard-to-understand function that tries to evaluate an expression
# in the parent frame, and provides a more informative error message when
# the error is about missing column in the information tibble.
# It could be regarded as a customized version of `tryCatch()`,
# with the `error` argument tailored for handling missing column error
# of a `dplyr` function.
# Used in `Experiment$filter()` and `Experiment$mutate()` private methods.
try_dplyr <- function(expr, info_df, info) {
  tryCatch(
    # `expr` is evaluated in the parent frame, thanks to the lazy evaluation.
    # This allows the client code run any dplyr functions in the parent frame.
    eval(expr, envir = parent.frame()),
    # Capture the error and provide a more informative message, including:
    # 1. The column that does not exist in the information data frame.
    # 2. The available columns in the information data frame.
    # If hte error is not about missing column, re-throw the error.
    error = function(e) {
      if (stringr::str_detect(conditionMessage(e), "object '.*' not found")) {
        missing_column <- stringr::str_extract_all(conditionMessage(e), "'(.*)'")[[1]]
        cli::cli_abort(c(
          "Column {.field {missing_column}} does not exist in the {info} information.",
          "i" = "Available column(s): {.field {colnames(info_df)}}"
        ))
      } else {
        rlang::abort(e)
      }
    }
  )
}


#' @import R6 dplyr purrr
NULL
