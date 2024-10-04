Experiment <- R6::R6Class(
  "Experiment",
  public = list(
    name = NULL,
    expr_mat = NULL,
    sample_info = NULL,
    var_info = NULL,
    initialize = function(name = NA, expr_mat = NA, sample_info = NA, var_info = NA, rownames = FALSE) {
      self$name <- name
      self$expr_mat <- expr_mat
      self$sample_info <- prepare_info(sample_info, rownames = rownames, col_name = "sample")
      self$var_info <- prepare_info(var_info, rownames = rownames, col_name = "variable")
      show_data_info(self$sample_info, self$var_info)
    }
  )
)


prepare_info <- function(data, rownames, col_name) {
  if (rownames) {
    return(tibble::as_tibble(tibble::rownames_to_column(data, var = col_name)))
  } else {
    return(tibble::as_tibble(data))
  }
}


show_data_info <- function(sample_info, var_info) {
  cli::cli_alert_info("No of Samples: {.val {nrow(sample_info)}}")
  cli::cli_alert_info("No of Variables: {.val {nrow(var_info)}}")
  cli::cli_alert_info("Meta-data fields for samples: {.field {setdiff(colnames(sample_info), 'sample')}}")
  cli::cli_alert_info("Meta-data fields for variables: {.field {setdiff(colnames(var_info), 'variable')}}")
}
