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
      if (rownames) {
        self$sample_info <- sample_info |>
          tibble::rownames_to_column("sample") |>
          tibble::as_tibble()
        self$var_info <- var_info |>
          tibble::rownames_to_column("variable") |>
          tibble::as_tibble()
      } else {
        self$sample_info <- tibble::as_tibble(sample_info)
        self$var_info <- tibble::as_tibble(var_info)
      }
      show_data_info(self$sample_info, self$var_info)
    }
  )
)


show_data_info <- function(sample_info, var_info) {
  cli::cli_alert_info("No of Samples: {.val {nrow(sample_info)}}")
  cli::cli_alert_info("No of Variables: {.val {nrow(var_info)}}")
  cli::cli_alert_info("Meta-data fields for samples: {.field {setdiff(colnames(sample_info), 'sample')}}")
  cli::cli_alert_info("Meta-data fields for variables: {.field {setdiff(colnames(var_info), 'variable')}}")
}
