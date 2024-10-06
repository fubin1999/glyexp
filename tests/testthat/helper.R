create_expr_mat <- function(samples, variables) {
  n_row <- length(variables)
  n_col <- length(samples)
  mat <- matrix(runif(n_row * n_col), nrow = n_row)
  rownames(mat) <- variables
  colnames(mat) <- samples
  mat
}

create_sample_info <- function(samples) {
  tibble::tibble(
    sample = samples,
    group = rep("A", length(samples))
  )
}

create_var_info <- function(variables) {
  tibble::tibble(
    variable = variables,
    type = rep("B", length(variables))
  )
}

create_test_exp <- function(samples, variables) {
  expr_mat <- create_expr_mat(samples, variables)
  sample_info <- create_sample_info(samples)
  var_info <- create_var_info(variables)
  suppressMessages(
    exp <- create_experiment(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
  )
  exp
}
