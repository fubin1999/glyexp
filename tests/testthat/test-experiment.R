# Helper functions----
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
    group = c("A", "A", "B")
  )
}

create_var_info <- function(variables) {
  tibble::tibble(
    variable = variables,
    type = c("C", "C", "H")
  )
}

# Real tests-----
test_that("creating a new object", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- create_expr_mat(variables, samples)
  sample_info <- create_sample_info(samples)
  var_info <- create_var_info(variables)

  expect_snapshot(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
  )

  expect_equal(exp$name, "my_experiment")
  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
})


test_that("data.frames converted to tibbles", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- create_expr_mat(samples, variables)
  sample_info <- data.frame(
    sample = samples,
    group = c("A", "A", "B")
  )
  var_info <- data.frame(
    variable = variables,
    type = c("C", "C", "H")
  )

  exp <- Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info
  )

  expected_sample_info <- create_sample_info(samples)
  expected_var_info <- create_var_info(variables)
  expect_equal(exp$sample_info, expected_sample_info)
  expect_equal(exp$var_info, expected_var_info)
})


test_that("rownames of data.frames are kept", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- create_expr_mat(samples, variables)
  sample_info <- data.frame(
    group = c("A", "A", "B"),
    row.names = samples
  )
  var_info <- data.frame(
    type = c("C", "C", "H"),
    row.names = variables
  )

  exp <- Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    rownames = TRUE
  )

  expected_sample_info <- create_sample_info(samples)
  expected_var_info <- create_var_info(variables)
  expect_equal(exp$sample_info, expected_sample_info)
  expect_equal(exp$var_info, expected_var_info)
})

