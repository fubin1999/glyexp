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
    group = rep("A", length(samples))
  )
}

create_var_info <- function(variables) {
  tibble::tibble(
    variable = variables,
    type = rep("B", length(variables))
  )
}

# Real tests-----
test_that("creating a new object", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- create_expr_mat(samples, variables)
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
    group = rep("A", 3)
  )
  var_info <- data.frame(
    variable = variables,
    type = rep("B", 3)
  )

  suppressMessages(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
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
    group = rep("A", 3),
    row.names = samples
  )
  var_info <- data.frame(
    type = rep("B", 3),
    row.names = variables
  )

  suppressMessages(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info,
      rownames = TRUE
    )
  )

  expected_sample_info <- create_sample_info(samples)
  expected_var_info <- create_var_info(variables)
  expect_equal(exp$sample_info, expected_sample_info)
  expect_equal(exp$var_info, expected_var_info)
})


test_that("missing samples in sample_info raises an alert", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2"))
  variable_info <- create_var_info(c("V1", "V2", "V3"))

  expect_snapshot(Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = variable_info
  ), error = TRUE)
})


test_that("extra samples in sample_info raises an alert", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3", "S4"))
  variable_info <- create_var_info(c("V1", "V2", "V3"))

  expect_snapshot(Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = variable_info
  ), error = TRUE)
})


test_that("missing variables in var_info raises an alert", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  variable_info <- create_var_info(c("V1", "V2"))

  expect_snapshot(Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = variable_info
  ), error = TRUE)
})


test_that("extra variables in var_info raises an alert", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  variable_info <- create_var_info(c("V1", "V2", "V3", "V4"))

  expect_snapshot(Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = variable_info
  ), error = TRUE)
})
