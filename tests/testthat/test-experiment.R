test_that("creating a new object", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- matrix(runif(9), nrow = 3)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- variables
  sample_info <- tibble::tibble(
    sample = samples,
    group = c("A", "A", "B")
  )
  var_info <- tibble::tibble(
    variable = variables,
    type = c("H5N4S1", "H5N4F1", "H5N4F1S1")
  )

  exp <- Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info
  )

  expect_equal(exp$name, "my_experiment")
  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
})


test_that("data.frames converted to tibbles", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- matrix(runif(9), nrow = 3)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- variables
  sample_info <- data.frame(
    sample = samples,
    group = c("A", "A", "B")
  )
  var_info <- data.frame(
    variable = variables,
    type = c("H5N4S1", "H5N4F1", "H5N4F1S1")
  )

  exp <- Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info
  )

  expected_sample_info <- tibble::tibble(
    sample = samples,
    group = c("A", "A", "B")
  )
  expected_var_info <- tibble::tibble(
    variable = variables,
    type = c("H5N4S1", "H5N4F1", "H5N4F1S1")
  )
  expect_equal(exp$sample_info, expected_sample_info)
  expect_equal(exp$var_info, expected_var_info)
})


test_that("rownames of data.frames are kept", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- matrix(runif(9), nrow = 3)
  colnames(expr_mat) <- samples
  rownames(expr_mat) <- variables
  sample_info <- data.frame(
    group = c("A", "A", "B"),
    row.names = samples
  )
  var_info <- data.frame(
    type = c("H5N4S1", "H5N4F1", "H5N4F1S1"),
    row.names = variables
  )

  exp <- Experiment$new(
    name = "my_experiment",
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    rownames = TRUE
  )

  expected_sample_info <- tibble::tibble(
    sample = samples,
    group = c("A", "A", "B")
  )
  expected_var_info <- tibble::tibble(
    variable = variables,
    type = c("H5N4S1", "H5N4F1", "H5N4F1S1")
  )
  expect_equal(exp$sample_info, expected_sample_info)
  expect_equal(exp$var_info, expected_var_info)
})
