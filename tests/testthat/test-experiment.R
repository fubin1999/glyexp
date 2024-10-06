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
  expect_equal(exp$get_expr_mat(), expr_mat)
  expect_equal(exp$get_sample_info(), sample_info)
  expect_equal(exp$get_var_info(), var_info)
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
  expect_equal(exp$get_sample_info(), expected_sample_info)
  expect_equal(exp$get_var_info(), expected_var_info)
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
  expect_equal(exp$get_sample_info(), expected_sample_info)
  expect_equal(exp$get_var_info(), expected_var_info)
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


test_that("expr_mat reordered according to sample_info and var_info", {
  expr_mat <- matrix(1:9, nrow = 3, byrow = TRUE)
  colnames(expr_mat) <- c("S1", "S2", "S3")
  rownames(expr_mat) <- c("V1", "V2", "V3")
  sample_info <- create_sample_info(c("S3", "S1", "S2"))
  variable_info <- create_var_info(c("V2", "V3", "V1"))

  suppressMessages(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = variable_info
    )
  )

  expected_mat <- matrix(c(6, 4, 5, 9, 7, 8, 3, 1, 2), nrow = 3, byrow = TRUE)
  colnames(expected_mat) <- c("S3", "S1", "S2")
  rownames(expected_mat) <- c("V2", "V3", "V1")
  expect_equal(exp$get_expr_mat(), expected_mat)
})


test_that("getting data copies instead of original data", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  expr_mat[1, 1] <- 1
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  suppressMessages(
    exp <- create_experiment(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
  )

  exp_sample_info <- exp$get_sample_info()
  exp_var_info <- exp$get_var_info()
  exp_expr_mat <- exp$get_expr_mat()
  exp_sample_info$sample <- c("S4", "S5", "S6")
  exp_var_info$variable <- c("V4", "V5", "V6")
  exp_expr_mat[1, 1] <- 100
  expect_equal(exp$get_sample_info()$sample, c("S1", "S2", "S3"))
  expect_equal(exp$get_var_info()$variable, c("V1", "V2", "V3"))
  expect_equal(exp$get_expr_mat()[1, 1], 1)
})


test_that("filter samples", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(return_value <- exp$filter_samples(sample %in% c("S1", "S2")))

  # should return self
  expect_true(rlang::is_reference(return_value, exp))
  # sample_info updated
  expect_equal(exp$get_sample_info()$sample, c("S1", "S2"))
  # expr_mat updated accordingly
  expect_equal(colnames(exp$get_expr_mat()), c("S1", "S2"))
})


test_that("filter samples when no samples selected", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(return_value <- exp$filter_samples(sample %in% c("S4", "S5")))

  # should return self
  expect_true(rlang::is_reference(return_value, exp))
  # sample_info updated
  expect_equal(exp$get_sample_info()$sample, character(0))
  # expr_mat updated accordingly
  expect_equal(colnames(exp$get_expr_mat()), NULL)
})


test_that("filtering samples using non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(exp$filter_samples(non_existing_column == 1), error = TRUE)
})


test_that("filter variables", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(return_value <- exp$filter_variables(variable %in% c("V1", "V2")))

  # should return self
  expect_true(rlang::is_reference(return_value, exp))
  # var_info updated
  expect_equal(exp$get_var_info()$variable, c("V1", "V2"))
  # expr_mat updated accordingly
  expect_equal(rownames(exp$get_expr_mat()), c("V1", "V2"))
})


test_that("filter variables when no variables selected", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(return_value <- exp$filter_variables(variable %in% c("V4", "V5")))

  # should return self
  expect_true(rlang::is_reference(return_value, exp))
  # var_info updated
  expect_equal(exp$get_var_info()$variable, character(0))
  # expr_mat updated accordingly
  expect_equal(rownames(exp$get_expr_mat()), NULL)
})


test_that("filtering variables using non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(exp$filter_variables(non_existing_column == 1), error = TRUE)
})
