test_that("getter functions work", {
  samples <- c("S1", "S2", "S3")
  variables <- c("V1", "V2", "V3")
  expr_mat <- create_expr_mat(samples, variables)
  sample_info <- create_sample_info(samples)
  var_info <- create_var_info(variables)
  suppressMessages(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
  )

  expect_equal(get_expr_mat(exp), expr_mat)
  expect_equal(get_sample_info(exp), sample_info)
  expect_equal(get_var_info(exp), var_info)
})
