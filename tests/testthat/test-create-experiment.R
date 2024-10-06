test_that("creates an experiment by funtion", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
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

  expect_equal(exp$name, "my_experiment")
  expect_equal(exp$get_expr_mat(), expr_mat)
  expect_equal(exp$get_sample_info(), sample_info)
  expect_equal(exp$get_var_info(), var_info)
})
