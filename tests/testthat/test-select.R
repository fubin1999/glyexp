test_that("select_samples function works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  sample_info$batch <- c(1, 2, 1)
  suppressMessages(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
  )

  expect_snapshot(new_exp <- select_samples(exp, group))

  # new object created
  expect_false(rlang::is_reference(new_exp, exp))
  # sample_info updated
  expect_equal(colnames(new_exp$get_sample_info()), c("sample", "group"))
  # exp not modified
  expect_equal(colnames(exp$get_sample_info()), c("sample", "group", "batch"))
})


test_that("select_variables function works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  var_info$nF <- c(1, 2, 1)
  suppressMessages(
    exp <- Experiment$new(
      name = "my_experiment",
      expr_mat = expr_mat,
      sample_info = sample_info,
      var_info = var_info
    )
  )

  expect_snapshot(new_exp <- select_variables(exp, type))

  # new object created
  expect_false(rlang::is_reference(new_exp, exp))
  # var_info updated
  expect_equal(colnames(new_exp$get_var_info()), c("variable", "type"))
  # exp not modified
  expect_equal(colnames(exp$get_var_info()), c("variable", "type", "nF"))
})
