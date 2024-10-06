test_that("filter_samples function works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(new_exp <- filter_samples(exp, sample %in% c("S1", "S2")))

  # new object created
  expect_false(rlang::is_reference(new_exp, exp))
  # sample_info updated
  expect_equal(new_exp$get_sample_info()$sample, c("S1", "S2"))
  # expr_mat updated accordingly
  expect_equal(colnames(new_exp$get_expr_mat()), c("S1", "S2"))
})


test_that("filter_samples doesn't affect old exp", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  suppressMessages(
    new_exp <- filter_samples(exp, sample %in% c("S1", "S2"))
  )

  # original object not changed
  expect_equal(exp$get_sample_info()$sample, c("S1", "S2", "S3"))
})


test_that("filter_variables function works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(new_exp <- filter_variables(exp, variable %in% c("V1", "V2")))

  # new object created
  expect_false(rlang::is_reference(new_exp, exp))
  # var_info updated
  expect_equal(new_exp$get_var_info()$variable, c("V1", "V2"))
  # expr_mat updated accordingly
  expect_equal(rownames(new_exp$get_expr_mat()), c("V1", "V2"))
})


test_that("filter_variables doesn't affect old exp", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  suppressMessages(
    new_exp <- filter_variables(exp, variable %in% c("V1", "V2"))
  )

  # original object not changed
  expect_equal(exp$get_var_info()$variable, c("V1", "V2", "V3"))
})
