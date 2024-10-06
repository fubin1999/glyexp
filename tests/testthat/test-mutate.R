test_that("mutate_samples function works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_samples(exp, new_col = 1)

  # new object created
  expect_false(rlang::is_reference(new_exp, exp))
  # new column added
  expect_equal(new_exp$get_sample_info()$new_col, c(1, 1, 1))
  # old exp not modified
  expect_equal(colnames(exp$get_sample_info()), c("sample", "group"))
})


test_that("mutate_variables function works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_variables(exp, new_col = 1)

  # new object created
  expect_false(rlang::is_reference(new_exp, exp))
  # new column added
  expect_equal(new_exp$get_var_info()$new_col, c(1, 1, 1))
  # old exp not modified
  expect_equal(colnames(exp$get_var_info()), c("variable", "type"))
})
