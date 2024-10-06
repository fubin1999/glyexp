# creating a new object

    Code
      exp <- Experiment$new(name = "my_experiment", expr_mat = expr_mat, sample_info = sample_info,
        var_info = var_info)
    Message
      i No of Samples: 3
      i No of Variables: 3
      i Meta-data fields for samples: group
      i Meta-data fields for variables: type

# missing samples in sample_info raises an alert

    Code
      Experiment$new(name = "my_experiment", expr_mat = expr_mat, sample_info = sample_info,
        var_info = variable_info)
    Message
      x Samples are not consistent in expr_mat and sample_info.
    Condition
      Error in `sanity_check()`:
      ! Please check the data consistency.

# extra samples in sample_info raises an alert

    Code
      Experiment$new(name = "my_experiment", expr_mat = expr_mat, sample_info = sample_info,
        var_info = variable_info)
    Message
      x Samples are not consistent in expr_mat and sample_info.
    Condition
      Error in `sanity_check()`:
      ! Please check the data consistency.

# missing variables in var_info raises an alert

    Code
      Experiment$new(name = "my_experiment", expr_mat = expr_mat, sample_info = sample_info,
        var_info = variable_info)
    Message
      x Variables are not consistent in expr_mat and var_info.
    Condition
      Error in `sanity_check()`:
      ! Please check the data consistency.

# extra variables in var_info raises an alert

    Code
      Experiment$new(name = "my_experiment", expr_mat = expr_mat, sample_info = sample_info,
        var_info = variable_info)
    Message
      x Variables are not consistent in expr_mat and var_info.
    Condition
      Error in `sanity_check()`:
      ! Please check the data consistency.

# filter samples

    Code
      return_value <- exp$filter_samples(sample %in% c("S1", "S2"))
    Message
      i 2 samples are selected.

# filter samples when no samples selected

    Code
      return_value <- exp$filter_samples(sample %in% c("S4", "S5"))
    Message
      ! No sample meets the condition(s). An empty Experiment object is returned.

