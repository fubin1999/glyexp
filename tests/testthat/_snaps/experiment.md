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

# filtering samples using non-existing columns raises an error

    Code
      exp$filter_samples(non_existing_column == 1)
    Condition
      Error in `value[[3L]]()`:
      ! Column 'non_existing_column' does not exist in the sample information.
      i Available column(s): sample and group

# filter variables

    Code
      return_value <- exp$filter_variables(variable %in% c("V1", "V2"))
    Message
      i 2 variables are selected.

# filter variables when no variables selected

    Code
      return_value <- exp$filter_variables(variable %in% c("V4", "V5"))
    Message
      ! No variable meets the condition(s). An empty Experiment object is returned.

# filtering variables using non-existing columns raises an error

    Code
      exp$filter_variables(non_existing_column == 1)
    Condition
      Error in `value[[3L]]()`:
      ! Column 'non_existing_column' does not exist in the variable information.
      i Available column(s): variable and type

# filter_samples function works

    Code
      new_exp <- filter_samples(exp, sample %in% c("S1", "S2"))
    Message
      i 2 samples are selected.

# filter_variables function works

    Code
      new_exp <- filter_variables(exp, variable %in% c("V1", "V2"))
    Message
      i 2 variables are selected.

