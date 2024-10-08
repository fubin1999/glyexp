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

# print experiemnt

    Code
      exp
    Message
      i ===== Experiment Object =====
      i Name: "my_experiment"
      i Expression Matrix: 3 samples, 3 variables
      i Sample Information Fields: group
      i Variable Information Fields: type

# filter samples

    Code
      return_value <- exp$filter_samples(sample %in% c("S1", "S2"))
    Message
      i 2 samples are selected.

# filtering samples when no samples selected raises an error

    Code
      exp$filter_samples(sample %in% c("S4", "S5"))
    Condition
      Error in `private$filter()`:
      ! No sample meets the condition(s): `sample %in% c("S4", "S5")`.

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

# filtering variables when no variables selected raises an error

    Code
      exp$filter_variables(variable %in% c("V4", "V5"))
    Condition
      Error in `private$filter()`:
      ! No variable meets the condition(s): `variable %in% c("V4", "V5")`.

# filtering variables when no variables selected with many conditions

    Code
      exp$filter_variables(variable %in% c("V4", "V5"), type == "D")
    Condition
      Error in `private$filter()`:
      ! No variable meets the condition(s): `variable %in% c("V4", "V5")` and `type == "D"`.

# filtering variables using non-existing columns raises an error

    Code
      exp$filter_variables(non_existing_column == 1)
    Condition
      Error in `value[[3L]]()`:
      ! Column 'non_existing_column' does not exist in the variable information.
      i Available column(s): variable and type

# mutate samples

    Code
      return_value <- exp$mutate_samples(new_sample = paste0("new_", sample))

# mutating samples using non-existing columns raises an error

    Code
      exp$mutate_samples(new_col = non_existing_column)
    Condition
      Error in `value[[3L]]()`:
      ! Column 'non_existing_column' does not exist in the sample information.
      i Available column(s): sample and group

# forbitting modifying 'sample' column

    Code
      exp$mutate_samples(sample = paste0("new_", sample))
    Condition
      Error in `private$mutate()`:
      ! Column sample is protected and cannot be modified.

# forbitting modifying 'sample' column using `across`

    Code
      exp$mutate_samples(across(sample, ~ paste0("new_", .)))
    Condition
      Error in `private$mutate()`:
      ! Column sample is protected and cannot be modified.

# forbitting modifying 'sample' column using `across(where)`

    Code
      exp$mutate_samples(across(where(is.character), ~ paste0("new_", .)))
    Condition
      Error in `private$mutate()`:
      ! Column sample is protected and cannot be modified.

# mutate variables

    Code
      return_value <- exp$mutate_variables(new_variable = paste0("new_", variable))

# mutating variables using non-existing columns raises an error

    Code
      exp$mutate_variables(new_col = non_existing_column)
    Condition
      Error in `value[[3L]]()`:
      ! Column 'non_existing_column' does not exist in the variable information.
      i Available column(s): variable and type

# forbitting modifying 'variable' column

    Code
      exp$mutate_variables(variable = paste0("new_", variable))
    Condition
      Error in `private$mutate()`:
      ! Column variable is protected and cannot be modified.

# select sample_info columns

    Code
      return_value <- exp$select_samples(group)

# select var_info columns

    Code
      return_value <- exp$select_variables(protein)

# selecting non-existing columns raises an error

    Code
      exp$select_samples(non_existing_column)
    Condition
      Error in `value[[3L]]()`:
      ! Column non_existing_column does not exist in the sample information.
      i Available column(s): sample and group. (The sample column is always selected automatically.)

---

    Code
      exp$select_variables(non_existing_column)
    Condition
      Error in `value[[3L]]()`:
      ! Column non_existing_column does not exist in the variable information.
      i Available column(s): variable and type. (The variable column is always selected automatically.)

# selecting 'sample' gives an error

    Code
      exp$select_samples(sample, group)
    Condition
      Error in `value[[3L]]()`:
      ! You cannot select or deselect the sample column explicitly.
      i The sample column is always selected automatically.

# selecting 'variable' gives an error

    Code
      exp$select_variables(variable, type)
    Condition
      Error in `value[[3L]]()`:
      ! You cannot select or deselect the variable column explicitly.
      i The variable column is always selected automatically.

# deselecting 'sample' gives an error

    Code
      exp$select_samples(-sample)
    Condition
      Error in `value[[3L]]()`:
      ! You cannot select or deselect the sample column explicitly.
      i The sample column is always selected automatically.

# deselecting 'variable' gives an error

    Code
      exp$select_variables(-variable)
    Condition
      Error in `value[[3L]]()`:
      ! You cannot select or deselect the variable column explicitly.
      i The variable column is always selected automatically.

