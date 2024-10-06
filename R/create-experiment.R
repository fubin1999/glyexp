#' Function that Creates a New [Experiment] Object
#'
#' @param name Name of the experiment.
#' @param expr_mat Expression matrix. Rows are variables and columns are samples.
#' @param sample_info Sample information, a tibble or a data.frame.
#'    The first column should be "sample", corresponding to the column names of `expr_mat`.
#'    The order of samples doesn't matter.
#' @param var_info Variable information, a tibble or a data.frame.
#'    The first column should be "variable", corresponding to the row names of `expr_mat`.
#'    The order of variables doesn't matter.
#' @param rownames Whether rownames of `sample_info` and `var_info` should be used.
#'    If TRUE, the row names of `sample_info` and `sample_info` will be
#'    converted to the first column (with names of "sample" and "variable", respectively).
#'
#' @return An [Experiment] object.
#' @export
create_experiment <- function(name, expr_mat, sample_info, var_info, rownames = FALSE) {
  Experiment$new(
    name = name,
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    rownames = rownames
  )
}
