.test_issue_safe_bins <- function(handle, breaks,
                                  table = "measurement",
                                  column = "value_as_number",
                                  concept_id = NULL,
                                  concept_col = NULL,
                                  n_bins = max(2L, length(breaks) - 1L)) {
  scope <- list(
    table = table,
    column = column,
    concept_id = concept_id,
    concept_col = concept_col,
    n_bins = as.integer(n_bins)
  )
  .rememberSafeNumericBins(handle, scope, breaks)
  scope
}

.test_value_bin_leaf <- function(scope, lower, upper,
                                 column = "value_as_number") {
  list(
    var = column,
    op = "value_bin",
    value = list(lower = lower, upper = upper),
    safe_scope = scope
  )
}
