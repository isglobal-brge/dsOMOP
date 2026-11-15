# A-31: effective scientific settings, shared by server and client.
# Public canonical/alias names only. No source reads or protection-key work.
.auditSettingContract <- function(name, alias_target = NULL) {
  id <- alias_target %||% name
  if (!is.character(id) || length(id) != 1L || is.na(id)) return(NULL)
  id <- sub("^dsomop:", "", trimws(id))
  if (id %in% c("incidence.rate", "ohdsi.cohort_diagnostics.incidence_rate",
                "ohdsi.cohort_incidence.incidence_summary")) {
    return(list(parameter = "clean_window", reason = "unsupported_clean_window",
      remedy = paste0("zero selects the descriptive first-episode/full-window estimand; ",
        "use a validated native risk recipe when a clean period is required")))
  }
  if (identical(id, "fe.prevalence")) {
    return(list(parameter = "time_windows", reason = "unsupported_fe_window",
      remedy = paste0("zero selects all-history events among scoped cohort members; ",
        "use a declared bounded native feature recipe for a time-limited estimand")))
  }
  NULL
}

.auditEffectiveSettings <- function(name, params, alias_target = NULL) {
  rule <- .auditSettingContract(name, alias_target)
  if (is.null(rule)) return(params)
  key <- rule$parameter
  fail <- function(reason, remedy) stop(reason, ": params.", key, "; ", remedy, call. = FALSE)
  if (!is.list(params) || is.object(params) ||
      (length(params) && (is.null(names(params)) || anyNA(names(params)) ||
                          any(!nzchar(names(params))) || anyDuplicated(names(params))))) {
    fail("invalid_setting", "provide a uniquely named parameter list")
  }
  value <- if (key %in% names(params)) params[[key]] else 0L
  # Catalogue integer transport admits decimal integer text, unlike JSON IDs.
  valid <- !is.object(value) && is.null(dim(value)) && length(value) == 1L &&
    (typeof(value) %in% c("integer", "double", "character")) && !is.na(value)
  number <- if (valid) suppressWarnings(as.numeric(value)) else NA_real_
  if (!valid || !is.finite(number) || number != floor(number) ||
      number < -.Machine$integer.max || number > .Machine$integer.max) {
    fail("invalid_setting", "provide one finite integer; null, fractions and vectors are invalid")
  }
  if (number != 0) fail(rule$reason, rule$remedy)
  params[[key]] <- "0"
  params
}
