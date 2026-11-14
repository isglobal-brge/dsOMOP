# Shared public-data plan and independent base-R oracle. No package internals
# are used in the oracle; the client sources this from its matching sibling.
numeric_lab <- 3000963L # Hemoglobin [Mass/volume] in Blood; g/dL (8713)
numeric_event <- function() list(table = "measurement",
  concept_set = list(concepts = list(numeric_lab)), value = list(column = "value_as_number"))
numeric_plan <- function() {
  red <- function(fn, ...) list(source = list(event = "lab"), reducer = c(list(fn = fn), list(...)))
  vars <- list(first = red("first_value"), last = red("last_value"), nth = red("nth_value", n = list(2L)),
    closest = red("closest_value"), min = red("min"), max = red("max"), mean = red("mean"),
    sd = red("sd"), slope = red("slope"), sum = red("sum"), median = red("median"),
    quantile = red("quantile", p = 0.25), cv = red("cv"), mode = red("mode", value = "value"),
    high = red("abnormal_high"), low = red("abnormal_low"),
    at_index = red("closest_value", window = list(start = -90L, end = 30L, anchor = "index")))
  list(version = 2L, meta = list(language_version = "2.0", semantic_version = "2.0.0", generated_by = "numeric-cdm-test"),
    events = list(lab = numeric_event()),
    populations = list(p = list(kind = "index", index = list(source = list(event = "lab"), select = "first",
      end = list(strategy = "event_end")))), variables = vars,
    outputs = list(wide = list(rows = "p", format = "wide", grain = "episode")))
}
numeric_measurements <- function(con, schema = NULL) {
  id <- if (is.null(schema)) "measurement" else DBI::Id(schema = schema, table = "measurement")
  d <- DBI::dbGetQuery(con, paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(con, id),
                                  " WHERE measurement_concept_id = ", numeric_lab))
  d$measurement_date <- as.Date(d$measurement_date)
  d[order(d$person_id, d$measurement_date, d$measurement_id), ]
}
numeric_oracle <- function(d) {
  pieces <- split(d, d$person_id)
  do.call(rbind, lapply(pieces, function(x) {
    v <- x$value_as_number[!is.na(x$value_as_number)]
    dates <- as.numeric(x$measurement_date[!is.na(x$value_as_number)])
    modal <- as.numeric(names(sort(table(v), decreasing = TRUE))[1L])
    data.frame(pid = x$person_id[[1L]], first = v[[1L]], last = tail(v, 1L),
      nth_2 = if (length(v) > 1L) v[[2L]] else NA_real_, closest = v[[1L]],
      min = min(v), max = max(v), mean = mean(v), sd = stats::sd(v),
      slope = if (length(unique(dates)) > 1L) unname(stats::coef(stats::lm(v ~ dates))[[2L]]) else NA_real_,
      sum = sum(v), median = stats::median(v), quantile = unname(stats::quantile(v, .25)),
      cv = stats::sd(v) / mean(v) * 100, mode = modal,
      high = sum(x$value_as_number > x$range_high, na.rm = TRUE),
      low = sum(x$value_as_number < x$range_low, na.rm = TRUE), at_index = v[[1L]])
  }))
}
numeric_formats <- function() {
  p <- numeric_plan()
  p$outputs$long <- list(rows = "p", format = "long", sources = list(list(event = "lab")),
    options = list(columns_derived = list("value", "unit", "days_from_index")))
  p$outputs$panel <- list(rows = "p", format = "person_period", options = list(
    sources = list(list(event = "lab")), window = list(start = 0L, end = 3650L, anchor = "index"),
    bins = list(width = 365L), analyses = list("mean", "last")))
  p$outputs$sparse <- list(rows = "p", format = "sparse", options = list(
    sources = list(list(event = "lab")), analyses = list("binary", "count")))
  p$outputs$temporal <- list(rows = "p", format = "temporal_covariates", options = list(
    sources = list(list(event = "lab")), analyses = list("mean", "last"), bin_width = 3651L,
    window = list(start = 0L, end = 3650L, anchor = "index")))
  p$outputs$survival <- list(rows = "p", format = "survival", options = list(
    outcomes = list(lab = list(event = "lab")), tar = list(start = 1L, end = 3650L, anchor = "index"),
    censor = list("observation_end")))
  # Covariates are private wide inputs joined to the survival root.
  p$outputs$survival_covariates <- list(rows = "p", format = "joined", grain = "episode",
    options = list(from = list(list(output = "survival"), list(output = "wide")), cardinality = "one_to_one"))
  p$publish <- as.list(c("wide", "long", "panel", "sparse", "temporal", "survival_covariates"))
  p
}
