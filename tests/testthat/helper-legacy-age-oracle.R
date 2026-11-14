# A-27/A-28 present legacy age bands as ordered categories with their complete
# public domain. Check that contract before comparing the historical text
# representation with immutable 2.10.2 captures. No other column or attribute
# is normalised, and the caller still checks every value against its oracle.
.expect_legacy_age_text <- function(frame, breaks, info) {
  if (is.null(breaks)) breaks <- getOption("dsomop.age_breaks",
    getOption("default.dsomop.age_breaks", seq(0L, 85L, 5L)))
  breaks <- as.numeric(unlist(breaks, use.names = FALSE))
  labels <- c(paste0(head(breaks, -1L), "-", tail(breaks, -1L) - 1L),
              paste0(tail(breaks, 1L), "+"))
  column <- frame$age_group
  values <- as.character(column)
  calendar <- list(category = "age_band", levels = as.list(labels))
  expect_identical(class(column), c("dsomop_date_factor", "ordered", "factor"), info = info)
  expect_identical(levels(column), labels, info = info)
  expect_identical(sort(names(attributes(column))),
    sort(c("levels", "class", "omop_date_contract")), info = info)
  expect_identical(attr(column, "omop_date_contract"),
    list(keys = values, levels = labels, labels = labels, spec = calendar,
         order = seq_along(labels), declared = labels), info = info)
  records <- setNames(lapply(seq_along(labels), function(i) {
    list(kind = "date", id = labels[[i]], label = labels[[i]],
         calendar = calendar, order = i)
  }), labels)
  contracts <- attr(frame, "omop_concepts")
  expect_identical(contracts$age_group,
    list(kind = "date", calendar = calendar, ids = values, records = records,
         declared = labels,
         resolved = list(ids = labels, labels = labels, levels = labels,
                         vocabulary = labels, kind = "date", calendar = calendar,
                         decisions = list())), info = info)
  frame$age_group <- values
  contracts$age_group <- NULL
  attr(frame, "omop_concepts") <- if (length(contracts)) contracts else NULL
  frame
}
