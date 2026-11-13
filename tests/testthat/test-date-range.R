test_that("B4 custom public ranges order all years independently of observed dates", {
  range <- c("2000-01-01", "2010-12-31")
  days <- seq(as.Date(range[1]), as.Date(range[2]), by="day")
  observations <- list(empty=as.Date(character()), missing=as.Date(NA_character_),
    endpoint=as.Date("2000-01-01"), middle=as.Date("2005-06-15"),
    disjoint=as.Date(c("2030-01-01", "1980-12-31")))
  for (pattern in c("%Y", "%m/%Y", "%G-W%V")) {
    spec <- .dateEffective(list(precision="custom", pattern=pattern, level_range=range))
    public <- unique(format(days, pattern))
    for (observed in observations) {
      value <- .datePresent(list(lo=observed, hi=observed), spec)
      expected <- unique(format(sort(unique(c(days, observed[!is.na(observed)]))), pattern))
      expect_true(is.ordered(value))
      expect_identical(as.character(value), format(observed, pattern))
      expect_identical(levels(value), expected)
      contract <- attr(value, "omop_date_contract")
      expect_identical(contract$declared, public)
      expect_true(all(diff(contract$order)>0))
      # Adding observations must not change the public levels or their order.
      expect_identical(levels(value)[levels(value) %in% public], public)
    }
  }
  expect_identical(unique(format(days, "%Y")), as.character(2000:2010))
  expect_identical(unique(format(days, "%m/%Y")),
    sprintf("%02d/%04d", rep(1:12, 11), rep(2000:2010, each=12)))
  expect_identical(head(unique(format(days, "%G-W%V")), 1), "1999-W52")
  expect_true("2004-W53" %in% unique(format(days, "%G-W%V")))
  expect_identical(tail(unique(format(days, "%G-W%V")), 1), "2010-W52")
})
