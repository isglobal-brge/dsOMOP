.date_fix_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.date_fix_backends), testthat::teardown_env())

test_that("B1 selected reducer and raw dates obey variable and recipe policies", {
  h <- .date_fix_backends$sqlite$handle
  for (scope in c("variable", "recipe")) for (precision in c("day", "month", "relative_days")) {
    spec <- list(precision=precision, future="na", as_of="2020-01-07")
    if (precision=="relative_days") spec$anchor <- list(date="2020-01-01")
    vars <- setNames(lapply(c("first_date", "last_date", "first_value", "last_value"), function(fn) {
      reducer <- list(fn=fn)
      if (grepl("value",fn)) reducer$value <- "measurement_date"
      list(source=list(event="lab"), reducer=reducer)
    }), c("first", "last", "first_value", "last_value"))
    vars$raw <- list(raw=list(table="observation_period", column="observation_period_end_date"))
    vars$helper <- list(derived=list(kind="calendar_date", reference=list(source=list(event="lab"),which="first")), broadcast=TRUE)
    if (scope=="variable") vars <- lapply(vars,function(v) {v$date_spec<-spec;v})
    plan <- .ws7_scenario_plan(vars)
    if (scope=="recipe") plan$options <- list(date_spec=spec)
    f <- .ws7_scenario_run(h,plan)$frame
    first <- as.Date(c("2020-01-01",NA,"2020-01-02","2020-01-07",NA,NA,NA,NA))
    last <- as.Date(c(NA,NA,"2020-01-03",NA,NA,NA,NA,NA))
    expected <- list(first=first,last=last,first_value=first,last_value=last,
                     raw=as.Date(rep(NA_character_,8)),helper=first)
    for (col in names(expected)) {
      got <- f[[col]][match(7001:7008,f$pid)]
      want <- expected[[col]]
      if (precision=="day") {expect_s3_class(got,"Date");expect_identical(got,want)}
      if (precision=="month") {
        expect_true(is.ordered(got))
        expect_identical(as.character(got),ifelse(is.na(want),NA_character_,"2020-01"))
      }
      if (precision=="relative_days") expect_identical(got,as.integer(want-as.Date("2020-01-01")))
    }
    # A later selected event is NA even where an earlier eligible event exists.
    expect_true(is.na(.ws7_value(f,7001,"last")))
    expect_true(is.na(.ws7_value(f,7004,"last_value")))
  }
  # A variable can explicitly allow dates beyond the recipe's cutoff.
  plan$variables$last$date_spec <- list(future="allow")
  plan$options$date_spec$as_of <- NULL
  plan$options$date_spec$future <- "allow"
  plan$variables$first$date_spec <- list(future="na",as_of="2019-12-31")
  f <- .ws7_scenario_run(h,plan)$frame
  expect_identical(.ws7_value(f,7001,"last"),19L)
  expect_true(all(is.na(f$first)))
})

test_that("B1 raw output roles and selected typed values reject invalid domains before presentation", {
  d <- structure(c(as.numeric(as.Date(c("0001-01-01","9999-12-31","2020-01-07","2020-01-08",NA))),
                   as.numeric(as.Date("0001-01-01"))-1,as.numeric(as.Date("9999-12-31"))+1,Inf,NaN,0.5),class="Date")
  columns <- list(d=.fragmentColumn("date",TRUE,"measure"))
  for (precision in c("day","month","relative_days")) {
    spec <- list(precision=precision,future="na",as_of="2020-01-07")
    if (precision=="relative_days") spec$anchor <- list(date="0001-01-01")
    f <- .dateApplyColumns(data.frame(d=d),columns,spec)$frame
    expect_identical(is.na(f$d),c(FALSE,TRUE,FALSE,rep(TRUE,7)))
    source <- .dateTypedValues(d,.dateEffective(spec))
    expect_identical(is.na(source$lo),is.na(f$d))
    expect_identical(source$lo,source$hi)
  }
  f <- .dateApplyColumns(data.frame(d=d),columns,list(precision="day"))$frame
  expect_identical(f$d[1:4],d[1:4])
  expect_true(all(is.na(f$d[5:10])))
})

test_that("B2 yearless patterns follow precisely the 2000 through 2399 calendar", {
  expected <- list(a=c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"),
                   A=c("Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday","Friday"),
                   u=c("6","7","1","2","3","4","5"))
  for (directive in names(expected)) {
    spec <- .dateEffective(list(precision="custom",pattern=paste0("%",directive),level_range=c("2020-01-01","2020-01-07")))
    value <- .datePresent(list(lo=as.Date("2020-01-01"),hi=as.Date("2020-01-01")),spec)
    expect_identical(levels(value),expected[[directive]])
    expect_identical(attr(value,"omop_date_contract")$order,as.numeric(as.Date("2000-01-01")+0:6))
  }
  # Year-bearing patterns still order chronologically across disjoint years.
  spec <- .dateEffective(list(precision="custom",pattern="%a %Y"))
  value <- .datePresent(list(lo=as.Date(c("2022-01-01","2020-01-01")),hi=as.Date(c("2022-01-01","2020-01-01"))),spec)
  expect_identical(levels(value),c("Wed 2020","Sat 2022"))
})

test_that("B3 hand-authored plans tokenize percent escapes and validate rendered label keys", {
  patterns <- c("%%Y %G-W%V","%%V %Y","%%%Y","%%%%%Y","%%Y %a","%Y%%","%%G%%V%Y")
  expected <- c("%Y 2020-W01","%V 2020","%2020","%%2020","%Y Wed","2020%","%G%V2020")
  vars <- setNames(lapply(patterns,function(pattern) list(
    derived=list(kind="calendar_date",reference=list(date="2020-01-01")),broadcast=TRUE,
    date_spec=list(precision="custom",pattern=pattern))),paste0("d",seq_along(patterns)))
  plan <- .ws7_scenario_plan(vars)
  f <- .ws7_scenario_run(.date_fix_backends$sqlite$handle,plan)$frame
  for (i in seq_along(patterns)) {
    expect_identical(as.character(f[[paste0("d",i)]]),rep(expected[i],8))
    spec <- .dateEffective(list(precision="custom",pattern=patterns[i],labels=setNames("Public",expected[i])))
    value <- .datePresent(list(lo=as.Date("2020-01-01"),hi=as.Date("2020-01-01")),spec)
    expect_identical(as.character(value),"Public")
    spec$labels <- setNames("Bad",paste0(expected[i],"x"))
    expect_error(.datePresent(list(lo=as.Date("2020-01-01"),hi=as.Date("2020-01-01")),spec),"Label keys")
  }
  for (pattern in c("%Y-W%V","%%%Y-W%V","%G-%%V","%%G-W%V","%Y%")) {
    plan$variables$d1$date_spec$pattern <- pattern
    expect_error(.planNormalizeV2(plan),"date_pattern_invalid")
  }
})
