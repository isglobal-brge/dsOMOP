.date_policy_backends<-.ws7_scenario_backends()
withr::defer(.ws4_cleanup(.date_policy_backends),testthat::teardown_env())
.date_policy_var<-function(source,kind="calendar_date",spec=NULL,...) {
  x<-list(derived=c(list(kind=kind,reference=source),list(...)),broadcast=TRUE)
  if(!is.null(spec))x$date_spec<-spec
  x
}
test_that("D23 D24 native unindexed reducers and event-relative date roles", {
  h<-.date_policy_backends$sqlite$handle
  p<-.ws7_scenario_plan(list(first=list(source=list(event="lab"),reducer=list(fn="first_date"),date_spec=list(precision="day"))))
  p$populations$t<-list(kind="criteria")
  p$outputs$o$grain<-"person"
  f<-.ws7_scenario_run(h,p,frame=NULL)$frame
  expect_s3_class(f$first,"Date");expect_false(all(is.na(f$first)))
  p<-.ws7_scenario_plan(structure(list(),names=character()),output=list(format="membership"))
  p$options<-list(date_spec=list(precision="relative_days",anchor=list(source=list(event="lab"),which="first")))
  f<-.ws7_scenario_run(h,p)$frame
  expect_true(is.integer(f$cohort_start_date));expect_true(is.na(.ws7_value(f,7002,"cohort_start_date")))
})

test_that("D34 D36 components select one event row, keep missing people, and block identifiers", {
  h<-.date_policy_backends$sqlite$handle
  src<-list(kind="date_components",year=list(kind="integer",value=2024L),month=list(kind="integer",value=2L),day=list(kind="integer",value=29L),event=list(event="lab"))
  p<-.ws7_scenario_plan(list(day=.date_policy_var(src)))
  f<-.ws7_scenario_run(h,p)$frame
  expect_identical(.ws7_value(f,7001,"day"),as.Date("2024-02-29"))
  expect_true(is.na(.ws7_value(f,7002,"day")))
  p$variables$day$derived$reference$year<-list(kind="column",name="person_id")
  expect_error(.ws7_scenario_run(h,p),"date_source_invalid")
  p$variables$day$derived$reference$event<-NULL
  expect_error(.ws7_scenario_run(h,p),"date_source_invalid")
})

test_that("D38 paired invalid endpoints and D41 joined metadata remain typed", {
  s<-list(precision="month");cols<-list(a=.fragmentColumn("date",TRUE,"start"),b=.fragmentColumn("date",TRUE,"end"))
  f<-.dateApplyColumns(data.frame(a=as.Date("2024-02-01"),b=as.Date("2024-01-01")),cols,s)$frame
  expect_true(all(is.na(f$a)));expect_true(all(is.na(f$b)))
  h<-.date_policy_backends$sqlite$handle
  p<-.ws7_scenario_plan(list(d=.date_policy_var(list(date="2024-02-29")),age=.date_policy_var("index",kind="age_at")))
  p$outputs<-list(a=list(rows="t",format="wide",columns=list("d")),b=list(rows="t",format="wide",columns=list("age")),o=list(rows="t",format="joined",options=list(from=list(list(output="a"),list(output="b")),how="left",cardinality="one_to_one")))
  p$options<-list(date_spec=list(precision="quarter"));p$publish<-list("o")
  f<-.ws7_scenario_run(h,p)$frame
  expect_true(is.ordered(f$d));expect_identical(as.character(f$d),rep("2024-Q1",8));expect_true(is.integer(f$age))
  for(fmt in c("sparse","temporal_covariates","dictionary")) {
    p<-.ws7_scenario_plan(list(d=.date_policy_var("index")),output=list(format=fmt,columns=list("d")))
    if(fmt=="dictionary")p$outputs$o$options<-structure(list(),names=character()) else p$outputs$o$options<-list(sources=list(list(event="lab")),analyses=list("binary"))
    if(fmt=="temporal_covariates")p$outputs$o$options$window<-list(start=0L,end=30L,anchor="index")
    expect_error(.ws7_scenario_run(h,p),"no roster date-variable slot")
  }
})

test_that("D31 all native age units agree with independent birthday fixtures", {
  p<-.ws7_scenario_plan(setNames(lapply(c("days","weeks","months","years","decimal_years"),function(u).date_policy_var("index",kind="age_at",unit=u)),c("days","weeks","months","years","decimal_years")))
  f<-.ws7_scenario_run(.date_policy_backends$sqlite$handle,p)$frame
  expect_identical(.ws7_value(f,7001,"days"),18271L)
  expect_equal(.ws7_value(f,7001,"weeks"),18271/7)
  expect_identical(.ws7_value(f,7001,"months"),600L)
  expect_identical(.ws7_value(f,7001,"years"),50L)
  expect_equal(.ws7_value(f,7001,"decimal_years"),50+9/366)
})

test_that("D39 mixed native and legacy dates resolve policies per column", {
  p<-.ws7_scenario_plan(list(native=.date_policy_var("index"),legacy=list(raw=list(table="death",column="death_date"))))
  h<-.date_policy_backends$sqlite$handle
  a<-.ws7_scenario_run(h,p)$frame
  expect_s3_class(a$native,"Date");expect_true(is.integer(a$legacy))
  p$outputs$o$options<-list(date_handling=list(mode="relative",date_columns=list("legacy")))
  b<-.ws7_scenario_run(h,p)$frame
  expect_identical(a$native,b$native);expect_identical(a$legacy,b$legacy)
  n<-.planNormalizeV2(p);expect_identical(.planNormalizeV2(n),n)
  p$outputs$o$options$date_handling$date_columns<-list("native")
  expect_error(.planNormalizeV2(p),"date_policy_conflict")
  p$options<-list(date_spec=list(precision="month"));p$outputs$o$options$date_handling$date_columns<-list("legacy")
  expect_error(.planNormalizeV2(p),"date_policy_conflict")
  p$options<-NULL;p$outputs$o$options<-NULL;p$variables$legacy$date_spec<-list(precision="month")
  c<-.ws7_scenario_run(h,p)$frame
  expect_true(is.ordered(c$legacy))
  p$variables$bad<-list(source=list(event="lab"),reducer=list(fn="count"),date_spec=list(precision="month"))
  expect_error(.ws7_scenario_run(h,p),"numeric measures are not timestamps")
})
