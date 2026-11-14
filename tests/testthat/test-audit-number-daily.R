.audit_daily_plan <- function(within="mean",missing="skip",outer="mean",spec=NULL) {
  input<-list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
  if(!is.null(spec))input$number_spec<-spec
  p<-.ws7_scenario_plan(list(daily=list(derived=list(kind="number_daily",input=input,
    within_day=within,missing=missing,reducer=list(fn=outer))),
    raw=list(source=list(event="lab"),reducer=list(fn="mean"))))
  p$events$lab$unit<-list(target=8554L,accepted=list(8554L),on_unmapped="missing")
  p
}
.audit_daily_handle <- function() {
  h<-.ws4_reach_handle();.ws7_extend(h$conn);h$blueprint<-NULL;h$role_registry<-NULL
  DBI::dbExecute(h$conn,"UPDATE measurement SET measurement_date = CASE WHEN measurement_id <= 970003 THEN '2020-01-01' ELSE '2020-01-02' END, value_as_number = CASE measurement_id WHEN 970001 THEN 3 WHEN 970002 THEN 3 WHEN 970003 THEN 9 WHEN 970004 THEN 15 ELSE NULL END WHERE person_id=7001")
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE measurement_id=970005")
  h
}
test_that("Q15 record and calendar-day weights are distinct private computations", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn))
  for(within in c("mean","max","first")) {
    p<-.audit_daily_plan(within,outer=if(within=="first")"last_value" else "mean")
    got<-.ws7_scenario_run(h,p)$frame
    expect_equal(.ws7_value(got,7001,"raw"),7.5)
    expect_equal(.ws7_value(got,7001,"daily"),switch(within,mean=10,max=12,first=15))
    expect_true(is.na(.ws7_value(got,7003,"daily")))
    expect_true(is.na(.ws7_value(got,7002,"daily")))
  }
})
test_that("Q15 missing stages, input conversion and final presentation do not commute", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number=NULL WHERE measurement_id=970002")
  skip<-.ws7_scenario_run(h,.audit_daily_plan())$frame
  prop<-.ws7_scenario_run(h,.audit_daily_plan(missing="propagate"))$frame
  expect_equal(.ws7_value(skip,7001,"daily"),10.5)
  expect_equal(.ws7_value(prop,7001,"daily"),15)
  p<-.audit_daily_plan(spec=list(transform=list(list(method="clip",lower=0,upper=10))))
  expect_equal(.ws7_value(.ws7_scenario_run(h,p)$frame,7001,"daily"),8)
  p<-.audit_daily_plan();p$variables$daily$number_spec<-list(transform=list(list(method="clip",lower=0,upper=10)))
  expect_equal(.ws7_value(.ws7_scenario_run(h,p)$frame,7001,"daily"),10)
})
test_that("Q15 refuses scalar inputs and unresolved units without a fallback", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn))
  p<-.audit_daily_plan();p$events$lab$unit<-NULL
  expect_error(.ws7_scenario_run(h,p),"number_unit_conflict")
  p<-.audit_daily_plan();p$variables$daily$derived$input<-list(source=list(event="lab"),reducer=list(fn="mean"))
  expect_error(.ws7_scenario_run(h,p),"number_daily_input")
  expect_true("number.daily" %in% .irRequiredOperations(.planNormalizeV2(.audit_daily_plan())))
})
