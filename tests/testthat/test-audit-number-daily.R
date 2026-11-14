.audit_daily_plan <- function(within="mean",missing="skip",outer="mean",spec=NULL) {
  input<-list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
  if(!is.null(spec))input$number_spec<-spec
  p<-.ws7_scenario_plan(list(daily=list(derived=list(kind="number_daily",input=input,
    within_day=within,missing=missing,reducer=list(fn=outer))),
    raw=list(source=list(event="lab"),reducer=list(fn="mean"))))
  p$events$lab$unit<-list(target=8554L,unknown="missing",censored="keep")
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
  converted<-.audit_daily_plan(spec=list(unit=list(kind="fixed_unit",from="unit:8554",to="coordinate:declared_quantity",factor=2,offset=0)))
  expect_equal(.ws7_value(.ws7_scenario_run(h,converted)$frame,7001,"daily"),20)
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

test_that("Q15 episode windows and publication failures retain their private boundaries", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_keys(list(h))
  p<-.audit_daily_plan();p$variables$daily$derived$reducer$window<-list(start=0L,end=0L,anchor="index")
  episodes<-.ws7_scenario_frame(c(7001L,7001L),index=c("2020-01-01","2020-01-02"))
  f<-.ws7_scenario_run(h,p,episodes)$frame
  expect_equal(sort(f$daily),c(5,15))
  for(mode in c("memory","staged"))local({
    target<-new.env(parent=emptyenv());target$untouched<-"original"
    run<-.ws7_ctx(h,.audit_daily_plan(),mode=mode,target=target);on.exit(.ws7_close(run))
    pop<-.ws7_population(run,"t",.ws7_scenario_frame())
    run$ctx$failure_hook<-function(step)if(step=="assign")stop("injected daily assignment failure")
    expect_error(.ws7_publish(run,list(t=pop)),"injected daily assignment failure")
    expect_identical(ls(target),"untouched")
    expect_identical(target$untouched,"original")
    expect_true(run$ctx$closed)
  })
  p<-.audit_daily_plan();p$events$lab$on_missing_table<-"unavailable"
  DBI::dbExecute(h$conn,"DROP TABLE measurement");h$blueprint<-NULL;h$role_registry<-NULL
  expect_true(all(is.na(.ws7_scenario_run(h,p)$frame$daily)))
})
test_that("Q15 refuses scalar inputs and unresolved units without a fallback", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn))
  p<-.audit_daily_plan();p$events$lab$unit<-NULL
  expect_error(.ws7_scenario_run(h,p),"number_unit_conflict")
  p<-.audit_daily_plan();p$variables$daily$derived$input<-list(source=list(event="lab"),reducer=list(fn="mean"))
  expect_error(.ws7_scenario_run(h,p),"number_daily_input")
  expect_true("number.daily" %in% .irOperations(.planNormalizeV2(.audit_daily_plan())))
})

test_that("Q15 temporal selection, stable ties, ranges and publication preserve people", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_keys(list(h))
  p<-.audit_daily_plan("first",outer="first_value")
  expect_equal(.ws7_value(.ws7_scenario_run(h,p)$frame,7001,"daily"),3)
  DBI::dbExecute(h$conn,"CREATE TEMP TABLE daily_reordered AS SELECT * FROM measurement ORDER BY measurement_id DESC")
  DBI::dbExecute(h$conn,"DELETE FROM measurement")
  DBI::dbExecute(h$conn,"INSERT INTO measurement SELECT * FROM daily_reordered")
  expect_equal(.ws7_value(.ws7_scenario_run(h,p)$frame,7001,"daily"),3)
  p<-.audit_daily_plan();p$variables$daily$derived$reducer$window<-list(start=-8L,end=-8L,anchor="index")
  expect_equal(.ws7_value(.ws7_scenario_run(h,p)$frame,7001,"daily"),15)
  p<-.audit_daily_plan();p$variables$daily$number_spec<-list(representation="category",range="reference")
  expect_error(.ws7_scenario_run(h,p),"range|reference")
  for(mode in c("memory","staged"))local({
    run<-.ws7_ctx(h,.audit_daily_plan(),mode=mode);on.exit(.ws7_close(run),add=TRUE)
    pop<-.ws7_population(run,"t",.ws7_scenario_frame())
    got<-.ws7_publish(run,list(t=pop),threshold=5L)
    expect_identical(got$status$status,"ok")
    expect_true(10 %in% got$values$o$o$daily)
    expect_identical(attr(got$values$o$o,"omop_numbers")$daily$privacy_class,2L)
  })
  # Many repeated episodes from one person cannot satisfy the final gate.
  local({
  run<-.ws7_ctx(h,.audit_daily_plan());on.exit(.ws7_close(run),add=TRUE)
  rows<-.ws7_scenario_frame(rep(7001L,8),index=as.character(as.Date("2020-01-10")+0:7))
  pop<-.ws7_population(run,"t",rows)
  got<-.ws7_publish(run,list(t=pop),threshold=5L)
  expect_identical(got$status$status,"refused")
  })
})

test_that("Q15 multiple concepts remain separate days and foreign event operands refuse", {
  h<-.audit_daily_handle();on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"UPDATE measurement SET measurement_date='2020-01-01',measurement_concept_id=990999 WHERE measurement_id=970004")
  p<-.audit_daily_plan();p$events$lab$concept_set$concepts<-list(.WS7$LAB,990999L)
  expect_error(.ws7_scenario_run(h,p),"number_daily_quantity")
  p$variables$daily$derived$input$number_spec<-list(unit=list(kind="fixed_unit",from="unit:8554",to="coordinate:common_quantity",factor=1,offset=0))
  # Concept A mean 5 and concept B mean 15 are two concept-days, even
  # though both share one calendar date and an asserted common coordinate.
  expect_equal(.ws7_value(.ws7_scenario_run(h,p)$frame,7001,"daily"),10)
  q<-.audit_daily_plan();q$events$foreign<-q$events$lab
  x<-q$variables$daily$derived$input;y<-x;y$derived$source$event<-"foreign"
  q$variables$daily$derived$input<-list(derived=list(kind="number_derive",inputs=list(
    list(alias="x",variable=x,stage="canonical"),list(alias="y",variable=y,stage="canonical")),
    exprs=list(version=1L,assign=list(list(name="answer",expr=list(op="+",args=list(list(col="x"),list(col="y")))))),
    unit="coordinate:common_quantity",result="answer"))
  expect_error(.ws7_scenario_run(h,q),"number_dependency_invalid")
})
