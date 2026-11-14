.audit_study_api <- function() {
  env<-new.env(parent=asNamespace("dsOMOPClient"))
  sys.source(system.file("examples","audit-numeric-studies.R",package="dsOMOPClient"),env)
  env
}
.audit_study_handle <- function(values,offsets,person=7001:7008) {
  h<-.ws4_reach_handle();.ws7_extend(h$conn)
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE person_id BETWEEN 7001 AND 7008")
  x<-expand.grid(record=seq_along(values),person_id=person)
  DBI::dbWriteTable(h$conn,"measurement",data.frame(measurement_id=980000L+seq_len(nrow(x)),person_id=x$person_id,
    measurement_concept_id=.WS7$LAB,measurement_date=as.character(as.Date("2020-01-10")+offsets[x$record]),
    measurement_type_concept_id=0L,value_as_number=values[x$record],unit_concept_id=8554L),append=TRUE)
  DBI::dbExecute(h$conn,"UPDATE observation_period SET observation_period_start_date='2010-01-01',observation_period_end_date='2030-01-01'")
  h$blueprint<-NULL;h$role_registry<-NULL;h
}
.audit_study_plan <- function(vars) {
  p<-dsOMOPClient::recipe_to_plan(dsOMOPClient::omop_recipe(variables=vars),version=2)
  result<-.ws7_scenario_plan(p$variables);result$events<-p$events;result
}
.audit_study_event <- function()dsOMOPClient::omop_event("measurement",.WS7$LAB,name="lab",unit=dsOMOPClient::omop_unit(8554L))
.audit_study_published <- function(h,p,expected,mode="memory") {
  .ws7_keys(list(h));run<-.ws7_ctx(h,p,mode=mode);on.exit(.ws7_close(run))
  pop<-.ws7_population(run,"t",.ws7_scenario_frame())
  got<-.ws7_publish(run,list(t=pop))
  expect_identical(got$status$status,"ok")
  f<-got$values$o$o
  for(n in names(expected))expect_equal(f[[n]],rep(expected[[n]],nrow(f)),tolerance=1e-12)
  invisible(f)
}
test_that("Q10 named occurrence windows include the declared day-zero boundary", {
  a<-.audit_study_api();h<-.audit_study_handle(rep(1,8),c(-366,-365,-181,-180,-31,-30,0,1));on.exit(DBI::dbDisconnect(h$conn))
  for(pre in c(FALSE,TRUE)) {
    windows<-a$audit_windows(pre)
    vars<-lapply(windows,function(w)dsOMOPClient::omop_variable(event=.audit_study_event(),reducer=dsOMOPClient::omop_reduce("count",window=w)))
    .audit_study_published(h,.audit_study_plan(vars),as.list(setNames(if(pre)c(5L,3L,1L) else c(6L,4L,2L),names(windows))))
  }
})
test_that("Q16 independent visit schedules produce distinct baseline comparisons", {
  a<-.audit_study_api()
  for(site in 1:2)local({
    h<-.audit_study_handle(if(site==1)c(10,15) else c(10,20),if(site==1)c(0,180) else c(-5,190));on.exit(DBI::dbDisconnect(h$conn))
    vars<-a$audit_numeric_studies(.audit_study_event())[c("baseline","followup","difference","ratio","percent_change")]
    .audit_study_published(h,.audit_study_plan(vars),if(site==1)list(baseline=10,followup=15,difference=5,ratio=1.5,percent_change=50) else list(baseline=10,followup=20,difference=10,ratio=2,percent_change=100),mode=if(site==1)"memory" else "staged")
  })
})
test_that("Q17 eligible observation denominators and Q18 prior dates run as emitted", {
  a<-.audit_study_api();h<-.audit_study_handle(c(4,6,10,NA),c(-1,0,0,1));on.exit(DBI::dbDisconnect(h$conn))
  vars<-a$audit_numeric_studies(.audit_study_event())[c("observation_fraction","above_count","previous_days_min","current_day_max","day_ratio")]
  .audit_study_published(h,.audit_study_plan(vars),list(observation_fraction=2/3,above_count=1,previous_days_min=4,current_day_max=10,day_ratio=2.5))
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number=NULL")
  f<-.ws7_scenario_run(h,.audit_study_plan(vars))$frame
  expect_true(all(is.na(f$observation_fraction)))
  expect_true(all(is.na(f$day_ratio)))
})
test_that("Q19 before and after reduction express different declared cleaning", {
  a<-.audit_study_api();h<-.audit_study_handle(c(170,400),c(0,1));on.exit(DBI::dbDisconnect(h$conn))
  vars<-a$audit_numeric_studies(.audit_study_event())[c("raw_mean","clean_mean","clip_mean","after_mean")]
  .audit_study_published(h,.audit_study_plan(vars),list(raw_mean=285,clean_mean=170,clip_mean=210,after_mean=NA_real_),mode="staged")
})
test_that("Q20 integer validity precedes the full declared ordinal scale", {
  a<-.audit_study_api();h<-.audit_study_handle(0,0);on.exit(DBI::dbDisconnect(h$conn))
  values<-c(0,10,2.5,11,NA,0,10,2.5)
  for(i in seq_along(values))DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number=? WHERE person_id=?",params=list(values[i],7000L+i))
  raw<-dsOMOPClient::omop_variable(event=.audit_study_event(),reducer=dsOMOPClient::omop_reduce("last_value"))
  vars<-a$audit_pain_scale(raw);p<-.audit_study_plan(vars)
  got<-.ws7_scenario_run(h,p)$frame
  expect_equal(got$numeric[match(7001:7008,got$pid)],c(0,10,NA,NA,NA,0,10,NA))
  .ws7_keys(list(h));run<-.ws7_ctx(h,p);on.exit(.ws7_close(run),add=TRUE,after=FALSE)
  pop<-.ws7_population(run,"t",.ws7_scenario_frame());published<-.ws7_publish(run,list(t=pop))
  expect_identical(published$status$status,"ok")
  expect_identical(levels(published$values$o$o$ordinal),as.character(0:10))
})
