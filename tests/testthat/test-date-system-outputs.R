.date_backends<-.ws7_scenario_backends()
withr::defer(.ws4_cleanup(.date_backends),testthat::teardown_env())
.date_var<-function(source,kind="calendar_date",spec=NULL,...) {
  x<-list(derived=c(list(kind=kind,reference=source),list(...)),broadcast=TRUE)
  if(!is.null(spec))x$date_spec<-spec
  x
}
test_that("D01 D24 D29 D30 native private dates, reducers and ages on every backend", {
  vars<-list(early=.date_var(list(date="0001-01-01")),leap=.date_var(list(date="0004-02-29")),
    century=.date_var(list(date="0999-12-31")),millennium=.date_var(list(date="1000-01-01")),
    elapsed=list(derived=list(kind="time_between",from=list(date="0999-12-31"),to=list(date="1000-01-01"))),
    first=list(source=list(event="lab"),reducer=list(fn="first_date"),date_spec=structure(list(),names=character())),
    last=list(source=list(event="lab"),reducer=list(fn="last_date"),date_spec=list(precision="month")),
    age=.date_var("index",kind="age_at"),
    visit=.date_var(list(source=list(event="lab"),which="first")))
  plan<-.ws7_scenario_plan(vars)
  for(b in names(.date_backends)) {
    f<-.ws7_scenario_run(.date_backends[[b]]$handle,plan)$frame
    expect_identical(f$early,rep(as.Date("0001-01-01"),8),info=b)
    expect_identical(f$leap,rep(as.Date("0004-02-29"),8),info=b)
    expect_identical(f$elapsed,rep(1L,8),info=b)
    expect_identical(f$century+1,f$millennium,info=b)
    expect_identical(f$first,f$visit,info=b)
    expect_true(is.ordered(f$last),info=b)
    expect_identical(.ws7_value(f,7001,"age"),50L,info=b)
    expect_true(is.na(.ws7_value(f,7002,"visit")),info=b)
  }
})
test_that("D38 D41 long, membership, intervals and panels present endpoints after arithmetic", {
  for(b in names(.date_backends)) for(p in c("day","year","semester","quarter","month","iso_week","cdc_week","epi_week","fiscal_year","fiscal_semester","fiscal_quarter","fiscal_month","custom")) {
    spec<-list(precision=p)
    if(p=="epi_week")spec<-c(spec,list(week_start=1L,week_min_days=4L))
    if(p=="custom")spec$pattern<-"%Y-%m"
    for(fmt in c("long","membership","intervals","person_period")) {
      opt<-switch(fmt,long=list(sources=list(list(event="lab")),columns=list("part")),membership=list(),intervals=list(options=list(sources=list(list(event="drug")))),person_period=list(options=list(sources=list(list(event="lab")),bins=list(width=10L),window=list(start=0L,end=20L,anchor="index"),analyses=list("mean"))))
      plan<-.ws7_scenario_plan(list(part=.date_var("index",kind="date_part",part="month")),output=c(list(format=fmt),opt))
      plan$options<-list(date_spec=spec)
      f<-.ws7_scenario_run(.date_backends[[b]]$handle,plan)$frame
      endpoints<-intersect(c("start_date","end_date","cohort_start_date","cohort_end_date","period_start_date","period_end_date"),names(f))
      expect_true(length(endpoints)>0,info=paste(b,p,fmt))
      for(col in endpoints) if(p=="day")expect_s3_class(f[[col]],"Date") else expect_true(is.ordered(f[[col]]),info=paste(b,p,fmt,col))
      if(fmt=="person_period")expect_true(all(f$days==f$period_end_days-f$period_start_days+1L))
    }
  }
})

test_that("D41 survival date covariates and endpoints preserve relative times", {
  h<-.date_backends$sqlite$handle
  p<-.ws7_scenario_plan(list(age=.date_var("index",kind="age_at")),output=list(format="survival",columns=list("age"),options=list(outcomes=list(dx=list(event="dx")),tar=list(start=0L,end=30L,anchor="index"),censor=list("observation_end"))))
  p$options<-list(date_spec=list(precision="month"))
  f<-.ws7_scenario_run(h,p)$frame
  expect_true(is.integer(f$age));expect_true(is.ordered(f$entry_date));expect_true(is.ordered(f$exit_date))
  expect_identical(f$follow_up_days,f$exit_days_from_index-f$entry_days_from_index)
  p$outputs$o<-list(rows="t",format="sparse",columns=list("age"),options=list(sources=list(list(event="dx")),analyses=list("binary")))
  expect_error(.ws7_scenario_run(h,p),"no roster date-variable slot")
})

test_that("D23 D24 native unindexed reducers and event-relative date roles", {
  h<-.date_backends$sqlite$handle
  p<-.ws7_scenario_plan(list(first=list(source=list(event="lab"),reducer=list(fn="first_date"),date_spec=list(precision="day"))))
  p$populations$t<-list(kind="criteria")
  frame<-.ws7_scenario_frame();frame$cohort_start_date[]<-as.Date(NA)
  f<-.ws7_scenario_run(h,p,frame=frame)$frame
  expect_s3_class(f$first,"Date");expect_false(all(is.na(f$first)))
  p<-.ws7_scenario_plan(list(),output=list(format="membership"))
  p$options<-list(date_spec=list(precision="relative_days",anchor=list(source=list(event="lab"),which="first")))
  f<-.ws7_scenario_run(h,p)$frame
  expect_true(is.integer(f$cohort_start_date));expect_true(is.na(.ws7_value(f,7002,"cohort_start_date")))
})

test_that("D34 D36 components select one event row, keep missing people, and block identifiers", {
  h<-.date_backends$sqlite$handle
  src<-list(kind="date_components",year=list(kind="integer",value=2024L),month=list(kind="integer",value=2L),day=list(kind="integer",value=29L),event=list(event="lab"))
  p<-.ws7_scenario_plan(list(day=.date_var(src)))
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
  h<-.date_backends$sqlite$handle
  p<-.ws7_scenario_plan(list(d=.date_var(list(date="2024-02-29")),age=.date_var("index",kind="age_at")))
  p$outputs<-list(a=list(rows="t",format="wide",columns=list("d")),b=list(rows="t",format="wide",columns=list("age")),o=list(rows="t",format="joined",options=list(from=list(list(output="a"),list(output="b")),how="left",cardinality="one_to_one")))
  p$options<-list(date_spec=list(precision="quarter"));p$publish<-list("o")
  f<-.ws7_scenario_run(h,p)$frame
  expect_true(is.ordered(f$d));expect_identical(as.character(f$d),rep("2024-Q1",8));expect_true(is.integer(f$age))
  for(fmt in c("sparse","temporal_covariates","dictionary")) {
    p<-.ws7_scenario_plan(list(d=.date_var("index")),output=list(format=fmt,columns=list("d")))
    if(fmt=="dictionary")p$outputs$o$options<-list() else p$outputs$o$options<-list(sources=list(list(event="lab")),analyses=list("binary"))
    if(fmt=="temporal_covariates")p$outputs$o$options$window<-list(start=0L,end=30L,anchor="index")
    expect_error(.ws7_scenario_run(h,p),"no roster date-variable slot")
  }
})
