.number_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.number_backends),testthat::teardown_env())
.number_svar <- function(fn,s=NULL,...) {
  v<-list(source=list(event="lab"),reducer=c(list(fn=fn),list(...)))
  if(!is.null(s))v$number_spec<-s
  v
}
test_that("N01 N13 N14 N25 N26 N36 N37 N39 numeric representations compile on every backend", {
  vars<-list(raw=.number_svar("mean",.pnv2EmptyObject()),floor=.number_svar("mean",list(rounding=list(method="floor"))),
    flag=.number_svar("mean",list(representation="flag",op="ge",thresholds=list(5))),
    bins=.number_svar("mean",list(representation="bins",breaks=list("-Inf",5,10,"Inf"))),
    clinical=.number_svar("last_value",list(representation="category")))
  p<-.ws7_scenario_plan(vars)
  for(b in names(.number_backends)) {
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,p)$frame
    expect_equal(f$floor,floor(f$raw),info=b)
    expect_identical(f$flag,as.integer(f$raw>=5),info=b)
    expect_true(is.ordered(f$bins));expect_identical(levels(f$bins),c("(-Inf,5)","[5,10)","[10,Inf)"))
    expect_true(is.ordered(f$clinical));expect_identical(levels(f$clinical),c("low","normal","high"))
  }
})
test_that("N23 N30 N31 N32 N33 N34 source computations keep every roster row", {
  vars<-list()
  for(m in c("difference","percent_change","ratio","slope","time_to_threshold")) {
    d<-list(kind="number_change",source=list(event="lab"),value="value",method=m,missing="skip")
    if(m=="time_to_threshold")d$threshold<-5
    vars[[m]]<-list(derived=d)
  }
  vars$n<-list(derived=list(kind="number_count",source=list(event="lab"),value="value",count_kind="nonmissing"))
  vars$missing<-list(derived=list(kind="number_count",source=list(event="lab"),value="value",count_kind="missing"))
  p<-.ws7_scenario_plan(vars)
  for(b in names(.number_backends)) {
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,p)$frame
    expect_equal(nrow(f),8L,info=b)
    expect_true(is.integer(f$n));expect_true(is.integer(f$missing))
    expect_equal(.ws7_value(f,7002,"n"),0L)
    expect_true(is.na(.ws7_value(f,7002,"difference")))
  }
})
test_that("N54 N55 N57 N58 N59 private expressions bind canonical and presented scalars", {
  req<-list(version=1L,assign=list(list(name="y",expr=list(op="*",args=list(list(col="x"),list(lit=2))))))
  input<-.number_svar("mean",list(rounding=list(method="floor")))
  vars<-list(raw=.number_svar("mean"))
  for(stage in c("canonical","presented"))vars[[stage]]<-list(derived=list(kind="number_derive",inputs=list(list(alias="x",variable=input,stage=stage)),exprs=req,unit="coordinate:doubled"))
  rows<-list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
  vars$total<-list(derived=list(kind="number_derive",inputs=list(list(alias="x",variable=rows,stage="canonical")),exprs=req,unit="coordinate:doubled",reducer=list(fn="sum")))
  vars$sum<-.number_svar("sum")
  for(b in names(.number_backends)) {
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,.ws7_scenario_plan(vars))$frame
    expect_equal(f$canonical,2*f$raw,info=b)
    expect_equal(f$presented,2*floor(f$raw),info=b)
    expect_equal(f$total,2*f$sum,info=b)
  }
})
test_that("N74 generated period columns bind original rows before display", {
  p<-.ws7_scenario_plan(.pnv2EmptyObject(),output=list(format="person_period",options=list(sources=list(list(event="lab")),bins=list(width=10L),window=list(start=0L,end=20L,anchor="index"),analyses=list("mean"))))
  leaf<-function(n,s=NULL) {v<-list(derived=list(kind="number_column",column=n));if(!is.null(s))v$number_spec<-s;v}
  p$outputs$o$number_columns<-list(days_raw=leaf("days",list(pipeline=list(list(kind="transform",method="identity")))),days_band=leaf("days",list(representation="bins",breaks=list(0,7,14,"Inf"))))
  for(b in names(.number_backends)) {
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,p)$frame
    expect_equal(f$days_raw,f$days,info=b)
    expect_true(is.ordered(f$days_band))
  }
})

test_that("N48 N52 N57 N65 N74 copies bind hidden original columns and joined canonical values", {
  copy<-function(n,s=NULL) {v<-list(derived=list(kind="number_column",column=n));if(!is.null(s))v$number_spec<-s;v}
  raw<-list(pipeline=list(list(kind="transform",method="identity")))
  p<-.ws7_scenario_plan(list(x=.number_svar("mean",list(rounding=list(method="floor")))))
  p$outputs$o$columns<-list("raw")
  p$outputs$o$number_columns<-list(raw=copy("x",raw))
  for(b in names(.number_backends)) {
    h<-.number_backends[[b]]$handle
    f<-.ws7_scenario_run(h,p)$frame
    baseline<-.ws7_scenario_run(h,.ws7_scenario_plan(list(x=.number_svar("mean"))))$frame
    expect_false("x" %in% names(f));expect_equal(f$raw,baseline$x,info=b)
  }
})
test_that("N60 N62 N64 N74 generated formulas retain canonical denominator and class 2", {
  cols<-list(person_id=.fragmentColumn("text",FALSE,"person_key"),n=.numberGenerated(.fragmentColumn("integer",FALSE,"measure"),"count"),days=.numberGenerated(.fragmentColumn("integer",FALSE,"measure"),"days",2L))
  f<-data.frame(person_id=c("p","q"),n=c(2L,0L),days=c(10L,20L))
  leaf<-function(n,s=NULL) {v<-list(derived=list(kind="number_column",column=n));if(!is.null(s))v$number_spec<-s;v}
  req<-list(version=1L,assign=list(list(name="rate",expr=list(op="/",args=list(list(op="*",args=list(list(lit=365250),list(col="n"))),list(col="d"))))))
  formula<-list(derived=list(kind="number_derive",inputs=list(list(alias="n",variable=leaf("n"),stage="canonical"),list(alias="d",variable=leaf("days"),stage="canonical")),exprs=req,result="rate",unit="coordinate:per_1000_person_years"),number_spec=list(pipeline=list(list(kind="transform",method="identity"))))
  node<-list(format="person_period",number_columns=list(rate=formula,raw=leaf("days",list(pipeline=list(list(kind="transform",method="identity"))))))
  out<-.numberOutputColumns(f,cols,node,"o","o",list(unit=list(kind="fixed_unit",from="days",to="weeks",factor=1/7)))
  expect_equal(out$frame$rate,c(73050,0));expect_equal(out$frame$days,c(10,20)/7)
  expect_equal(out$frame$n,c(2L,0L));expect_equal(out$frame$raw,c(10L,20L))
  expect_identical(attr(out$columns$rate,"number_contract")$privacy_class,2L)
  # A joined compiler component carries its canonical branch privately.
  again<-.numberOutputColumns(out$frame,out$columns,list(format="joined",number_columns=list(days2=leaf("days",list(pipeline=list(list(kind="transform",method="identity")))))),"j","j",canonical=out$canonical,canonical_columns=out$canonical_columns)
  expect_equal(again$frame$days2,c(10L,20L))
})
test_that("N27 native numeric CV is a dimensionless ratio", {
  for(b in names(.number_backends)) {
    p<-.ws7_scenario_plan(list(cv=.number_svar("cv",.pnv2EmptyObject()),sd=.number_svar("sd"),mean=.number_svar("mean")))
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,p)$frame
    expect_equal(f$cv,f$sd/f$mean,info=b)
  }
})

test_that("N48 N58 N67 N72 native numeric rows and event expressions align without collapse", {
  rows<-list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
  req<-list(version=1L,assign=list(list(name="y",expr=list(op="*",args=list(list(col="x"),list(lit=2))))))
  vars<-list(raw=rows,doubled=list(derived=list(kind="number_derive",inputs=list(list(alias="x",variable=rows,stage="canonical")),exprs=req,unit="coordinate:doubled")))
  for(fmt in c("long","intervals"))for(b in names(.number_backends)) {
    out<-list(format=fmt,sources=list(list(event="lab")),columns=as.list(names(vars)),options=.pnv2EmptyObject())
    if(fmt=="long")out$options<-list(columns_derived=list("value")) else {out$options<-list(sources=out$sources);out$sources<-NULL}
    p<-.ws7_scenario_plan(vars,output=out)
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,p)$frame
    expect_gt(nrow(f),0L);expect_equal(f$doubled,2*f$raw,info=paste(b,fmt))
    expect_null(attr(f$raw,"number_ranges"));expect_false(inherits(f$raw,"dsomop_number_value"))
  }
})

test_that("N46 N49 N66 final support gates preserve rare private categories and exact integers", {
  .ws7_keys(lapply(.number_backends,function(b)b$handle))
  p<-.ws7_scenario_plan(list(raw=.number_svar("mean",.pnv2EmptyObject()),band=.number_svar("mean",list(representation="bins",breaks=list("-Inf",5,6,"Inf")))))
  for(b in names(.number_backends))for(mode in c("memory","staged"))for(size in c(5L,4L)) local({
    run<-.ws7_ctx(.number_backends[[b]]$handle,p,mode=mode)
    on.exit(.ws7_close(run),add=TRUE)
    pop<-.ws7_population(run,"t",.ws7_scenario_frame((7001:7008)[seq_len(size)]))
    got<-.ws7_publish(run,list(t=pop),threshold=5L)
    expect_identical(got$status$status,if(size==5L)"ok" else "refused",info=paste(b,mode,size))
    if(size==5L) {
      f<-got$values$o$o
      expect_true(is.ordered(f$band));expect_equal(nrow(f),5L)
      expect_null(attr(f$raw,"number_ranges"))
      expect_false(any(c("range_low","range_high","unit_unknown") %in% names(attr(f,"omop_numbers")$raw)))
    } else expect_length(got$values,0L)
  })
  x<-list(start_date=as.Date("2020-01-01")+0:1,v=c("9007199254740993","9007199254740995"))
  expect_identical(.reduceOne("first_value",x,list(),value_type="bigint"),"9007199254740993")
  expect_identical(.reduceOne("min",x,list(),value_type="bigint"),"9007199254740993")
  expect_error(.reduceOne("mean",x,list(),value_type="bigint"),"retain raw/count")
})

test_that("N35 N73 independent index origin and elapsed class 2 compile", {
  vars<-list(cross=list(derived=list(kind="number_change",source=list(event="lab"),value="value",method="time_to_threshold",missing="skip",reference="index",time_unit="weeks",threshold=5,op="ge")),elapsed=list(derived=list(kind="time_between",from=list(date="2020-01-01"),to=list(date="2020-01-15"),unit="days"),number_spec=list(unit=list(kind="fixed_unit",from="days",to="weeks",factor=1/7))))
  for(b in names(.number_backends)) {
    f<-.ws7_scenario_run(.number_backends[[b]]$handle,.ws7_scenario_plan(vars))$frame
    expect_equal(f$elapsed,rep(2,nrow(f)))
    expect_true(all(is.na(f$cross)|f$cross>=0))
  }
})
test_that("N49 exact private count materializations survive identity and Parquet", {
  skip_if_not_installed("arrow")
  h<-.number_backends[[1]]$handle;.ws7_keys(list(h))
  # Materialize the independently known count, without allocating 2^53 events.
  testthat::local_mocked_bindings(.numberEventRecord=function(ctx,node,name,cons) {
    list(columns=setNames(list(.fragmentColumn("bigint",TRUE,"measure")),name),available=TRUE,
      declared_empty_value=0L,evaluate=function() {
        out<-data.frame(row_key=.reduceRosterFrame(ctx,cons$roster)$cohort_row_id)
        out[[name]]<-bit64::as.integer64(rep("9007199254740993",nrow(out)));out
      })
  })
  v<-list(derived=list(kind="number_count",source=list(event="lab"),value="value",count_kind="nonmissing"))
  p<-.ws7_scenario_plan(list(raw=v,identity=utils::modifyList(v,list(number_spec=list(pipeline=list(list(kind="transform",method="identity"))))),none=utils::modifyList(v,list(number_spec=list(rounding=list(method="none"))))))
  for(mode in c("memory","staged"))local({
    run<-.ws7_ctx(h,p,mode=mode);on.exit(.ws7_close(run),add=TRUE)
    pop<-.ws7_population(run,"t",.ws7_scenario_frame())
    got<-.ws7_publish(run,list(t=pop),threshold=5L)
    expect_identical(got$status$status,"ok")
    for(n in c("raw","identity","none")) {
      expect_s3_class(got$values$o$o[[n]],"integer64")
      expect_identical(as.character(got$values$o$o[[n]]),rep("9007199254740993",8))
    }
  })
})

test_that("N71 carry-forward expiry uses canonical event time before numeric display", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn),add=TRUE)
  .ws7_extend(h$conn);h$blueprint<-NULL;h$role_registry<-NULL
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE person_id = 7001 AND measurement_id <> 970004")
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number = 9.96 WHERE measurement_id = 970004")
  for(start in c(0L,5L,9L)) {
    p<-.ws7_scenario_plan(.pnv2EmptyObject(),output=list(format="person_period",options=list(sources=list(list(event="lab")),analyses=list("mean"),window=list(start=start,end=start,anchor="index"),bins=list(width=1L),carry_forward=list(lab=list(lookback=10L,expire=7L)))))
    p$options<-list(number_spec=list(rounding=list(method="decimals",digits=1)))
    p$outputs$o$number_columns<-list(raw=list(derived=list(kind="number_column",column="lab_carry"),number_spec=list(pipeline=list(list(kind="transform",method="identity")))))
    f<-.ws7_scenario_run(h,p,.ws7_scenario_frame(7001))$frame
    expect_equal(f$raw,if(start<=7)9.96 else NA_real_)
    expect_equal(f$lab_carry,if(start<=7)10 else NA_real_)
    expect_equal(nrow(f),1L)
  }
})

test_that("N55 N59 N68 prespecified selections precede scalar differences", {
  b<-.number_svar("closest_value",list(unit=list(kind="fixed_unit",from="unit:8554",to="coordinate:lab",factor=2)),window=list(start=-9L,end=-9L,anchor="index"))
  f<-.number_svar("closest_value",list(unit=list(kind="fixed_unit",from="unit:8554",to="coordinate:lab",factor=5)),window=list(start=0L,end=0L,anchor="index"))
  req<-list(version=1L,assign=list(list(name="change",expr=list(op="-",args=list(list(col="f"),list(col="b"))))))
  v<-list(derived=list(kind="number_derive",inputs=list(list(alias="b",variable=b,stage="canonical"),list(alias="f",variable=f,stage="canonical")),exprs=req,unit="coordinate:change"))
  for(backend in names(.number_backends)) {
    p<-.ws7_scenario_plan(list(change=v));p$events$lab$unit<-list(target=8554L)
    got<-.ws7_scenario_run(.number_backends[[backend]]$handle,p)$frame
    expect_equal(.ws7_value(got,7001,"change"),5)
    expect_true(is.na(.ws7_value(got,7002,"change")))
  }
})
test_that("N58 N59 N69 nested weighted sums use one identical complete-pair mask", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn),add=TRUE)
  .ws7_extend(h$conn);h$blueprint<-NULL;h$role_registry<-NULL
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE person_id = 7001 AND measurement_id = 970005")
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number = CASE measurement_id WHEN 970001 THEN 2 WHEN 970002 THEN NULL WHEN 970003 THEN 8 WHEN 970004 THEN 4 END, range_high = CASE measurement_id WHEN 970001 THEN 1 WHEN 970002 THEN 100 WHEN 970003 THEN 3 WHEN 970004 THEN 0 END WHERE person_id = 7001")
  leaf<-function(value)list(derived=list(kind="number_rows",source=list(event="lab"),value=value))
  inputs<-list(list(alias="x",variable=leaf("value"),stage="canonical"),list(alias="w",variable=leaf("range_high"),stage="canonical"))
  mask<-list(and=list(list(var="x",op="not_null"),list(var="w",op="not_null"),list(var="w",op=">=",value=0)))
  make<-function(expr)list(derived=list(kind="number_derive",inputs=inputs,exprs=list(version=1L,assign=list(list(name="v",expr=list(ifelse=list(test=mask,yes=expr,no=list(lit=0)))))),unit="coordinate:weighted",reducer=list(fn="sum")))
  numerator<-make(list(op="*",args=list(list(col="x"),list(col="w"))))
  denominator<-make(list(col="w"))
  weighted<-list(derived=list(kind="number_derive",inputs=list(list(alias="n",variable=numerator,stage="canonical"),list(alias="d",variable=denominator,stage="canonical")),exprs=list(version=1L,assign=list(list(name="ratio",expr=list(op="/",args=list(list(col="n"),list(col="d")))))),unit="coordinate:weighted_mean"))
  got<-.ws7_scenario_run(h,.ws7_scenario_plan(list(weighted=weighted)))$frame
  expect_equal(.ws7_value(got,7001,"weighted"),6.5)
  expect_true(is.na(.ws7_value(got,7002,"weighted")))
  expect_false(any(c("x","w","n","d") %in% names(got)))
})

test_that("N64 PDC intersects exposure with disjoint observation coverage before division", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn),add=TRUE)
  .ws7_extend(h$conn);h$blueprint<-NULL;h$role_registry<-NULL
  DBI::dbExecute(h$conn,"DELETE FROM observation_period WHERE person_id = 7001")
  DBI::dbWriteTable(h$conn,"observation_period",data.frame(observation_period_id=c(999901L,999902L),person_id=7001L,observation_period_start_date=c("2020-01-01","2020-01-09"),observation_period_end_date=c("2020-01-02","2020-01-10"),period_type_concept_id=44814724L),append=TRUE)
  window<-list(start=-9L,end=0L,anchor="index")
  covered<-list(source=list(event="drug"),reducer=list(fn="covered_days",observed_only=TRUE,window=window))
  observed<-list(derived=list(kind="observed_days",window=window))
  req<-list(version=1L,assign=list(list(name="pdc",expr=list(op="/",args=list(list(col="covered"),list(col="observed"))))))
  pdc<-list(derived=list(kind="number_derive",inputs=list(list(alias="covered",variable=covered,stage="canonical"),list(alias="observed",variable=observed,stage="canonical")),exprs=req,unit="proportion"))
  p<-.ws7_scenario_plan(list(covered=covered,observed=observed,pdc=pdc))
  p$populations$t$coverage<-list(policy="union",gap=6L)
  got<-.ws7_scenario_run(h,p)$frame
  expect_equal(.ws7_value(got,7001,"covered"),4L)
  expect_equal(.ws7_value(got,7001,"observed"),4L)
  expect_equal(.ws7_value(got,7001,"pdc"),1)
})

test_that("N24 N39 generated unavailable measures and structural survival keys retain their contracts", {
  col<-.numberGenerated(.fragmentColumn("integer",TRUE,"measure"),"count",available=FALSE)
  got<-.numberOutputColumns(data.frame(n_case=NA_integer_),list(n_case=col),list(format="person_period"),"p","p",recipe=list(representation="present"))
  expect_true(is.na(got$frame$n_case))
  for(name in c("event","from","to","trans","state_visit_number","event_number","outcome_event_number","weight"))
    expect_false(.numberEligibleColumn(.fragmentColumn("integer",TRUE,"measure"),name,list(format="survival")))
})

test_that("N74 original schema bindings reject invalid declarations and select generated columns", {
  c<-.fragmentColumn("numeric",TRUE,"measure")
  leaf<-function(key,component=NULL) {d<-list(kind="number_column",column=key);if(!is.null(component))d$component<-component;list(derived=d)}
  node<-list(format="person_period",number_columns=list(copy=leaf("days")))
  expect_silent(.numberPreflightColumns(node,list(p=list(days=c))))
  node$number_columns$copy<-leaf("unknown")
  expect_error(.numberPreflightColumns(node,list(p=list(days=c))),"original numeric")
  node$number_columns$copy<-leaf("days")
  expect_error(.numberPreflightColumns(node,list(p=list(days=c),q=list(days=c))),"component explicitly")
  node$number_columns$copy<-leaf("days","absent")
  expect_error(.numberPreflightColumns(node,list(p=list(days=c))),"Unknown output component")
  p<-.ws7_scenario_plan(.pnv2EmptyObject(),output=list(format="person_period",columns=list("days","copy"),number_columns=list(copy=leaf("days")),options=list(sources=list(list(event="lab")),bins=list(width=10L),window=list(start=0L,end=20L,anchor="index"),analyses=list("mean"))))
  got<-.ws7_scenario_run(.number_backends[[1L]]$handle,p)$frame
  expect_equal(got$copy,got$days)
  expect_false("lab_mean" %in% names(got))
  expect_true(all(c("person_id","cohort_row_id") %in% names(got)))
})

test_that("N28 temporal and coverage reducers retain their canonical identity before presentation", {
  vars<-list()
  for(fn in c("days_since","days_until","time_since","gap_max","gap_mean"))vars[[fn]]<-list(source=list(event="lab"),reducer=list(fn=fn))
  for(fn in c("duration_sum","covered_days","supply_days","drug_duration"))vars[[fn]]<-list(source=list(event="drug"),reducer=list(fn=fn))
  for(fn in c("era_count","era_days","first_era_days"))vars[[fn]]<-list(source=list(interval="course"),reducer=list(fn=fn))
  p<-.ws7_scenario_plan(vars);p$intervals<-list(course=list(source=list(event="drug"),gap=0L))
  for(backend in unique(c(names(.number_backends)[1L],intersect("postgres",names(.number_backends))))) {
    h<-.number_backends[[backend]]$handle
    canonical<-.ws7_scenario_run(h,p)$frame
    identity<-p;identity$options<-list(number_spec=list(pipeline=list(list(kind="transform",method="identity"))))
    got<-.ws7_scenario_run(h,identity)$frame
    for(n in names(vars))expect_identical(got[[n]],canonical[[n]],info=paste(backend,n))
    binned<-p;binned$options<-list(number_spec=list(representation="bins",breaks=list("-Inf",0,7,14,"Inf")))
    bands<-.ws7_scenario_run(h,binned)$frame
    for(n in names(vars)) {
      expect_true(is.ordered(bands[[n]]),info=n)
      expect_equal(as.character(bands[[n]]),as.character(.numberPresent(canonical[[n]],binned$options$number_spec)),info=n)
    }
  }
})

test_that("N36 mapped source mean precedes log, exact rounding and bins", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn),add=TRUE)
  .ws7_extend(h$conn);h$blueprint<-NULL;h$role_registry<-NULL
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE person_id = 7001 AND measurement_id NOT IN (970001,970003)")
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number = CASE measurement_id WHEN 970001 THEN 5 ELSE 15 END WHERE person_id = 7001")
  s<-list(unit=list(kind="event_unit",spec=list(target=8840L,convert=list(`8554`=list(factor=2,offset=10)))),transform=list(list(method="log")),rounding=list(method="decimals",digits=2))
  p<-.ws7_scenario_plan(list(logged=.number_svar("mean",s),band=.number_svar("mean",c(s,list(representation="bins",breaks=list(3,4,5))))))
  got<-.ws7_scenario_run(h,p)$frame
  expect_equal(.ws7_value(got,7001,"logged"),3.4)
  expect_identical(as.character(.ws7_value(got,7001,"band")),"[3,4)")
})

test_that("N59 explicit scalar broadcast aligns to existing event rows", {
  scalar<-.number_svar("mean");scalar$broadcast<-TRUE
  rows<-list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
  req<-list(version=1L,assign=list(list(name="centered",expr=list(op="-",args=list(list(col="x"),list(col="m"))))))
  v<-list(derived=list(kind="number_derive",inputs=list(list(alias="x",variable=rows,stage="canonical"),list(alias="m",variable=scalar,stage="canonical")),exprs=req,unit="coordinate:centered"))
  p<-.ws7_scenario_plan(list(centered=v),output=list(format="long",sources=list(list(event="lab")),columns=list("centered"),options=list(columns_derived=list("value"))))
  got<-.ws7_scenario_run(.number_backends[[1L]]$handle,p)$frame
  expected<-ave(got$value,got$person_id,FUN=function(x)x-mean(x,na.rm=TRUE))
  expect_equal(got$centered,expected)
  p$variables$centered$derived$inputs[[2]]$variable$broadcast<-FALSE
  expect_error(.ws7_scenario_run(.number_backends[[1L]]$handle,p),"explicit broadcast")
})
