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
    if(fmt=="long")out$options<-list(columns_derived=list("value"))
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
