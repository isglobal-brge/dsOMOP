review_handle <- function() {
  h <- .ws4_reach_handle()
  .ws7_extend(h$conn)
  h$blueprint <- NULL
  h$role_registry <- NULL
  h
}
review_expr <- function(expr) list(version=1L,assign=list(list(name="y",expr=expr)))
review_rows <- function() list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
review_derive <- function(input, reducer=NULL, expr=list(col="x"), stage="canonical") {
  d <- list(kind="number_derive",inputs=list(list(alias="x",variable=input,stage=stage)),exprs=review_expr(expr),unit="coordinate:test")
  if(!is.null(reducer)) d$reducer <- reducer
  list(derived=d)
}

test_that("review scalar formula cannot silently broadcast into long rows", {
  h <- review_handle(); on.exit(DBI::dbDisconnect(h$conn))
  scalar <- list(source=list(event="lab"),reducer=list(fn="mean"))
  formula <- review_derive(scalar)
  p <- .ws7_scenario_plan(list(formula=formula),output=list(format="long",sources=list(list(event="lab")),columns=list("formula"),options=list(columns_derived=list("value"))))
  expect_error(.ws7_scenario_run(h,p), "broadcast")
})

test_that("review expression slope retains temporal class 2", {
  h <- review_handle(); on.exit(DBI::dbDisconnect(h$conn))
  p <- .ws7_scenario_plan(list(slope=review_derive(review_rows(),list(fn="slope"))))
  got <- .ws7_scenario_run(h,p)
  contract <- attr(got$compiled$components$o$fragment$columns$slope,"number_contract")
  expect_identical(contract$privacy_class,2L)
})

test_that("review generated relation days retain day units and class 2", {
  h <- review_handle(); on.exit(DBI::dbDisconnect(h$conn))
  p <- .ws7_scenario_plan(.pnv2EmptyObject(),output=list(format="long",sources=list(list(event="lab")),options=list(relations=list(nextlab=list(source=list(event="lab"),within=list(0L,30L))))))
  p$options <- list(number_spec=list(pipeline=list(list(kind="transform",method="identity"))))
  got <- .ws7_scenario_run(h,p)
  contract <- attr(got$compiled$components$o$fragment$columns$days_to_nextlab,"number_contract")
  expect_identical(contract$unit,"days")
  expect_identical(contract$privacy_class,2L)
})

test_that("review integer64 endpoint changes survive scalar materialization", {
  rows <- data.frame(start_date=as.Date("2020-01-01")+0:1,v=bit64::as.integer64(c("10","20")))
  d <- list(method="difference",missing="skip")
  # This is the same vapply(numeric(1)) boundary used by .numberEventRecord.
  actual <- vapply(list(rows),function(r).numberChangeValues(r,d),numeric(1))
  cat("\nBIGINT endpoint difference: ",format(actual,digits=17,scientific=TRUE),"\n")
  expect_equal(actual,10,tolerance=0)
})

test_that("review presented integer inputs have their presented expression type", {
  h <- review_handle(); on.exit(DBI::dbDisconnect(h$conn))
  input <- list(derived=list(kind="number_count",source=list(event="lab"),value="value",count_kind="nonmissing"),number_spec=list(transform=list(list(method="affine",factor=.5))))
  p <- .ws7_scenario_plan(list(copy=review_derive(input,stage="presented")))
  got <- .ws7_scenario_run(h,p)
  expect_type(got$frame$copy,"double")
  expect_identical(attr(got$compiled$components$o$fragment$columns$copy,"number_contract")$input_type,"numeric")
})

test_that("review supply role has day coordinate and permits day conversion", {
  h <- review_handle(); on.exit(DBI::dbDisconnect(h$conn))
  input <- list(derived=list(kind="number_rows",source=list(event="drug"),value="days_supply"),number_spec=list(unit=list(kind="fixed_unit",from="days",to="weeks",factor=1/7)))
  p <- .ws7_scenario_plan(list(supply=input),output=list(format="long",sources=list(list(event="drug")),columns=list("supply")))
  expect_no_error(.ws7_scenario_run(h,p))
})

test_that("review variable-local source maps preserve long event row binding", {
  h <- review_handle(); on.exit(DBI::dbDisconnect(h$conn))
  input <- review_rows()
  input$number_spec <- list(unit=list(kind="event_unit",spec=list(target=8840L,convert=list(`8554`=list(factor=2,offset=0)))))
  p <- .ws7_scenario_plan(list(mapped=input),output=list(format="long",sources=list(list(event="lab")),columns=list("mapped"),options=list(columns_derived=list("value"))))
  got <- .ws7_scenario_run(h,p)
  # Every nonmissing fixture value has unit 8554; target-map rows must align.
  at <- !is.na(got$frame$value)
  expect_gt(sum(at),0L)
  expect_equal(got$frame$mapped[at],2*got$frame$value[at])
})

test_that("review presented BIGINT expression input retains its fractional value", {
  h <- .ws4_reach_handle(); on.exit(DBI::dbDisconnect(h$conn))
  .ws7_extend(h$conn); h$blueprint <- NULL; h$role_registry <- NULL
  derive <- function(v,expr,stage="canonical") list(derived=list(kind="number_derive",inputs=list(list(alias="x",variable=v,stage=stage)),exprs=list(version=1L,assign=list(list(name="y",expr=expr))),unit="coordinate:test"))
  scalar <- list(source=list(event="lab"),reducer=list(fn="mean"))
  inner <- derive(scalar,list(lit=1099511627776,as="integer"))
  inner$number_spec <- list(transform=list(list(method="affine",factor=1,offset=.5)))
  outer <- derive(inner,list(col="x"),"presented")
  got <- .ws7_scenario_run(h,.ws7_scenario_plan(list(fraction=outer)))
  cat("\nPresented result: ",paste(as.character(got$frame$fraction),collapse=", "),"; class ",class(got$frame$fraction),"\n")
  expect_type(got$frame$fraction,"double")
  expect_false(inherits(got$frame$fraction,"integer64"))
  expect_equal(as.numeric(got$frame$fraction),rep(1099511627776.5,8L),tolerance=0)
})

test_that("normalization siblings project through checked origins in long and intervals", {
  h<-review_handle();on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"UPDATE measurement SET unit_concept_id = 999999 WHERE measurement_id = 970004")
  mapped<-function(factor,unknown="missing") {
    v<-review_rows();v$number_spec<-list(unit=list(kind="event_unit",spec=list(target=8840L,convert=list(`8554`=list(factor=factor,offset=0)),unknown=unknown)));v
  }
  for(format in c("long","intervals"))for(predicate in c(FALSE,TRUE)) {
    vars<-list(raw=review_rows(),twice=mapped(2),triple=mapped(3),half=mapped(.5),excluded=mapped(2,"exclude"))
    p<-.ws7_scenario_plan(vars,output=list(format=format,sources=list(list(event="lab")),columns=as.list(names(vars))))
    if(format=="intervals") {p$outputs$o$options<-list(sources=p$outputs$o$sources);p$outputs$o$sources<-NULL}
    if(predicate)p$events$lab$where<-list(var="value",op=">",value=4)
    got<-.ws7_scenario_run(h,p)$frame
    expect_gt(nrow(got),0)
    expect_true(any(!is.na(got$twice)))
    # The unknown-unit row is raw 3 and becomes missing under both policies.
    expected<-got$raw;expected[which(expected==3&got$pid==7001)]<-NA_real_
    expect_equal(got$twice,2*expected)
    expect_equal(got$triple,3*expected)
    expect_equal(got$excluded,2*expected)
    half<-.5*expected;if(predicate)half[which(half<=4)]<-NA_real_
    expect_equal(got$half,half)
  }
  # Distinct declarations of the same table must not cross-bind event IDs.
  p<-.ws7_scenario_plan(list(mapped=mapped(2)),output=list(format="long",sources=list(list(event="other")),columns=list("mapped")))
  p$events$other<-p$events$lab
  expect_true(all(is.na(.ws7_scenario_run(h,p)$frame$mapped)))
  mixed<-review_derive(review_rows())
  mixed$derived$inputs[[2]]<-list(alias="z",variable=mapped(2),stage="canonical")
  p<-.ws7_scenario_plan(list(mixed=mixed),output=list(format="long",sources=list(list(event="lab")),columns=list("mixed")))
  expect_error(.ws7_scenario_run(h,p),"identical source/event identity")
  rows<-data.frame(row_key="a",event_id="1",source_ns="lab")
  expect_error(.numberMatchEventRows(list(ir=list(events=list())),rows,rbind(rows,rows)),"one-to-one")
})

test_that("BIGINT longitudinal methods check arithmetic and retain exact threshold comparisons", {
  rows<-data.frame(start_date=as.Date("2020-01-01")+0:1,v=bit64::as.integer64(c("10","20")))
  expected<-c(difference=10,percent_change=100,ratio=2,slope=10)
  for(method in names(expected)) {
    d<-list(method=method,missing="skip")
    expect_equal(vapply(list(rows),function(r).numberChangeValues(r,d),numeric(1)),unname(expected[method]),tolerance=0)
    large<-rows;large$v<-bit64::as.integer64(c("9007199254740993","9007199254740995"))
    expect_error(.numberChangeValues(large,d),"retain raw/count")
    # Exactly representable BIGINT operands above 2^53 remain supported.
    exact<-rows;exact$v<-bit64::as.integer64(c("9007199254740992","9007199254740994"))
    expect_true(is.finite(.numberChangeValues(exact,d)))
    overflow<-rows;overflow$v<-bit64::as.integer64(c("-9223372036854775807","9223372036854775807"))
    expect_error(.numberChangeValues(overflow,d),"retain raw/count")
  }
  expect_equal(.numberChangeValues(large,list(method="time_to_threshold",threshold=9007199254740994,op="ge",reference="first",missing="skip")),1L)
  expect_equal(.reduceOne("slope",list(start_date=rows$start_date,v=rows$v),list(),value_type="bigint"),10)
  expect_error(.reduceOne("slope",list(start_date=large$start_date,v=large$v),list(),value_type="bigint"),"retain raw/count")
  expect_error(.numberMaterialize(1099511627776.5,"bigint"),"declared integer type")
  expect_error(.numberMaterialize(.5,"integer"),"declared integer type")
})

test_that("compiled scalar grain requires broadcast through nested expressions", {
  h<-review_handle();on.exit(DBI::dbDisconnect(h$conn))
  scalar<-list(source=list(event="lab"),reducer=list(fn="mean"))
  for(v in list(scalar,review_derive(scalar),review_derive(review_derive(scalar)))) {
    p<-.ws7_scenario_plan(list(x=v),output=list(format="long",sources=list(list(event="lab")),columns=list("x")))
    expect_error(.ws7_scenario_run(h,p),"broadcast")
    p$variables$x$broadcast<-TRUE
    got<-.ws7_scenario_run(h,p)$frame
    expect_gt(sum(!is.na(got$x)),0L)
    expect_true(all(vapply(split(got$x,got$person_id),function(x)length(unique(x))==1L,logical(1))))
  }
  for(v in list(review_rows(),review_derive(review_rows()),review_derive(review_derive(review_rows())))) {
    p<-.ws7_scenario_plan(list(x=v),output=list(format="long",sources=list(list(event="lab")),columns=list("x"),options=list(columns_derived=list("value"))))
    got<-.ws7_scenario_run(h,p)$frame
    expect_equal(got$x,got$value)
  }
})

test_that("operand prefix types reach nested expressions and reducers", {
  h<-review_handle();on.exit(DBI::dbDisconnect(h$conn))
  base<-review_derive(review_rows(),expr=list(lit=1099511627776,as="integer"))
  for(prefix in c("affine","fixed"))for(stage in c("canonical","presented")) {
    inner<-base
    inner$number_spec<-if(prefix=="affine")list(transform=list(list(method="affine",factor=1,offset=.5))) else
      list(unit=list(kind="fixed_unit",from="coordinate:test",to="coordinate:shifted",factor=1,offset=.5))
    for(reducer in list(NULL,list(fn="first_value"),list(fn="mean"))) {
      v<-review_derive(review_derive(inner,stage=stage),reducer=reducer)
      output<-if(is.null(reducer))list(format="long",sources=list(list(event="lab")),columns=list("x")) else list()
      got<-.ws7_scenario_run(h,.ws7_scenario_plan(list(x=v),output=output))
      transformed<-prefix=="fixed"||stage=="presented"
      expect_equal(as.numeric(got$frame$x[!is.na(got$frame$x)]),rep(1099511627776+if(transformed).5 else 0,sum(!is.na(got$frame$x))),tolerance=0)
      contract<-attr(got$compiled$components$o$fragment$columns$x,"number_contract")
      expect_identical(contract$input_type,if(transformed||identical(reducer$fn,"mean"))"numeric" else "bigint")
    }
  }
})

test_that("selected roles keep supply and unrelated coordinates independent of measured maps", {
  h<-review_handle();on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure ADD COLUMN quantity REAL")
  DBI::dbExecute(h$conn,"UPDATE drug_exposure SET days_supply = 14, quantity = 3")
  leaf<-function(value)list(derived=list(kind="number_rows",source=list(event="drug"),value=value))
  days<-leaf("days_supply");weeks<-days
  weeks$number_spec<-list(unit=list(kind="fixed_unit",from="days",to="weeks",factor=1/7))
  p<-.ws7_scenario_plan(list(days=days,weeks=weeks,qty=leaf("quantity")),output=list(format="long",sources=list(list(event="drug")),columns=list("days","weeks","qty")))
  got<-.ws7_scenario_run(h,p)
  expect_true(all(c("days","weeks","qty") %in% names(got$frame)))
  expect_gt(nrow(got$frame),0L)
  expect_true(all(got$frame$days==14));expect_true(all(got$frame$weeks==2));expect_true(all(got$frame$qty==3))
  cs<-got$compiled$components$o$fragment$columns
  expect_identical(attr(cs$days,"number_contract")$unit,"days")
  expect_identical(attr(cs$weeks,"number_contract")$unit,"weeks")
  expect_identical(attr(cs$qty,"number_contract")$unit,"1")
  other<-review_rows();other$derived$value<-"unit_unknown"
  p<-.ws7_scenario_plan(list(other=other),output=list(format="long",sources=list(list(event="lab")),columns=list("other")))
  p$events$lab$unit<-list(target=8840L,convert=list(`8554`=list(factor=2,offset=0)))
  got<-.ws7_scenario_run(h,p)
  expect_identical(attr(got$compiled$components$o$fragment$columns$other,"number_contract")$unit,"1")
  expect_true("other" %in% names(got$frame))
  expect_true(all(got$frame$other %in% 0:1))
  p$variables$other$number_spec<-list(unit=list(kind="fixed_unit",from="unit:8840",to="coordinate:x",factor=2))
  expect_error(.ws7_scenario_run(h,p),"matching declared units")
})

test_that("temporal reducers and relation copies keep class 2 through publication transport", {
  skip_if_not_installed("arrow")
  h<-review_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_keys(list(h))
  slope<-review_derive(review_rows(),list(fn="slope"))
  copy<-function(n)list(derived=list(kind="number_column",column=n))
  band<-function(v) {v$number_spec<-list(representation="bins",breaks=list("-Inf",0,7,"Inf"));v}
  wide<-.ws7_scenario_plan(list(slope=slope,nested=review_derive(review_derive(slope)),band=band(slope)),output=list(number_columns=list(copy=copy("slope"),nested_copy=review_derive(review_derive(copy("slope"))))))
  long<-.ws7_scenario_plan(.pnv2EmptyObject(),output=list(format="long",sources=list(list(event="lab")),options=list(relations=list(nextlab=list(source=list(event="lab"),within=list(0L,30L)))),number_columns=list(copy=copy("days_to_nextlab"),nested=review_derive(review_derive(copy("days_to_nextlab"))),band=band(copy("days_to_nextlab")))))
  wide$outputs$o$columns<-list("slope","nested","band","copy","nested_copy")
  long$options<-list(number_spec=list(pipeline=list(list(kind="transform",method="identity"))))
  for(p in list(wide,long))for(mode in c("memory","staged"))local({
    run<-.ws7_ctx(h,p,mode=mode);on.exit(.ws7_close(run),add=TRUE)
    pop<-.ws7_population(run,"t",.ws7_scenario_frame())
    got<-.ws7_publish(run,list(t=pop),threshold=5L)
    expect_identical(got$status$status,"ok")
    f<-got$values$o$o;contracts<-attr(f,"omop_numbers")
    expected<-if(p$outputs$o$format=="wide")c("slope","nested","band","copy","nested_copy") else c("days_to_nextlab","copy","nested","band")
    expect_true(all(expected %in% names(f)))
    for(n in expected)expect_equal(contracts[[n]]$privacy_class,2L,info=paste(mode,n))
    expect_true(is.ordered(f$band))
    expect_equal(contracts[[if(p$outputs$o$format=="wide")"slope" else "days_to_nextlab"]]$unit,if(p$outputs$o$format=="wide")"coordinate:test/days" else "days")
  })
})

test_that("BIGINT source changes survive compiler scalar materialization", {
  h<-review_handle();on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure RENAME COLUMN refills TO old_refills")
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure ADD COLUMN refills BIGINT")
  DBI::dbExecute(h$conn,"UPDATE drug_exposure SET refills = CASE drug_exposure_id WHEN 970001 THEN 10 WHEN 970002 THEN 20 END")
  expect_identical(.eventColumnType(.buildBlueprint(h)$columns$drug_exposure,"refills"),"bigint")
  vars<-lapply(c("difference","percent_change","ratio","slope"),function(m)
    list(derived=list(kind="number_change",source=list(event="drug"),value="refills",method=m,missing="skip")))
  names(vars)<-c("difference","percent_change","ratio","slope")
  p<-.ws7_scenario_plan(vars)
  got<-.ws7_scenario_run(h,p)$frame
  expected<-c(difference=10,percent_change=100,ratio=2,slope=2.5)
  for(n in names(vars))expect_equal(.ws7_value(got,7001,n),unname(expected[n]),tolerance=0)
  DBI::dbExecute(h$conn,"UPDATE drug_exposure SET refills = CASE drug_exposure_id WHEN 970001 THEN 9007199254740993 WHEN 970002 THEN 9007199254740995 END")
  expect_error(.ws7_scenario_run(h,p),"retain raw/count")
})

test_that("output formula contracts retain exact integer result types", {
  x<-bit64::as.integer64(c("9007199254740993","9007199254740995"))
  leaf<-list(derived=list(kind="number_column",column="x"))
  node<-list(format="wide",number_columns=list(y=review_derive(leaf)))
  column<-.numberGenerated(.fragmentColumn("bigint",TRUE,"measure"),"1")
  got<-.numberOutputColumns(data.frame(x=x),list(x=column),node,"o","o")
  expect_identical(got$frame$y,x)
  expect_identical(attr(got$columns$y,"number_contract")$input_type,"bigint")
  expect_identical(attr(got$columns$y,"number_contract")$result_type,"integer64")
})
