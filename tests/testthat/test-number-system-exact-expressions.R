regression2_handle <- function() {
  h <- .ws4_reach_handle(); .ws7_extend(h$conn)
  h$blueprint <- NULL; h$role_registry <- NULL; h
}
regression2_derive <- function(input, expr, reducer=NULL) {
  d <- list(kind="number_derive",inputs=list(list(alias="x",variable=input,stage="canonical")),
    exprs=list(version=1L,assign=list(list(name="y",expr=expr))),unit="coordinate:test")
  if(!is.null(reducer)) d$reducer <- reducer
  list(derived=d)
}
regression2_rows <- function() list(derived=list(kind="number_rows",source=list(event="lab"),value="value"))
regression2_large_expr <- function() list(op="+",args=list(list(lit=9007199254740992,as="integer"),list(lit=1,as="integer")))

test_that("review 2 integer arithmetic promotion survives a selector", {
  h <- regression2_handle(); on.exit(DBI::dbDisconnect(h$conn))
  v <- regression2_derive(regression2_rows(), regression2_large_expr(), list(fn="first_value"))
  got <- .ws7_scenario_run(h,.ws7_scenario_plan(list(selected=v)))
  value <- .ws7_value(got$frame,7001,"selected")
  cat("\nPromoted integer selector: ",format(value,digits=17),"; class ",class(value),"\n")
  expect_identical(as.character(value),"9007199254740993")
  expect_s3_class(value,"integer64")
})

test_that("review 2 arithmetic reducers refuse inexact promoted integers", {
  h <- regression2_handle(); on.exit(DBI::dbDisconnect(h$conn))
  v <- regression2_derive(regression2_rows(), regression2_large_expr(), list(fn="mean"))
  expect_error({
    got <- .ws7_scenario_run(h,.ws7_scenario_plan(list(average=v)))
    cat("\nPromoted integer mean: ",format(.ws7_value(got$frame,7001,"average"),digits=17),"\n")
  },"retain raw/count")
})

test_that("review 2 numeric expressions check integer intermediate conversions", {
  h <- regression2_handle(); on.exit(DBI::dbDisconnect(h$conn))
  scalar <- list(source=list(event="lab"),reducer=list(fn="mean"))
  expr <- list(op="/",args=list(regression2_large_expr(),list(lit=1,as="integer")))
  v <- regression2_derive(scalar,expr)
  expect_error({
    got <- .ws7_scenario_run(h,.ws7_scenario_plan(list(quotient=v)))
    cat("\nInline integer quotient: ",format(.ws7_value(got$frame,7001,"quotient"),digits=17),"\n")
  },"retain raw/count")
  # Splitting the identical integer intermediate into a prior assignment
  # reaches the guard; placing it inline must have the same admission result.
  split <- v
  split$derived$exprs$assign <- list(list(name="z",expr=regression2_large_expr()),
    list(name="y",expr=list(op="/",args=list(list(col="z"),list(lit=1,as="integer")))))
  expect_error(.ws7_scenario_run(h,.ws7_scenario_plan(list(quotient=split))),"retain raw/count")
})

test_that("review 2 ordinary integer slope survives expression arithmetic", {
  h <- regression2_handle(); on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure RENAME COLUMN refills TO old_refills")
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure ADD COLUMN refills BIGINT")
  DBI::dbExecute(h$conn,"UPDATE drug_exposure SET refills = CASE drug_exposure_id WHEN 970001 THEN 10000000000 WHEN 970002 THEN 20000000000 END")
  rows <- list(derived=list(kind="number_rows",source=list(event="drug"),value="refills"))
  v <- regression2_derive(rows,list(op="+",args=list(list(col="x"),list(lit=0,as="integer"))),list(fn="slope"))
  got <- .ws7_scenario_run(h,.ws7_scenario_plan(list(slope=v)))
  value <- .ws7_value(got$frame,7001,"slope")
  cat("\nInteger expression slope: ",format(value,digits=17),"\n")
  expect_equal(value,2500000000,tolerance=0)
})

test_that("review 2 stored BIGINT selector is exact with and without plus zero", {
  h <- regression2_handle(); on.exit(DBI::dbDisconnect(h$conn))
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure RENAME COLUMN refills TO old_refills")
  DBI::dbExecute(h$conn,"ALTER TABLE drug_exposure ADD COLUMN refills BIGINT")
  DBI::dbExecute(h$conn,"UPDATE drug_exposure SET refills = CASE drug_exposure_id WHEN 970001 THEN 9007199254740993 WHEN 970002 THEN 9007199254740995 END")
  expect_identical(.eventColumnType(.buildBlueprint(h)$columns$drug_exposure,"refills"),"bigint")
  rows <- list(derived=list(kind="number_rows",source=list(event="drug"),value="refills"))
  exact <- regression2_derive(rows,list(col="x"),list(fn="first_value"))
  arithmetic <- regression2_derive(rows,list(op="+",args=list(list(col="x"),list(lit=0,as="integer"))),list(fn="first_value"))
  got <- .ws7_scenario_run(h,.ws7_scenario_plan(list(exact=exact,arithmetic=arithmetic)))
  original <- .ws7_value(got$frame,7001,"exact")
  actual <- .ws7_value(got$frame,7001,"arithmetic")
  cat("\nStored BIGINT first(x): ",as.character(original),"; first(x + integer(0)): ",format(actual,digits=17),"\n")
  expect_identical(as.character(original),"9007199254740993")
  expect_identical(as.character(actual),"9007199254740993")
  expect_s3_class(actual,"integer64")
})

exact_lit <- function(x, type="integer") list(lit=x,as=type)
exact_op <- function(op,a,b) list(op=op,args=list(a,b))
exact_test <- list(var="x",op=">=",value=0)
exact_numeric_boundaries <- function(z) list(
  add=exact_op("+",z,exact_lit(0,"numeric")),
  subtract=exact_op("-",z,exact_lit(0,"numeric")),
  multiply=exact_op("*",z,exact_lit(1,"numeric")),
  divide=exact_op("/",z,exact_lit(1)),
  branch=list(ifelse=list(test=exact_test,yes=z,no=exact_lit(0,"numeric"))),
  cases=list(case_when=list(cases=list(list(when=exact_test,then=z)),`else`=exact_lit(0,"numeric"))),
  coalesce=list(coalesce=list(z,exact_lit(0,"numeric"))))

test_that("inline and named intermediates share admission at every numeric boundary", {
  h<-regression2_handle();on.exit(DBI::dbDisconnect(h$conn))
  scalar<-list(source=list(event="lab"),reducer=list(fn="mean"))
  for(increment in c(1,2))for(named in c(FALSE,TRUE)) {
    inner<-exact_op("+",exact_lit(9007199254740992),exact_lit(increment))
    boundaries<-exact_numeric_boundaries(if(named)list(col="z") else inner)
    for(kind in names(boundaries)) {
      v<-regression2_derive(scalar,boundaries[[kind]])
      if(named)v$derived$exprs$assign<-c(list(list(name="z",expr=inner)),v$derived$exprs$assign)
      p<-.ws7_scenario_plan(list(answer=v))
      if(increment==1)expect_error(.ws7_scenario_run(h,p),"number_type_unsupported.*retain raw/count|retain raw/count",info=paste(named,kind)) else {
        got<-.ws7_scenario_run(h,p)
        expect_identical(.ws7_value(got$frame,7001,"answer"),9007199254740994,info=paste(named,kind))
      }
    }
  }
})

test_that("integer expression domains do not depend on rows or physical narrowing", {
  expressions<-list(exact_lit(1),regression2_large_expr(),
    exact_op("-",regression2_large_expr(),exact_lit(9007199254740992)),
    list(coalesce=list(exact_lit(NULL),regression2_large_expr())),
    list(ifelse=list(test=exact_test,yes=regression2_large_expr(),no=exact_lit(0))),
    list(case_when=list(cases=list(list(when=exact_test,then=regression2_large_expr())),`else`=exact_lit(0))))
  for(expr in expressions)for(x in list(numeric(),c(1,2),c(NA_real_,NA_real_))) {
    req<-list(version=1L,assign=list(list(name="y",expr=expr)))
    expect_identical(.numberExpression(req,data.frame(x=x),type_only=TRUE),"bigint")
    expect_s3_class(.numberExpression(req,data.frame(x=x)),"integer64")
  }
  # Unused integers are not conversion boundaries; do not refuse unrelated work.
  req<-list(version=1L,assign=list(list(name="z",expr=regression2_large_expr()),list(name="y",expr=exact_lit(.5,"numeric"))))
  expect_identical(.numberExpression(req,data.frame(x=1)),.5)
})

test_that("reducers check actual integer64 storage despite a narrower caller type", {
  values<-bit64::as.integer64(c("9007199254740993","9007199254740995"))
  x<-list(v=values,start_date=as.Date(c("2020-01-01","2020-01-02")))
  for(fn in c("first_value","last_value","nth_value","closest_value","min","max","mode")) {
    got<-.reduceOne(fn,x,list(n=1L),anchor=x$start_date[1L],value_type="integer")
    expect_identical(as.character(got),if(fn %in% c("last_value","max"))"9007199254740995" else "9007199254740993")
  }
  for(fn in c("mean","sum","sd","cv","median","quantile","slope"))
    expect_error(.reduceOne(fn,x,list(p=.5),value_type="integer"),"retain raw/count",info=fn)
})

test_that("integer promotion remains exact through nested compiled selectors and transport", {
  skip_if_not_installed("arrow")
  h<-regression2_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_keys(list(h))
  inner<-regression2_derive(regression2_rows(),regression2_large_expr())
  nested<-regression2_derive(inner,exact_op("+",list(col="x"),exact_lit(0)),list(fn="first_value"))
  p<-.ws7_scenario_plan(list(answer=nested))
  for(mode in c("memory","staged"))local({
    run<-.ws7_ctx(h,p,mode=mode);on.exit(.ws7_close(run),add=TRUE)
    pop<-.ws7_population(run,"t",.ws7_scenario_frame())
    got<-.ws7_publish(run,list(t=pop),threshold=5L)
    expect_identical(got$status$status,"ok")
    frame<-got$values$o$o
    expect_s3_class(frame$answer,"integer64")
    expect_identical(unique(as.character(frame$answer[!is.na(frame$answer)])),"9007199254740993")
    expect_identical(attr(frame,"omop_numbers")$answer$input_type,"bigint")
    expect_identical(attr(frame,"omop_numbers")$answer$result_type,"integer64")
  })
})

test_that("output-local formulas retain conservative integer contracts for empty rows", {
  leaf<-list(derived=list(kind="number_column",column="x"))
  node<-list(format="wide",number_columns=list(y=regression2_derive(leaf,exact_op("+",list(col="x"),exact_lit(0)))))
  for(x in list(integer(),1L,bit64::as.integer64("9007199254740993"))) {
    column<-.numberGenerated(.fragmentColumn(if(inherits(x,"integer64"))"bigint" else "integer",TRUE,"measure"),"1")
    got<-.numberOutputColumns(data.frame(x=x),list(x=column),node,"o","o")
    expect_identical(as.character(got$frame$y),as.character(x))
    expect_identical(attr(got$columns$y,"number_contract")$input_type,"bigint")
    expect_identical(attr(got$columns$y,"number_contract")$result_type,"integer64")
  }
})

test_that("native recursive checks preserve the legacy mutate boundary", {
  frame<-data.frame(x=1)
  expr<-exact_numeric_boundaries(regression2_large_expr())$divide
  expect_identical(.mutateEval(expr,frame,c(x="numeric")),9007199254740992)
  expect_error(.numberExpression(list(version=1L,assign=list(list(name="y",expr=expr))),frame),"retain raw/count")
})


test_that("all compiled integer expression selectors preserve exact values", {
  h<-regression2_handle();on.exit(DBI::dbDisconnect(h$conn))
  for(fn in c("first_value","last_value","nth_value","min","max","mode")) {
    r<-list(fn=fn);if(fn=="nth_value")r$n<-1L
    v<-regression2_derive(regression2_rows(),regression2_large_expr(),r)
    got<-.ws7_scenario_run(h,.ws7_scenario_plan(list(answer=v)))
    expect_identical(as.character(.ws7_value(got$frame,7001,"answer")),"9007199254740993",info=fn)
    expect_s3_class(got$frame$answer,"integer64")
  }
})
