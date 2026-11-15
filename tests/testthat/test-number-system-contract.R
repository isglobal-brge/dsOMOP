test_that("N42 N43 public numeric domains survive the actual encoded reconciliation protocol", {
  f<-data.frame(person_id=paste0("p",1:8),x=c(0,1,2,3,4,5,6,7))
  columns<-list(person_id=.fragmentColumn("text",FALSE,"person_key"),x=.fragmentColumn("numeric",TRUE,"measure"))
  out<-.numberOutputColumns(f,columns,list(format="wide"),"o","o",list(representation="bins",breaks=list("-Inf",0,5,10,"Inf")))
  f<-.numberCapture(out$frame,out$columns);class(f)<-c("omop.table","data.frame")
  report<-.conceptLevelReport(f)
  spec<-dsOMOPClient:::.conceptUnionReports(list(a=report,b=report))
  decoded<-.ds_arg(.conceptEncode(spec))
  got<-.conceptRelevel(f,decoded)
  expect_true(is.ordered(got$x));expect_identical(levels(got$x),levels(f$x))
  expect_equal(as.character(got$x),as.character(f$x))
})
test_that("N37 N45 N53 canonical effective specs inherit defaults and retain reset identity", {
  p<-list(version=2L,meta=list(language_version="2.0",semantic_version="2.0.0"),populations=list(p=list(kind="criteria")),variables=list(n=list(derived=list(kind="n_observation_periods"),number_spec=.pnv2EmptyObject())),outputs=list(o=list(rows="p",format="wide")),options=list(number_spec=list(rounding=list(method="floor"))))
  n<-.planNormalizeV2(p)
  expect_identical(n$variables$n$number_spec$pipeline[[1]]$method,"floor")
  expect_equal(.planNormalizeV2(n),n)
  p$variables$n$number_spec<-list(pipeline=list(list(kind="transform",method="identity")))
  expect_identical(.planNormalizeV2(p)$variables$n$number_spec$pipeline[[1]]$method,"identity")
})

test_that("N38 N47 N55 N59 N74 output binding refuses structural keys and propagates unavailability", {
  c<-list(person_id=.fragmentColumn("integer",FALSE,"person_key"),event=.fragmentColumn("integer",FALSE,"measure"),x=.numberGenerated(.fragmentColumn("numeric",TRUE,"measure"),"1"))
  attr(c$x,"number_origin")$available<-FALSE
  leaf<-function(n)list(derived=list(kind="number_column",column=n))
  req<-list(version=1L,assign=list(list(name="filled",expr=list(coalesce=list(list(col="x"),list(lit=0))))))
  derive<-list(derived=list(kind="number_derive",unit="1",exprs=req,inputs=list(list(alias="x",variable=leaf("x"),stage="canonical"))))
  f<-data.frame(person_id=1L,event=1L,x=NA_real_)
  out<-.numberOutputColumns(f,c,list(format="survival",number_columns=list(filled=derive)),"o","o")
  expect_true(is.na(out$frame$filled))
  for(n in c("person_id","event","unknown"))expect_error(.numberOutputColumns(f,c,list(format="survival",number_columns=list(copy=leaf(n))),"o","o"),"original numeric")
  expect_error(.numberOutputColumns(f,c,list(format="wide",number_columns=list(x=leaf("x"))),"o","o"),"unique")
  expect_error(.numberPresent(as.Date("2020-01-01"),list()),"numeric analytic")
  expect_error(.numberPresent(factor("3"),list()),"numeric analytic")
  expect_error(.numberSpecCheck(list(representation="bins",breaks=c("1","2"))),"numeric")
})

test_that("N39 N65 numeric matrices preserve their implicit zero convention", {
  frame<-data.frame(covariate_value=c(1,2))
  columns<-list(covariate_value=.numberGenerated(.fragmentColumn("numeric",TRUE,"measure"),"count"))
  for(format in c("sparse","temporal_covariates")) {
    node<-list(format=format)
    for(method in c("exp","log"))expect_error(.numberOutputColumns(frame,columns,node,"o","o",list(transform=list(method=method))),"implicit matrix zeros.*dense")
    got<-.numberOutputColumns(frame,columns,node,"o","o",list(transform=list(method="affine",factor=2)))
    expect_equal(got$frame$covariate_value,c(2,4))
  }
})

test_that("N04 N39 N65 mixed matrix coordinates apply matching fixed-unit defaults independently", {
  c<-.numberGenerated(.fragmentColumn("numeric",TRUE,"measure"),"mixed_coordinates")
  meta<-attr(c,"number_origin");meta$coordinates<-list(list(covariate_id=1L,analysis="count",unit="count"),list(covariate_id=2L,analysis="exposed_days",unit="days"));attr(c,"number_origin")<-meta
  frame<-data.frame(covariate_id=c(1L,2L),covariate_value=c(2,14))
  columns<-list(covariate_id=.fragmentColumn("integer",FALSE,"concept"),covariate_value=c)
  spec<-list(unit=list(kind="fixed_unit",from="days",to="weeks",factor=1/7))
  out<-.numberOutputColumns(frame,columns,list(format="temporal_covariates"),"o","o",spec)
  expect_equal(out$frame$covariate_value,c(2,2))
  contract<-attr(out$columns$covariate_value,"number_contract")
  expect_identical(vapply(contract$coordinates,function(x)x$number$unit,""),c("count","weeks"))
  expect_identical(contract$privacy_class,2L)
})
