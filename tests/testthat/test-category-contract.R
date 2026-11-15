test_that("C32 C33 C34 C35 public points use exact calendar targets and lossless ties", {
  skip_if_not_installed("dsOMOPClient")
  for(unit in c("months","years")) {
    point<-dsOMOPClient::omop_timepoint(if(unit=="months")12L else 1L,unit)
    expected<-if(unit=="months")"2025-02-28" else "2025-03-01"
    expect_equal(.repeatedTarget(as.Date("2024-02-29"),point),as.Date(expected))
  }
  anchor<-as.Date("2024-08-31")
  points<-dsOMOPClient::omop_timepoints(list(baseline=dsOMOPClient::omop_timepoint(),m6=dsOMOPClient::omop_timepoint(6,"months",c(-7,7)),m12=dsOMOPClient::omop_timepoint(12,"months")),anchor="2024-08-31")
  expect_equal(vapply(points$points,function(p)as.character(.repeatedTarget(anchor,p)),""),c(baseline="2024-08-31",m6="2025-02-28",m12="2025-08-31"))
  rows<-data.frame(start_date=as.Date(c("2020-01-02","2020-01-01","2020-01-01")),source_ns="a",event_id=c("3","10","2"))
  expect_identical(.repeatedSelect(rows,as.Date("2020-01-01"),"closest"),3L)
  expect_identical(.repeatedSelect(rows,as.Date("2020-01-01"),"first"),3L)
  expect_identical(.repeatedSelect(rows,as.Date("2020-01-01"),"last"),1L)
  expect_true(is.na(.repeatedTarget(as.Date("9999-12-31"),dsOMOPClient::omop_timepoint(1,"days"))))
})

test_that("C49 resolved category identities and order cannot be tampered", {
  skip_if_not_installed("dsOMOPClient")
  spec<-dsOMOPClient::omop_category_spec(levels=c("concept:2","concept:10"),ordered=FALSE,reference="concept:10")
  contract<-list(spec=unclass(spec),producer=list(kind="concept"),producer_order=list(),producer_ordered=FALSE)
  records<-setNames(lapply(c("concept:2","concept:10"),function(k)list(kind="category",id=k,label=k,origin="producer",category=contract)),c("concept:2","concept:10"))
  shared<-.categoryResolveRecords(unname(records))
  expect_silent(.categoryValidateResolution(shared,records))
  serialized<-.ds_arg(.conceptEncode(shared))
  expect_silent(.categoryValidateResolution(serialized,records))
  for(field in c("aliases","keys","ordered","reference")) {
    bad<-shared
    if(field=="aliases")bad$aliases[[1]]<-"concept:10"
    if(field=="keys")bad$keys<-rev(bad$keys)
    if(field=="ordered")bad$ordered<-TRUE
    if(field=="reference")bad$reference<-"concept:2"
    expect_error(.categoryValidateResolution(bad,records),"category_domain_incompatible")
  }
})

test_that("C28 C38 private definedness survives repeated row alignment", {
  x<-.categoryValue(c(NA_real_,1,NA_real_),c(FALSE,TRUE,TRUE))
  twice<-x[c(3,1,2)][c(2,3)]
  expect_identical(attr(twice,"category_defined"),c(FALSE,TRUE))
  expect_s3_class(twice,"dsomop_category_value")
})

test_that("R5 copy eligibility uses selector lineage, never text or factor values", {
  ctx <- new.env(parent=emptyenv())
  plain <- .fragmentColumn("text",TRUE,"measure")
  input_only <- plain
  attr(input_only,"category_input_contract") <- list(variable=list())
  # Counts and indicators also have category_input_contract: it is not a result type.
  for (column in list(plain,input_only,.fragmentColumn("integer",TRUE,"measure"))) {
    for (value in list("label",factor("label"))) {
      node <- list(category_columns=list(copy=list(derived=list(kind="category_column",column="answer"))))
      expect_error(.categoryOutputColumns(ctx,data.frame(answer=value),list(answer=column),node,"w","w"),
        "category_type_unsupported")
    }
  }
})
