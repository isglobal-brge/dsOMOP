test_that("N28 N47 N62 N73 temporal numeric declarations retain class 2 through expressions", {
  expect_identical(.numberDeclaredUnit(list(),list(derived=list(kind="age"))),"years")
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn),add=TRUE)
  .ws7_extend(h$conn);h$blueprint<-NULL;h$role_registry<-NULL
  vars<-list()
  for(fn in c("days_since","days_until","time_since","gap_max","gap_mean"))vars[[fn]]<-list(source=list(event="lab"),reducer=list(fn=fn))
  for(fn in c("duration_sum","covered_days","supply_days","drug_duration"))vars[[fn]]<-list(source=list(event="drug"),reducer=list(fn=fn))
  vars$observed<-list(derived=list(kind="observed_days",window=list(start=-9L,end=0L,anchor="index")))
  vars$rate<-list(derived=list(kind="number_derive",inputs=list(list(alias="d",variable=vars$observed,stage="canonical")),exprs=list(version=1L,assign=list(list(name="y",expr=list(op="/",args=list(list(lit=1),list(col="d")))))),unit="coordinate:inverse_days"))
  p<-.ws7_scenario_plan(vars)
  got<-.ws7_scenario_run(h,p)
  columns<-got$compiled$components$o$fragment$columns
  for(n in names(vars))expect_identical(attr(columns[[n]],"number_contract")$privacy_class,2L,info=n)
})

test_that("N49 native parsing and staged numeric capsules preserve adjacent binary64 constants", {
  constants<-list(factor=1/7,breaks=as.list(c(1.25-2^-52,1.25,1.25+2^-52)))
  plan<-list(version=2L,options=list(number_spec=list(transform=list(list(method="affine",factor=1/7)))))
  expect_identical(.irJsonForm(plan)$options$number_spec$transform[[1]]$factor,1/7)
  decoded<-.ds_arg(.conceptEncode(list(numbers=list(x=constants))))
  expect_identical(decoded$numbers$x$factor,1/7)
  expect_identical(unlist(decoded$numbers$x$breaks),unlist(constants$breaks))
})
