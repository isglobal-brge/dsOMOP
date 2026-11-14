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
