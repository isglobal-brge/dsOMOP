# Registered integer columns can arrive from DBI as lossless decimal text.
# Alter only that private transport representation, retaining real catalogue
# validation, event selection, roster joins and output compilation.
test_that("D34 integer component sources accept lossless bigint transport", {
  backends<-.ws7_scenario_backends()
  on.exit(.ws4_cleanup(backends),add=TRUE)
  original<-.reduceStreamRows
  testthat::local_mocked_bindings(.reduceStreamRows=function(...) {
    out<-original(...)
    if("unit_concept_id" %in% names(out$rows)) out$rows$unit_concept_id<-as.character(out$rows$unit_concept_id)
    out
  })
  src<-list(kind="date_components",year=list(kind="column",name="unit_concept_id"),month=list(kind="integer",value=1L),day=list(kind="integer",value=1L),event=list(event="lab"))
  p<-.ws7_scenario_plan(list(d=list(derived=list(kind="calendar_date",reference=src))))
  for(b in names(backends)) {
    f<-.ws7_scenario_run(backends[[b]]$handle,p)$frame
    expect_identical(.ws7_value(f,7001,"d"),as.Date("8554-01-01"),info=b)
    expect_true(is.na(.ws7_value(f,7002,"d")),info=b)
  }
})
