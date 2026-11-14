test_that("C46 C64 public all-format category recipes compile on retained Synthea backends", {
  .numeric_enabled()
  ns<-new.env(parent=asNamespace("dsOMOPClient"))
  sys.source(testthat::test_path("..","..","..","dsOMOPClient","tests","testthat","helper-category-cdm.R"),ns)
  p<-dsOMOPClient::recipe_to_plan(ns$.category_format_recipe(3000963L),version=2)
  hs<-.numeric_backends();.ws7_keys(hs)
  for(h in hs) {
    got<-.numeric_run(h,p,"memory")
    expect_length(got$values,length(p$outputs))
    for(root in names(got$values))for(component in names(got$values[[root]])) {
      frame<-got$values[[root]][[component]]
      spec<-dsOMOPClient:::.conceptUnionReports(list(site=.conceptLevelReport(frame)))
      message("Relevel Synthea ",root,"/",component)
      expect_silent(omopAsFactorColumnsDS(frame,dsOMOPClient:::.ds_encode(spec)))
    }
  }
})

test_that("C46 C49 GiBleed component contracts relevel without changing identity", {
  path<-Sys.getenv("DSOMOP_TEST_CATEGORY_GIBLEED")
  if(!nzchar(path))skip("opt-in GiBleed fixture")
  ns<-new.env(parent=asNamespace("dsOMOPClient"))
  sys.source(testthat::test_path("..","..","..","dsOMOPClient","tests","testthat","helper-category-cdm.R"),ns)
  p<-dsOMOPClient::recipe_to_plan(ns$.category_format_recipe(3006322L),version=2)
  copy<-withr::local_tempfile(fileext=".sqlite");stopifnot(file.copy(path,copy))
  con<-DBI::dbConnect(RSQLite::SQLite(),copy);withr::defer(DBI::dbDisconnect(con))
  h<-.ws4_handle_on(con,"sqlite","sqlite",NULL);.ws7_keys(list(sqlite=h))
  got<-.numeric_run(h,p,"memory")
  for(root in names(got$values))for(component in names(got$values[[root]])) {
    frame<-got$values[[root]][[component]]
    report<-.conceptLevelReport(frame)
    spec<-dsOMOPClient:::.conceptUnionReports(list(site=report))
    message("Relevel component ",root,"/",component)
    expect_silent(omopAsFactorColumnsDS(frame,dsOMOPClient:::.ds_encode(spec)))
  }
})
