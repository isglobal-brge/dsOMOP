test_that("Q13 canonical and alias discovery retain their actual method scope", {
  h<-create_test_handle();on.exit(cleanup_handle(h));suppressWarnings(.omopAnalysisRegistry(h))
  pairs<-list(c("dsomop:plp.performance","dsomop:ohdsi.plp.plp_performances"),
    c("dsomop:sccs.outcome_rate_per_month","dsomop:ohdsi.sccs.sccs_result"),
    c("dsomop:incidence.rate","dsomop:ohdsi.cohort_incidence.incidence_summary"))
  for(pair in pairs) {
    first<-.omopAnalysisGet(h,pair[1])$interoperability
    second<-.omopAnalysisGet(h,pair[2])$interoperability
    expect_identical(first,second)
    expect_identical(first$schema_version,"ohdsi-declaration-1")
    expect_false(first$compatibility=="upstream_equivalent")
    expect_true(.jsonSchemaValidate(.jsonSchemaLoad(system.file("schema","ohdsi-declaration-1.json",package="dsOMOP")),first)$valid)
  }
  expect_identical(.omopAnalysisGet(h,"dsomop:plp.performance")$interoperability$estimation_contract$evaluation,"in_sample/resubstitution")
  expect_match(.omopAnalysisGet(h,"dsomop:cm.effect_estimate")$interoperability$estimation_contract$estimand,"crude arm-only")
  expect_identical(.omopAnalysisGet(h,"dsomop:sccs.outcome_rate_per_month")$interoperability$estimation_contract$estimand,"outcome rate per month")
})
test_that("Q13 ordinary results carry effective settings without observed inventories", {
  withr::local_options(list(dsomop.tools.mode="standard"))
  h<-create_test_handle();on.exit(cleanup_handle(h));suppressWarnings(.omopAnalysisRegistry(h))
  DBI::dbWriteTable(h$conn,"cohort",data.frame(cohort_definition_id=1L,subject_id=1:15,cohort_start_date="2020-01-01",cohort_end_date="2020-12-31"),overwrite=TRUE)
  a<-.omopAnalysisRun(h,"dsomop:fe.prevalence",params=list(time_windows=0L),scope=list(cohort=1L))
  b<-.omopAnalysisRun(h,"dsomop:fe.prevalence",scope=list(cohort=1L))
  expect_identical(attr(a,"interoperability"),attr(b,"interoperability"))
  expect_match(attr(a,"interoperability")$estimation_contract$estimand,"all-history")
  expect_false(any(c("person_id","row_count","selected_dates","noise_fingerprint") %in% names(attr(a,"interoperability"))))
})
