.audit_physical_export <- function(version) {
  h<-create_test_handle();dir<-test_path("..","fixtures","ohdsi-audit",paste0("cm-",version))
  for(path in list.files(dir,full.names=TRUE,pattern="[.]csv$")) {
    rows<-read.csv(path,check.names=FALSE,stringsAsFactors=FALSE,na.strings="NA")
    # CSV has no SQL types. Use upstream's documented identifier / scalar
    # types, including all-NA measures, to materialize the physical export.
    for(n in names(rows)) {
      if(n %in% c("database_id","target_estimator","description","definition"))rows[[n]]<-as.character(rows[[n]])
      else if(grepl("_id$",n))rows[[n]]<-as.integer(rows[[n]])
      else rows[[n]]<-as.numeric(rows[[n]])
    }
    DBI::dbWriteTable(h$conn,sub("[.]csv$","",basename(path)),rows,overwrite=TRUE)
  }
  .buildBlueprint(h);h
}
test_that("Q06 pinned actual CM 5.5.2 exports retain the existing protected projection", {
  h<-.audit_physical_export("5.5.2");on.exit(cleanup_handle(h))
  schema<-.ohdsiInspectResultSchema(h,"cm_result")
  expect_identical(schema$status,"supported")
  expect_identical(schema$layout_id,"cm-inline-export-5.5.2-1")
  expect_identical(schema$upstream_version,"unknown")
  expect_identical(schema$verified_export_version,"5.5.2")
  got<-.ohdsiGetResults(h,"cm_result")
  expect_gt(nrow(got),0L)
  expect_true(all(names(got) %in% .ohdsiReviewedContracts()$cohort_method$cm_result$public_columns))
  expect_false(any(c("definition","database_id","calibrated_rr","target_days") %in% names(got)))
  # Public source artifact is untouched; its first eight nonempty contrasts
  # supply independently exported estimates and genuine two-arm support.
  source<-read.csv(test_path("..","fixtures","ohdsi-audit","cm-5.5.2","cm_result.csv"))
  expect_true(all(na.omit(got$rr) %in% source$rr))
  DBI::dbExecute(h$conn,"DELETE FROM cm_analysis WHERE analysis_id=1")
  expect_error(.ohdsiGetResults(h,"cm_result",columns="rr"),"results_schema_integrity.*references")
})
test_that("Q06 reference uniqueness and associations are checked outside projected slices", {
  for(table in c("cm_analysis","cm_target_comparator_outcome"))local({
    h<-.audit_physical_export("5.5.2");on.exit(cleanup_handle(h))
    DBI::dbExecute(h$conn,paste0("INSERT INTO ",table," SELECT * FROM ",table," LIMIT 1"))
    expect_error(.ohdsiGetResults(h,"cm_result",columns="rr"),"results_schema_integrity.*references")
  })
})
test_that("Q06 actual current CM export refuses without result-row access", {
  h<-.audit_physical_export("6.0.3");on.exit(cleanup_handle(h))
  expect_identical(.ohdsiInspectResultSchema(h,"cm_result")$status,"recognized_unsupported")
  local_mocked_bindings(.executeQuery=function(...)stop("PATIENT_OR_RESULT_READ"))
  expect_error(.ohdsiGetResults(h,"cm_result"),"unsupported_results_schema.*cm_target_comparator")
  expect_error(.ohdsiResultPoolingContract(h,"cm_result"),"unsupported_results_schema")
})
test_that("actual upstream export fixtures match pinned immutable archive manifests", {
  manifests<-jsonlite::fromJSON(test_path("..","fixtures","ohdsi-audit","physical-exports.json"),simplifyVector=FALSE)
  for(m in manifests)for(name in names(m$files)) {
    path<-test_path("..","fixtures","ohdsi-audit",paste0("cm-",m$package_version),name)
    expect_identical(paste(openssl::sha256(file(path,"rb"))),m$files[[name]])
  }
})
