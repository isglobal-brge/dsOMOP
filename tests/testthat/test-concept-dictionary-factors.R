test_that("legacy dictionaries retain declared and observed IDs in every label and storage mode", {
  handle <- create_test_handle()
  on.exit(cleanup_handle(handle),add=TRUE)
  .ws7_keys(list(handle))
  withr::local_options(list(nfilter.subset=3,nfilter.tab=10,nfilter.levels.max=1,nfilter.levels.density=1))
  for(translate in c(FALSE,TRUE))for(mode in c("memory","staged")) {
    plan <- list(cohort=list(type="cohort_table",cohort_definition_id=1),outputs=list(
      events=list(type="event_level",table="condition_occurrence",
        columns=c("condition_concept_id","condition_type_concept_id"),
        concept_set=201820L,representation=list(format="long"),filters=list(concept_set=list(ids=201820L))),
      dictionary=list(type="concept_dictionary",source_outputs="events")),
      options=list(translate_concepts=translate,block_sensitive=TRUE))
    result <- .planExecute(handle,plan,list(events="dictionary_events",dictionary="dictionary_ids"),output_mode=mode)
    dictionary <- result$dictionary
    if(inherits(dictionary,"FlowerDatasetDescriptor")) {
      report <- .conceptStageReport(dictionary)
      chunks <- list();.publishReadChunks(dictionary,function(x)chunks[[length(chunks)+1L]] <<- x)
      dictionary <- do.call(rbind,chunks)
    } else report <- .conceptLevelReport(dictionary)
    expect_true(is.factor(dictionary$concept_id))
    expect_setequal(report$levels$concept_id,c("201820","44818518"))
    expect_length(report$unsafe,0)
    expect_identical(dictionary$concept_name,c("Diabetes mellitus","EHR"))
    expect_identical(as.character(dictionary$concept_id),if(translate)dictionary$concept_name else c("201820","44818518"))
    expect_true(all(dictionary$used_in_outputs=="events"))
  }
})

test_that("dictionary collection uses factor identity metadata and composite reference IDs", {
  handle <- create_test_handle();on.exit(cleanup_handle(handle),add=TRUE)
  ctx <- list(handle=handle,ir=list(options=list(labels=list("201820"="group","44818518"="group")),variables=list()))
  source <- .conceptPresent(ctx,data.frame(alias=c(201820L,44818518L)),list(alias=.fragmentColumn("integer",TRUE,"concept")))
  output <- list(type="event_level",table="condition_occurrence",columns=c(alias="condition_concept_id"))
  expect_setequal(.dictionaryObservedIds(handle,source,output,"events"),c("201820","44818518"))
  composite <- list(covariateRef=data.frame(conceptId=c(201820L,44818518L)))
  # Real covariate references carry the persons behind them (the rule's n).
  attr(composite$covariateRef,"omop_reference_support") <- list(counts=c(`201820`=5L,`44818518`=5L),total=50L)
  expect_setequal(.dictionaryObservedIds(handle,composite,output,"events"),c("201820","44818518"))
})
