test_that("shared type extraction cannot bypass admitted factor metadata", {
  value <- factor(c("private_a","private_b"))
  attr(value,"omop_category_contract") <- list(keys=c("concept:1","concept:2"),
    levels=levels(value),reference="concept:1",contrasts=list(kind="treatment",reference="concept:1"))
  expect_identical(.presentationDescribe(value,domains=FALSE),list(r_type="factor",storage_type="integer"))
  ctx <- list(ir=list())
  column <- .fragmentColumn("text",TRUE,"measure")
  schema <- .categoryReleaseSchema(ctx,data.frame(x=value),list(x=column))
  expect_false(any(c("levels","reference","aliases","contrasts") %in% names(schema$columns[[1L]])))
  expect_false(grepl("private_|concept:1",.jcsCanonical(schema),fixed=FALSE))
})

test_that("membership producer inventory is independent of patient rows", {
  for(kind in c("index","criteria","setop","match")) {
    expect_identical(.presentationMembershipColumns(list(kind=kind)),
      c(cohort_start_date="Date",cohort_end_date="Date",index_source="character"))
    expect_identical(names(.presentationMembershipColumns(list(kind=kind),list(legacy_cohort_definition_id=NULL))),
      c("cohort_definition_id","cohort_start_date","cohort_end_date","index_source"))
  }
})


test_that("native multi-state identities keep their declared lossless type", {
  keys <- c("9007199254740993","9007199254740994")
  dates <- as.Date(c("2020-01-01","2020-01-02"))
  opts <- list(transitions=list(list(from="index",to="lab")),initial_state="index")
  retained <- list(lab=data.frame(row_key=character(),start_date=as.Date(character()),event_id=integer()))
  make <- function(ids).survivalMultistate(opts,keys,c(11L,22L),dates,dates,dates+10,
    ids,retained,"lab","priority")$msdata
  value <- make(1:2)
  expect_type(value$cohort_row_id,"character")
  expect_setequal(value$cohort_row_id,keys)
  expect_identical(value$row_id,value$cohort_row_id)
  expect_identical(value$person_id,c(11L,22L)[match(value$cohort_row_id,keys)])
  expect_identical(make(integer())$cohort_row_id,character())
})
