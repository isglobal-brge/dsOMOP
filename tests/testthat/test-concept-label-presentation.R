.cl_backends <- .ws7_scenario_backends()
withr::defer(.ws4_cleanup(.cl_backends), testthat::teardown_env())

test_that("A27 aliased education and cause-of-death preserve concept lineage", {
  h <- .cl_backends$sqlite$handle
  .ws7_keys(list(h))
  variables <- list(
    education = list(source = list(event = 'lab'), reducer = list(fn = 'last_value', value = 'value_as_concept_id')),
    count_concept_id = list(source = list(event = 'lab'), reducer = list(fn = 'count')),
    cause = list(raw = list(table = 'death', column = 'cause_concept_id')))
  plan <- .ws7_scenario_plan(variables, output = list(columns = as.list(names(variables))))
  plan$options <- list(labels = list('0' = 'unknown', '45000123' = 'declared only'))
  plan$variables$education$labels <- list('0' = 'education unknown')
  run <- .ws7_ctx(h, plan)
  on.exit(.ws7_close(run), add = TRUE)
  pop <- .ws7_population(run, 't', .ws7_scenario_frame(c(7001, 7003, 7004, 7005, 7006, 7007)))
  result <- .ws7_publish(run, list(t = pop), threshold = 1L)
  expect_identical(result$status$status, 'ok')
  df <- result$values$o$o
  expect_true(is.factor(df$education))
  expect_true(is.factor(df$cause))
  expect_false(is.factor(df$count_concept_id))
  expect_true('education unknown' %in% levels(df$education))
  expect_true('declared only' %in% levels(df$education))
  expect_identical(as.character(df$education[!is.na(df$education)]), rep('education unknown', sum(!is.na(df$education))))
})

test_that("A27 factors preserve missing values, zero fallback and declarations", {
  h <- .cl_backends$sqlite$handle
  ctx <- list(handle = h, ir = list(options = list(labels = list('45000123' = 'yes', '45000124' = 'yes')), variables = list()))
  cols <- list(renamed = .fragmentColumn('integer', TRUE, 'concept'), example_concept_id = .fragmentColumn('numeric'))
  raw <- data.frame(person_id = paste0('p', 1:5), renamed = c(0L, 45000123L, NA, 45000124L, 45000125L), example_concept_id = 1:5)
  df <- .conceptPresent(ctx, raw, cols)
  expect_true(is.factor(df$renamed))
  expect_true(is.na(df$renamed[[3L]]))
  expect_identical(as.character(df$renamed[c(2,4,5)]), c('yes', 'yes', '45000125'))
  expect_identical(df$example_concept_id, 1:5)
  ctx$ir$options$translate_concepts <- FALSE
  expect_identical(.conceptPresent(ctx, raw, cols), raw)
})

test_that("A27 disclosure checks identity before recode merges and fails closed", {
  h <- .cl_backends$sqlite$handle
  ctx <- list(handle = h, ir = list(options = list(labels = list('1' = 'yes', '2' = 'yes')), variables = list()))
  df <- .conceptPresent(ctx, data.frame(person_id = paste0('p', 1:4), answer = c(1,1,1,2)), list(answer = .fragmentColumn('integer', TRUE, 'concept')))
  withr::local_options(list(nfilter.tab = 2, nfilter.levels.max = 40, nfilter.levels.density = 1))
  expect_error(omopAsFactorColumnsDS(df,list()), 'concept presentation protocol')
  report <- omopFactorLevelsDS(df)
  expect_identical(report$unsafe, 'answer')
  expect_length(report$concepts, 0L)
  spec <- list(concept_protocol = 1L, columns = list(answer = list(ids = c('1','2'), labels = c('yes','yes'), levels = 'yes')))
  expect_error(.conceptRelevel(df, spec), 'cannot certify')
  withr::local_options(list(nfilter.tab = 1))
  expect_identical(levels(.conceptRelevel(df, spec)$answer), 'yes')
  spec$columns$answer$ids <- '1'; spec$columns$answer$labels <- 'yes'
  expect_error(.conceptRelevel(df, spec), 'declared user recode')
})

.cl_formats_plan <- function() {
  vars <- list(education = list(source = list(event = 'lab'), reducer = list(fn = 'last_value', value = 'value_as_concept_id')),
               cause = list(raw = list(table = 'death', column = 'cause_concept_id')))
  p <- .ws7_scenario_plan(vars)
  p$outputs$long <- list(rows = 't', format = 'long', sources = list(list(event = 'lab')))
  p$outputs$panel <- list(rows = 't', format = 'temporal_covariates', options = list(
    sources = list(list(event = 'dx')), bin_width = 30L, window = list(start = -30L, end = 30L, anchor = 'index'), analyses = list('count')))
  p$outputs$period <- list(rows = 't', format = 'person_period', options = list(
    sources = list(list(event = 'dx')), bins = list(width = 100L), window = list(start = -30L, end = 30L, anchor = 'index'), analyses = list('count')))
  p$outputs$sparse <- list(rows = 't', format = 'sparse', options = list(sources = list(list(event = 'dx')), analyses = list('binary','count')))
  p$outputs$membership <- list(rows = 't', format = 'membership')
  p$outputs$dictionary <- list(rows = 't', format = 'dictionary')
  p$outputs$survival <- list(rows = 't', format = 'survival', options = list(
    outcomes = list(disease = list(event = 'dx')), censor = list('observation_end'), tar = list(start = 1L,end = 30L,anchor = 'index')))
  p$outputs$joined <- list(rows = 't', format = 'joined', grain = 'episode', options = list(
    from = list(list(output = 'o'),list(output = 'survival')), cardinality = 'one_to_one'))
  p$options <- list(labels = list('0' = 'unknown', '45000123' = 'declared only'))
  p$variables$education$labels <- list('0' = 'education unknown')
  p
}

test_that('A27 all native format components preserve classes and staged factors', {
  h <- .cl_backends$sqlite$handle; .ws7_keys(list(h))
  p <- .cl_formats_plan()
  results <- lapply(c('memory','staged'), function(mode) {
    run <- .ws7_ctx(h,p,mode); on.exit(.ws7_close(run), add = TRUE)
    pop <- .ws7_population(run,'t',.ws7_scenario_frame(c(7001,7003,7004,7005,7006,7007)))
    result <- .ws7_publish(run,list(t=pop),threshold=1L)
    expect_identical(result$status$status,'ok')
    result$values
  })
  for (root in names(results[[1L]])) for (comp in names(results[[1L]][[root]])) {
    mem <- results[[1L]][[root]][[comp]]; stg <- results[[2L]][[root]][[comp]]
    expect_identical(names(mem),names(stg),info=paste(root,comp))
    for (col in names(mem)) {
      expect_identical(as.character(mem[[col]]),as.character(stg[[col]]),info=paste(root,comp,col))
      if(is.factor(mem[[col]])) expect_identical(levels(mem[[col]]),levels(stg[[col]]),info=paste(root,comp,col))
    }
  }
  mem <- results[[1L]]
  expect_true(is.factor(mem$o$o$education))
  expect_true(is.factor(mem$joined$joined$education))
  expect_true(is.factor(mem$joined$joined$cause))
  expect_identical(levels(mem$o$o$education),levels(mem$joined$joined$education))
  expect_true(is.factor(mem$long$long$value_as_concept_id))
  expect_true(is.factor(mem$dictionary$dictionary$concept_id))
  expect_identical(as.character(mem$dictionary$dictionary$concept_id),mem$dictionary$dictionary$concept_name)
  for(root in c('panel','sparse')) {
    expect_true(is.factor(mem[[root]]$covariateRef$concept_id),info=root)
    expect_false(is.factor(mem[[root]][[root]]$covariate_id),info=root)
  }
  expect_false(is.factor(mem$membership$membership$cohort_definition_id))
  expect_true(all(vapply(mem$period$period[grep('_count$',names(mem$period$period))], is.numeric, logical(1))))
  expect_false(any(grepl('9220000',names(mem$period$period))))
})

 test_that('A27 registry separates semantic identity from privacy class', {
  cc <- read.csv(testthat::test_path('..','..','inst','column_classes.csv'))
  expect_true(all(cc$semantic_class[cc$column %in% c('value_as_concept_id','cause_concept_id')] == 'concept'))
  expect_true(all(cc$assignment_class[cc$semantic_class == 'concept'] == 3L))
  expect_false(.conceptDeclaredColumn('example_concept_id',data.frame(column_name='example_concept_id',semantic_class='ordinary')))
})

test_that('A27 effective variable names override recipe maps before component validation', {
  h <- .cl_backends$sqlite$handle
  ctx <- list(handle=h,ir=list(options=list(column_names=list(a_1='same',b_2='same',dormant='person_id')),
    variables=list(a=list(column_names=list(a_1='education')))))
  cols <- list(a_1=.fragmentColumn('numeric'),b_2=.fragmentColumn('numeric'))
  attr(cols$a_1,'concept_name') <- list(id='1',prefix='a_',suffix='',owner='a',declared=TRUE)
  attr(cols$b_2,'concept_name') <- list(id='2',prefix='b_',suffix='',owner='b',declared=TRUE)
  raw <- data.frame(person_id=1:5,a_1=1,b_2=0)
  expect_identical(names(.conceptPresent(ctx,raw,cols)),c('person_id','education','same'))
  ctx$ir$variables$a <- NULL
  expect_error(.conceptPresent(ctx,raw,cols),'explicit name')
  expect_identical(names(.conceptPresent(ctx,raw[c('person_id','a_1')],cols['a_1'])),c('person_id','same'))
  expect_identical(names(.conceptPresent(ctx,raw[c('person_id','b_2')],cols['b_2'])),c('person_id','same'))
  ctx$ir$options$column_names <- list(a_1='person_id')
  expect_error(.conceptPresent(ctx,raw,cols),'explicit name')
})

test_that('A27 protected factor reconciliation verifies and preserves its seal', {
  base <- withr::local_tempdir(); Sys.chmod(base,'0700')
  withr::local_envvar(c(DSOMOP_STATE_DIR=base,DSOMOP_DP_ENABLED=NA_character_,DSOMOP_TEST_ALLOW_EPHEMERAL_STATE='1'))
  withr::local_options(list(dsomop.state_dir=base,dsomop.dp.enabled=TRUE,dsomop.dp.domain='labels-test',
    dsomop.dp.snapshot_id='2026-11-13',dsomop.dp.privacy_epoch=1L,nfilter.tab=1,nfilter.levels.density=1))
  old <- .pkg_state$dp_runtime; .pkg_state$dp_runtime <- NULL
  withr::defer(.pkg_state$dp_runtime <- old)
  suppressMessages(.dsomopDpNoiseRoot())
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(),variables=list()))
  df <- .conceptPresent(ctx,data.frame(person_id=1:6,education=rep(c(45000123L,45000124L),each=3),measure=1:6),
    list(education=.fragmentColumn('integer',TRUE,'concept')))
  df <- .testPseudonymize(df)
  df <- .dsomopDpSealPersonLocal(df,'test/labels',dataset_identity=list(resource_identity='labels',target_dialect='sqlite',cdm_schema='main',vocabulary_schema='main',results_schema='main'))
  expect_silent(.dsomopDpVerifyPersonLocal(df))
  spec <- list(concept_protocol=1L,columns=list(education=list(ids=c('45000123','45000124'),labels=c('education','cause'),levels=c('education','cause'))))
  result <- .conceptRelevel(df,spec)
  expect_silent(.dsomopDpVerifyPersonLocal(result))
  expect_identical(result$person_id,df$person_id)
  expect_identical(result$measure,df$measure)
  expect_identical(levels(result$education),c('education','cause'))
  analysis <- omopAsFactorColumnsDS(df, spec, 'education')
  expect_silent(.dsomopDpVerifyPersonLocal(analysis))
  expect_identical(analysis$person_id, df$person_id)
  expect_identical(analysis$measure, df$measure)
  expect_identical(levels(analysis$education), c('C1','C2'))
  expect_identical(as.integer(analysis$education), as.integer(result$education))
  expect_error(omopAsFactorColumnsDS(df, spec, 'person_id'), 'published concept column')
  expect_error(omopAsFactorColumnsDS(df, spec, c('education','measure')), 'published concept column')
  corrupt <- df; corrupt$measure[[1L]] <- 100
  expect_error(.conceptRelevel(corrupt,spec),'provenance|authentic|changed|integrity')
  expect_error(omopAsFactorColumnsDS(corrupt,spec,'education'),'provenance|authentic|changed|integrity')
})

test_that('A27 absent vocabulary gives exact ID factors, zero and real missingness', {
  original <- .buildBlueprint
  testthat::local_mocked_bindings(.buildBlueprint=function(handle) {
    bp <- original(handle); bp$tables$present_in_db[bp$tables$table_name %in% c('concept','vocabulary')] <- FALSE; bp
  })
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(labels=list('45000123'='recode')),variables=list()))
  df <- .conceptPresent(ctx,data.frame(person_id=1:4,education=c(0L,45000123L,45000124L,NA)),list(education=.fragmentColumn('integer',TRUE,'concept')))
  expect_identical(as.character(df$education),c('0','recode','45000124',NA_character_))
  expect_identical(levels(df$education),c('0','recode','45000124'))
})

test_that('A27 categorical factors keep federation columns in glm model frames', {
  x <- .conceptFactor(c('yes','yes','no','no'), c('no','yes','elsewhere'))
  frame <- data.frame(y=c(1,2,3,4), education=x)
  fit <- suppressWarnings(stats::glm(y ~ education, data=frame, family=stats::gaussian(), x=TRUE))
  expect_identical(colnames(fit$x), c('(Intercept)','educationyes','educationelsewhere'))
  expect_equal(unname(fit$x[,3]), rep(0,4))
  expect_identical(levels(x[1:2,drop=TRUE]), levels(x))
  expect_false(is.ordered(x))
})

test_that('A27 sparse reference names use vocabulary independently of recodes and gate observed IDs', {
  h <- .cl_backends$sqlite$handle
  ctx <- list(handle=h,ir=list(options=list(labels=list('0'='recoded')),variables=list()))
  raw <- data.frame(covariateId=c(1,2), covariateName=c('0_binary','0_count'),analysisId=c(1L,2L),conceptId=c(0L,0L))
  attr(raw,'omop_concept_reference') <- TRUE
  attr(raw,'omop_reference_support') <- list(counts=c('0'=5L),total=5L)
  value <- .conceptPresent(ctx,raw,list(conceptId=.fragmentColumn('integer',TRUE,'concept')))
  expect_true(is.factor(value$conceptId))
  expect_identical(as.character(value$conceptId),rep('recoded',2))
  expect_false(any(grepl('recoded',value$covariateName)))
  expect_true(all(grepl('_(binary|count)$',value$covariateName)))
  withr::local_options(list(nfilter.tab=3,nfilter.levels.max=40,nfilter.levels.density=1))
  expect_length(.conceptLevelReport(value)$unsafe,0)
  attr(value,'omop_reference_support')$counts[] <- 1L
  report <- .conceptLevelReport(value)
  expect_identical(report$unsafe,'conceptId')
  expect_length(report$concepts,0)
})

test_that('A27 declared-only value recodes cannot change sparse feature names', {
  testthat::local_mocked_bindings(.conceptVocabulary=function(handle,ids) setNames(lapply(ids,function(id)
    list(id=id,name='Same',vocabulary='v',version='1',origin='vocabulary',label='Same')),ids))
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(labels=list('2'='declared')),variables=list()))
  frame <- data.frame(covariateId=1001,covariateName='1_binary',analysisId=1L,conceptId=1L)
  value <- .conceptPresent(ctx,frame,list(conceptId=.fragmentColumn('integer',TRUE,'concept')))
  expect_identical(value$covariateName,'same_binary')
  expect_true('declared' %in% levels(value$conceptId))
  expect_identical(attr(value,'omop_concepts')$conceptId$resolved$naming_ids,'1')
})
