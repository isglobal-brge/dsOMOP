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
  untranslated <- .conceptPresent(ctx, raw, cols)
  expect_true(is.factor(untranslated$renamed))
  expect_identical(as.character(untranslated$renamed), as.character(raw$renamed))
  expect_identical(levels(untranslated$renamed), c("0", "45000123", "45000124", "45000125"))
})

test_that("A27 public labels need no observed identity discovery", {
  h <- .cl_backends$sqlite$handle
  ctx <- list(handle = h, ir = list(options = list(labels = list('1' = 'yes', '2' = 'yes')), variables = list()))
  df <- .conceptPresent(ctx, data.frame(person_id = paste0('p', 1:4), answer = c(1,1,1,2)), list(answer = .fragmentColumn('integer', TRUE, 'concept')))
  withr::local_options(list(nfilter.tab = 5, nfilter.levels.max = 1, nfilter.levels.density = 0.1))
  expect_error(omopAsFactorColumnsDS(df,list()), 'concept presentation protocol')
  report <- omopFactorLevelsDS(df)
  expect_length(report$unsafe, 0)
  expect_identical(report$levels$answer, c('1','2'))
  spec <- list(concept_protocol = 1L, columns = list(answer = list(ids = c('1','2'), labels = c('yes','yes'), levels = 'yes')))
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
  # Naming a generated covariate's concept is a level release: no per-concept
  # person minimum since 2.10.6, the DataSHIELD level rule still gates it.
  expect_length(.conceptLevelReport(value)$unsafe,0)
  withr::local_options(list(nfilter.levels.density=0.1))
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

test_that('concept discovery uses output rows including NA and ignores per-level support', {
  ctx <- list(handle=.cl_backends$sqlite$handle, ir=list(options=list(),variables=list()))
  raw <- data.frame(person_id=rep('one',10),cause=c(45000123L,45000124L,rep(NA_integer_,8)))
  frame <- .conceptPresent(ctx,raw,list(cause=.fragmentColumn('integer',TRUE,'concept')))
  withr::local_options(list(nfilter.tab=10,nfilter.levels.max=2,nfilter.levels.density=0.2))
  expect_length(.conceptLevelReport(frame)$unsafe,0)
  withr::local_options(list(nfilter.levels.max=1))
  expect_identical(.conceptLevelReport(frame)$unsafe_rules$cause,'nfilter.levels.max')
  withr::local_options(list(nfilter.levels.max=2,nfilter.levels.density=0.1))
  expect_identical(.conceptLevelReport(frame)$unsafe_rules$cause,'nfilter.levels.density')
  # Once this site's own observed levels pass, assignment accepts a public
  # complete union larger than its cap (no level-count rule on the union).
  withr::local_options(list(nfilter.levels.max=2,nfilter.levels.density=0.2))
  shared <- .conceptResolve(unname(attr(frame,'omop_concepts')$cause$records))
  shared$ids <- c(shared$ids,'45000125')
  shared$labels <- c(shared$labels,'45000125'); shared$levels <- unique(shared$labels)
  expect_identical(levels(.conceptRelevel(frame,list(concept_protocol=1L,columns=list(cause=shared)))$cause),shared$levels)
})

test_that('declared category domains and complete recodes reveal no local subset', {
  skip_if_not_installed('dsOMOPClient')
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(),variables=list()))
  column <- .fragmentColumn('integer',TRUE,'concept')
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(levels=c('concept:1','concept:2','concept:3')))
  present <- function(ids,col=column) .conceptPresent(ctx,data.frame(person_id=seq_along(ids),cause=ids),list(cause=col))
  withr::local_options(list(nfilter.tab=10,nfilter.levels.max=1,nfilter.levels.density=0.1))
  first <- .conceptLevelReport(present(c(1L,1L)))
  second <- .conceptLevelReport(present(c(2L,3L)))
  expect_length(first$unsafe,0); expect_identical(first,second)
  recode <- dsOMOPClient::omop_category_recode(c('concept:1'='category:a','concept:2'='category:a'),levels='category:a',labels='Grouped',unmatched='missing')
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(recode=recode))
  frame <- present(c(1L,9999L),column)
  report <- .conceptLevelReport(frame)
  expect_length(report$unsafe,0)
  expect_setequal(report$levels$cause,c('concept:1','concept:2','category:a'))
  expect_false(grepl('9999',.jcsCanonical(report),fixed=TRUE))
  expect_identical(as.character(frame$cause),c('Grouped',NA_character_))
  expect_identical(report,.conceptLevelReport(present(c(2L,8888L),column)))
  shared <- .categoryResolveRecords(report$concepts$cause,identity=report$categories$cause)
  expect_identical(as.character(.conceptRelevel(frame,list(concept_protocol=1L,columns=list(cause=shared)))$cause),c('Grouped',NA_character_))
})

test_that('staged discovery uses the same row denominator and public declaration rule', {
  testthat::local_mocked_bindings(.conceptStageData=function(descriptor) .ds_arg(descriptor$metadata$concept_presentation))
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(),variables=list()))
  frame <- .conceptPresent(ctx,data.frame(person_id=rep('same',10),cause=c(45000123L,45000124L,rep(NA_integer_,8))),list(cause=.fragmentColumn('integer',TRUE,'concept')))
  descriptor <- .conceptStageCapture(frame,list(metadata=list()))
  withr::local_options(list(nfilter.tab=10,nfilter.levels.max=2,nfilter.levels.density=0.2))
  expect_identical(.conceptStageReport(descriptor)$levels,.conceptLevelReport(frame)$levels)
  expect_length(.conceptStageReport(descriptor)$unsafe,0)
  withr::local_options(list(nfilter.levels.density=0.1))
  expect_identical(.conceptStageReport(descriptor)$unsafe_rules$cause,'nfilter.levels.density')
  ctx$ir$options$labels <- list('45000123'='First','45000124'='Second')
  frame <- .conceptPresent(ctx,data.frame(person_id=1L,cause=45000123L),list(cause=.fragmentColumn('integer',TRUE,'concept')))
  descriptor <- .conceptStageCapture(frame,list(metadata=list()))
  expect_length(.conceptStageReport(descriptor)$unsafe,0)
  expect_identical(.conceptStageReport(descriptor)$levels$cause,c('45000123','45000124'))
})

test_that('unmapped kept categories and local hierarchy expansion require discovery', {
  skip_if_not_installed('dsOMOPClient')
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(),variables=list()))
  column <- .fragmentColumn('integer',TRUE,'concept')
  recode <- dsOMOPClient::omop_category_recode(c('concept:1'='category:a'),levels='category:a',labels='Grouped',unmatched='keep')
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(recode=recode))
  frame <- .conceptPresent(ctx,data.frame(person_id=1:2,cause=c(1L,2L)),list(cause=column))
  withr::local_options(list(nfilter.tab=10,nfilter.levels.max=1,nfilter.levels.density=0.1))
  expect_identical(.conceptLevelReport(frame)$unsafe_rules$cause,'nfilter.levels.density')
  local_mapping <- list('concept:1'='concept:90','concept:2'='concept:91')
  testthat::local_mocked_bindings(.categoryRollupMap=function(handle,spec)list(adapter=.CATEGORY_ADAPTER,
    mapping=local_mapping,digest='private-test-hierarchy'))
  rollup <- dsOMOPClient::omop_category_rollup('snomed',separation=1L,domain=c('concept:1','concept:2'))
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(rollup=rollup))
  frame <- .conceptPresent(ctx,data.frame(person_id=1:2,cause=c(1L,2L)),list(cause=column))
  report <- .conceptLevelReport(frame)
  # A declared rollup domain and the targets its public hierarchy maps it to
  # are declared levels (2.10.6): nothing here is discovered from the rows.
  expect_length(report$unsafe,0)
  expect_true(length(report$categories) > 0)
  # A complete explicitly public hierarchy level requires no discovery.
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(rollup=rollup,levels=c('concept:90','concept:91')))
  public <- .conceptPresent(ctx,data.frame(person_id=1:2,cause=c(1L,2L)),list(cause=column))
  expect_length(.conceptLevelReport(public)$unsafe,0)
  # A locally unused member of the public hierarchy level still travels to all sites.
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(rollup=rollup,levels=c('concept:90','concept:91','concept:92')))
  unused <- .conceptPresent(ctx,data.frame(person_id=1:2,cause=c(1L,2L)),list(cause=column))
  expect_true('concept:92' %in% .conceptLevelReport(unused)$levels$cause)
  expect_length(.conceptLevelReport(unused)$unsafe,0)
  # Descendant expansion remains private when every possible output target is
  # written explicitly. No local source mapping or digest enters the report.
  rollup$domain <- list(concepts=1L,include_descendants=TRUE)
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(rollup=rollup,levels=c('concept:90','concept:91')))
  public_expansion <- .conceptPresent(ctx,data.frame(person_id=1:2,cause=c(1L,2L)),list(cause=column))
  report <- .conceptLevelReport(public_expansion)
  expect_length(report$unsafe,0)
  expect_setequal(report$levels$cause,c('concept:90','concept:91'))
  expect_null(report$categories$cause$rollup$mapping)
  expect_null(report$categories$cause$rollup$digest)
  expect_false(grepl('concept:2"',.jcsCanonical(report),fixed=TRUE))
  expect_identical(attr(public_expansion,'omop_concepts')$cause$ids,c('concept:90','concept:91'))
  local_mapping <- list('concept:1'='concept:90','concept:3'='concept:91')
  other_expansion <- .conceptPresent(ctx,data.frame(person_id=1:2,cause=c(1L,3L)),list(cause=column))
  expect_identical(.conceptLevelReport(other_expansion),report)
  wrapper <- .fragmentColumn('text',TRUE,'measure')
  attr(wrapper,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(ordered=FALSE))
  wrapped <- .categoryPresent(ctx,frame,list(cause=wrapper))
  # A nested presentation inherits the declared rollup domain: still nothing discovered.
  expect_length(.conceptLevelReport(wrapped)$unsafe,0)
})

test_that('category reassignment covers actual rows without disclosing unused private records', {
  skip_if_not_installed('dsOMOPClient')
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(),variables=list()))
  column <- .fragmentColumn('integer',TRUE,'concept')
  attr(column,'category_spec') <- unclass(dsOMOPClient::omop_category_spec(order=dsOMOPClient::omop_category_order(metadata='identity')))
  frame <- .conceptPresent(ctx,data.frame(person_id=1:4,cause=c(1L,1L,2L,2L)),list(cause=column))
  frame <- frame[1:2,]
  attr(frame,'omop_concepts')$cause$ids <- rep('concept:1',2)
  withr::local_options(list(nfilter.levels.max=1,nfilter.levels.density=1))
  report <- .conceptLevelReport(frame)
  expect_identical(report$levels$cause,'concept:1')
  shared <- .categoryResolveRecords(report$concepts$cause,identity=report$categories$cause)
  out <- .conceptRelevel(frame,list(concept_protocol=1L,columns=list(cause=shared)))
  expect_identical(levels(out$cause),'1')
  expect_identical(as.character(out$cause),c('1','1'))
})

test_that('untranslated dictionary factors preserve the separate vocabulary name column', {
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(translate_concepts=FALSE),variables=list()))
  raw <- data.frame(concept_id=c(45000123L,45000124L),concept_name=c('Original name one','Original name two'))
  frame <- .conceptPresent(ctx,raw,list(concept_id=.fragmentColumn('integer',TRUE,'concept')))
  expect_identical(as.character(frame$concept_id),as.character(raw$concept_id))
  expect_identical(frame$concept_name,raw$concept_name)
  shared <- .conceptResolve(unname(attr(frame,'omop_concepts')$concept_id$records))
  withr::local_options(list(nfilter.levels.density=1))
  expect_identical(.conceptRelevel(frame,list(concept_protocol=1L,columns=list(concept_id=shared)))$concept_name,raw$concept_name)
  ctx$ir$options$translate_concepts <- TRUE
  translated <- .conceptPresent(ctx,raw,list(concept_id=.fragmentColumn('integer',TRUE,'concept')))
  expect_identical(translated$concept_name,as.character(translated$concept_id))
})

test_that('hierarchy-derived binary schemas release declarations without local maps', {
  skip_if_not_installed('dsOMOPClient')
  ctx <- list(handle=.cl_backends$sqlite$handle,ir=list(options=list(),variables=list()))
  rollup <- dsOMOPClient::omop_category_rollup('snomed',separation=1L,
    domain=list(concepts=1L,include_descendants=TRUE))
  input <- list(category_spec=unclass(dsOMOPClient::omop_category_spec(rollup=rollup)))
  # The vocabulary map of the declared domain is identified by its digest only,
  # so sites mapping it differently refuse; its expansion is never listed.
  testthat::local_mocked_bindings(.categoryRollupMap=function(...)
    list(adapter='A',mapping=list(`concept:2`='concept:90'),digest='map-digest'))
  binary_report <- function(input) {
    column <- .fragmentColumn('integer',TRUE,'measure')
    attr(column,'semantic_flag') <- TRUE
    attr(column,'category_input_contract') <- .categoryInputContract(ctx,input)
    frame <- data.frame(person_id=1:2,flag=c(0L,1L))
    attr(frame,'omop_codebook_schema') <- .categoryReleaseSchema(ctx,frame,list(flag=column))
    .conceptLevelReport(frame)
  }
  report <- binary_report(input)
  declaration <- report$schema$columns[[2]]$derivation$category_input
  expect_identical(declaration,list(variable=input,maps=list(input='map-digest')))
  expect_false(grepl('concept:2"',.jcsCanonical(report),fixed=TRUE))
  expect_length(report$unsafe,0)
  # With every target declared, not even the digest is released.
  input$category_spec$levels <- list('concept:90')
  declaration <- binary_report(input)$schema$columns[[2]]$derivation$category_input
  expect_identical(declaration,list(variable=input))
})

test_that('generated-column refusals identify their unchanged admission policy', {
  testthat::local_mocked_bindings(.conceptStageData=function(descriptor) .ds_arg(descriptor$metadata$concept_presentation))
  frame <- data.frame(feature=1L)
  attr(frame,'omop_generated_columns') <- list(feature=list(declared=FALSE,permitted=FALSE))
  report <- .conceptLevelReport(frame)
  # Generated keys embed concept IDs: refusals name the class, never the key.
  expect_identical(report$unsafe,'generated concept columns')
  expect_identical(report$unsafe_rules[['generated concept columns']],'generated-column admission')
  expect_false('feature' %in% report$keys)
  staged <- .conceptStageReport(.conceptStageCapture(frame,list(metadata=list())))
  expect_identical(staged$unsafe_rules,report$unsafe_rules)
})
