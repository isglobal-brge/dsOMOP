test_that("Q27 same-date interviews join answers by recorded visit FK", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_extend(h$conn);.ws7_keys(list(h))
  api<-new.env(parent=asNamespace("dsOMOPClient"));sys.source(system.file("examples","audit-va-interviews.R",package="dsOMOPClient"),api)
  people<-7001:7008
  visits<-data.frame(visit_occurrence_id=990001:990016,person_id=rep(people,each=2),visit_concept_id=990100L,
    visit_start_date="2020-01-10",visit_end_date="2020-01-10",visit_type_concept_id=0L)
  DBI::dbWriteTable(h$conn,"visit_occurrence",visits,append=TRUE)
  # Old interview: Q1=yes, Q2 absent from this form; new interview: Q1=no,
  # Q2=no. Both are on the same date, so a date join cannot identify a form.
  observations<-do.call(rbind,lapply(seq_len(nrow(visits)),function(i) {
    old<-i%%2==1
    data.frame(observation_id=995000L+i*10L+seq_len(if(old)2 else 3),person_id=visits$person_id[i],
      observation_concept_id=if(old)c(990101L,990103L) else c(990101L,990103L,990102L),
      observation_date="2020-01-10",observation_type_concept_id=0L,
      value_as_concept_id=if(old)c(990112L,990121L) else c(990111L,990122L,990111L),
      visit_occurrence_id=visits$visit_occurrence_id[i])
  }))
  DBI::dbWriteTable(h$conn,"observation",observations,append=TRUE);h$blueprint<-NULL;h$role_registry<-NULL
  r<-api$audit_va_interviews(dsOMOPClient::omop_event("visit_occurrence",990100L),990101L,990102L,990103L,
    c("990111"="No","990112"="Yes","990113"="Don't know","990114"="Refused","990115"="Not asked"),
    c("990121"="Form 1","990122"="Form 2"))
  p<-dsOMOPClient::recipe_to_plan(r,version=2)
  for(mode in c("memory","staged"))local({
    run<-.ws7_ctx(h,p,mode=mode);on.exit(.ws7_close(run))
    got<-.ws7_publish(run,NULL)
    expect_identical(got$status$status,"ok")
    f<-got$values$interviews$interviews
    expect_equal(nrow(f),16L)
    # Inspect only the private synthetic compiled relation; publication uses
    # the same roster/lineage and gate. This is not an analyst row endpoint.
    raw<-got$compiled$interviews$components$interviews$frame
    one<-as.character(raw$answer_one);two<-as.character(raw$answer_two);form<-as.character(raw$form_revision)
    old<-form=="990121";new<-form=="990122"
    expect_equal(sum(old),8L);expect_equal(sum(new),8L)
    expect_true(all(one[old]=="990112"));expect_true(all(is.na(two[old])))
    expect_true(all(one[new]=="990111"&two[new]=="990111"))
  })
})
