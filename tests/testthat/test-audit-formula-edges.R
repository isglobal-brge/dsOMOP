.audit_egfr_plan <- function() {
  api<-function(n)getExportedValue("dsOMOPClient",n)
  e<-api("omop_event")("measurement",.WS7$LAB,name="lab",unit=api("omop_unit")(8554L))
  raw<-api("omop_variable")(event=e,reducer=api("omop_reduce")("first_value",missing="keep"))
  scr<-api("omop_number")(raw,api("omop_number_spec")(unit=api("omop_number_unit")("unit:8554","coordinate:idms_creatinine_mg_dl",1)))
  age<-api("omop_age_at")(api("omop_anchor")(e,"first",scope="episode"))
  sex<-api("omop_variable")(table="person",column="gender_concept_id")
  v<-api("omop_formula_egfr_ckdepi2021")(scr,age,sex,list(female="concept:8532",male="concept:8507"))
  compiled<-api("recipe_to_plan")(api("omop_recipe")(variables=list(egfr=v)),version=2)
  p<-.ws7_scenario_plan(compiled$variables);p$events<-compiled$events;p
}
test_that("Q14 adult boundaries and invalid input eligibility execute in the emitted plan", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_extend(h$conn)
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE person_id BETWEEN 7001 AND 7008")
  values<-c(.7,.9,1.4,1.8,0,-1,NA,.9)
  ages<-c(18L,80L,17L,40L,40L,40L,40L,40L)
  sex<-c(8532L,8507L,8532L,0L,8507L,8507L,8507L,NA_integer_)
  DBI::dbWriteTable(h$conn,"measurement",data.frame(measurement_id=980001:980008,person_id=7001:7008,
    measurement_concept_id=.WS7$LAB,measurement_date="2020-01-01",measurement_type_concept_id=0L,
    value_as_number=values,unit_concept_id=8554L),append=TRUE)
  for(i in seq_along(ages))DBI::dbExecute(h$conn,"UPDATE person SET year_of_birth=?,month_of_birth=1,day_of_birth=1,gender_concept_id=? WHERE person_id=?",params=list(2020L-ages[i],sex[i],7000L+i))
  h$blueprint<-NULL;h$role_registry<-NULL
  p<-.audit_egfr_plan()
  result<-.ws7_scenario_run(h,p);got<-result$frame
  # Independent power equation, not the builder's log/exp DAG.
  expected<-c(142*.9938^18*1.012,142*.9938^80,rep(NA_real_,6))
  expect_equal(got$egfr[match(7001:7008,got$pid)],expected,tolerance=1e-10)
  .ws7_keys(list(h))
  local({
    run<-.ws7_ctx(h,p);on.exit(.ws7_close(run))
    pop<-.ws7_population(run,"t",.ws7_scenario_frame())
    release<-.ws7_publish(run,list(t=pop))
    expect_identical(release$status$status,"ok")
    expect_identical(attr(release$values$o$o,"omop_numbers")$egfr$privacy_class,2L)
  })
  # An absent source stays unavailable, even if all other operands are valid.
  p$events$lab$on_missing_table<-"unavailable"
  DBI::dbExecute(h$conn,"DROP TABLE measurement");h$blueprint<-NULL;h$role_registry<-NULL
  missing<-.ws7_scenario_run(h,p)$frame
  expect_true(all(is.na(missing$egfr)))
})
