.audit_egfr_plan <- function(factor=1) {
  api<-function(n)getExportedValue("dsOMOPClient",n)
  e<-api("omop_event")("measurement",.WS7$LAB,name="lab",unit=api("omop_unit")(8554L))
  raw<-api("omop_variable")(event=e,reducer=api("omop_reduce")("first_value",missing="keep"))
  scr<-api("omop_number")(raw,api("omop_number_spec")(unit=api("omop_number_unit")("unit:8554","coordinate:idms_creatinine_mg_dl",factor)))
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
  # Independently declared micromol/L conversion: 88.4 equals 1 mg/dL.
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number=1")
  mg<-.ws7_scenario_run(h,.audit_egfr_plan())$frame
  DBI::dbExecute(h$conn,"UPDATE measurement SET value_as_number=88.4")
  micro<-.ws7_scenario_run(h,.audit_egfr_plan(1/88.4))$frame
  expect_equal(micro$egfr,mg$egfr,tolerance=1e-10)
  # An absent source stays unavailable, even if all other operands are valid.
  p$events$lab$on_missing_table<-"unavailable"
  DBI::dbExecute(h$conn,"DROP TABLE measurement");h$blueprint<-NULL;h$role_registry<-NULL
  missing<-.ws7_scenario_run(h,p)$frame
  expect_true(all(is.na(missing$egfr)))
})

test_that("Q14 normalized measurement selection keeps birthday age and adult eligibility aligned", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_extend(h$conn)
  DBI::dbExecute(h$conn,"DELETE FROM measurement WHERE person_id BETWEEN 7001 AND 7003")
  DBI::dbWriteTable(h$conn,"measurement",data.frame(measurement_id=981001:981006,
    person_id=rep(7001:7003,each=2),measurement_concept_id=.WS7$LAB,
    measurement_date=rep(c("2020-01-01","2020-01-03"),3),measurement_type_concept_id=0L,
    value_as_number=.9,unit_concept_id=c(999999L,8554L,999999L,8554L,8554L,999999L)),append=TRUE)
  for(i in 1:3)DBI::dbExecute(h$conn,
    "UPDATE person SET year_of_birth=?,month_of_birth=1,day_of_birth=2,gender_concept_id=8507 WHERE person_id=?",
    params=list(c(1980L,2002L,2002L)[i],7000L+i))
  h$blueprint<-NULL;h$role_registry<-NULL
  api<-function(n)getExportedValue("dsOMOPClient",n)
  e<-api("omop_event")("measurement",.WS7$LAB,name="scr")
  map<-api("omop_unit")(8554L,unknown="exclude")
  conversion<-api("omop_number_unit")("unit:8554","coordinate:idms_creatinine_mg_dl",1)
  sex<-api("omop_variable")(table="person",column="gender_concept_id")
  sex_map<-list(female="concept:8532",male="concept:8507")
  run<-function(variables) {
    built<-api("recipe_to_plan")(api("omop_recipe")(variables=variables),version=2)
    p<-.ws7_scenario_plan(built$variables);p$events<-built$events
    frame<-.ws7_scenario_run(h,p)$frame
    frame[match(7001:7003,frame$pid),,drop=FALSE]
  }
  for(which in c("first","last","nth")) {
    raw<-api("omop_variable")(event=e,reducer=api("omop_reduce")(paste0(which,"_value"),missing="keep"))
    scr<-api("omop_number")(raw,api("omop_number_pipeline")(map,conversion))
    age<-api("omop_age_at")(api("omop_anchor")(e,which,scope="episode"))
    expect_error(api("omop_formula_egfr_ckdepi2021")(scr,age,sex,sex_map),
      "put source-unit normalization on the shared event")
    # Actual compiled inputs demonstrate the fork; the constructor must refuse
    # it even though both original declarations have exactly the same event.
    fork<-run(list(scr=scr,age=age))
    expect_equal(fork$scr,rep(.9,3))
    expect_equal(fork$age,if(which=="last")c(40,18,18) else c(39,17,17))
    shared<-e;shared$unit<-map;raw$event<-shared
    age<-api("omop_age_at")(api("omop_anchor")(shared,which,scope="episode"))
    for(spec in list(api("omop_number_spec")(unit=conversion),api("omop_number_pipeline")(map,conversion))) {
      scr<-api("omop_number")(raw,spec)
      egfr<-api("omop_formula_egfr_ckdepi2021")(scr,age,sex,sex_map)
      aligned<-run(list(scr=scr,age=age,egfr=egfr))
      expect_equal(aligned$scr,rep(.9,3))
      expect_equal(aligned$age,c(40,18,17))
      # Independent power oracle: the adult birthday is included, while a
      # selected measurement before that birthday remains ineligible.
      expect_equal(aligned$egfr,c(142*.9938^40,142*.9938^18,NA_real_),tolerance=1e-10)
    }
  }
})

test_that("Q14 formulas refuse independent event streams instead of aligning by position", {
  h<-.ws4_reach_handle();on.exit(DBI::dbDisconnect(h$conn));.ws7_extend(h$conn)
  h$blueprint<-NULL;h$role_registry<-NULL
  e<-dsOMOPClient::omop_event("measurement",.WS7$LAB,name="weight",unit=dsOMOPClient::omop_unit(8554L))
  other<-e;other$name<-"height"
  rows<-function(event,unit)dsOMOPClient::omop_number_rows(event,
    number_spec=dsOMOPClient::omop_number_spec(unit=dsOMOPClient::omop_number_unit("unit:8554",unit,1)))
  bmi<-dsOMOPClient::omop_formula_bmi(rows(e,"coordinate:kg"),rows(other,"coordinate:metres"))
  built<-dsOMOPClient::recipe_to_plan(dsOMOPClient::omop_recipe(variables=list(bmi=bmi)),version=2)
  p<-.ws7_scenario_plan(built$variables);p$events<-built$events
  expect_error(.ws7_scenario_run(h,p),"number_dependency_invalid")
})
