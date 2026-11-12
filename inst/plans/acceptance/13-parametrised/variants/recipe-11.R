# Parametrised variant of acceptance recipe 11 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../11-incidence-years/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 11 - incidence denominators: calendar years with observed days (design 10.7 item 11)
resp <- omop_event("condition_occurrence", 320136, descendants = TRUE, name = "resp")
rec <- omop_recipe(
  events = list(resp),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("years", rows = "c1", format = "person_period", options = list(
    representation = "dense", bins = list(calendar = "year"), window = omop_window(0, omop_param("horizon", type = "integer")),
    outcomes = list(resp = resp), analyses = "count"))))
bound <- recipe_bind(rec, horizon = 730L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
