# Acceptance recipe 11 - incidence denominators: calendar years with observed days (design 10.7 item 11)
library(dsOMOPClient)
resp <- omop_event("condition_occurrence", 320136, descendants = TRUE, name = "resp")
rec <- omop_recipe(
  events = list(resp),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("years", rows = "c1", format = "person_period", options = list(
    representation = "dense", bins = list(calendar = "year"), window = omop_window(0, 730),
    outcomes = list(resp = resp), analyses = "count"))))
plan <- recipe_to_plan(rec)
