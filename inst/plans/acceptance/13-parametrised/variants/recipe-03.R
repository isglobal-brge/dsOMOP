# Parametrised variant of acceptance recipe 03 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../03-distinct-days/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 03 - two diagnoses on distinct days (design 10.7 item 3)
dm <- omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm")
w365 <- omop_window(-365, 0)
rec <- omop_recipe(
  events = list(dm),
  populations = list(
    omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end"))),
    omop_population("dm2", parent_id = "c1", filters = list(omop_count_is(dm, ">=", 2, window = w365, distinct = "day")))),
  variables = list(dm_days = omop_variable(name = "dm_days", event = dm, reducer = omop_reduce("count", window = w365, distinct = "day")),
                   dm_rows = omop_variable(name = "dm_rows", event = dm, reducer = omop_reduce("count", window = w365))),
  outputs = list(omop_output("two_days", rows = "dm2", format = "wide", grain = "episode")))
bound <- recipe_bind(rec, dx = omop_concept_set(201820, descendants = TRUE))
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
