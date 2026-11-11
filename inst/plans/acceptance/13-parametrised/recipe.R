# Acceptance recipe 13 - recipe 03 parametrised with omop_param() and bound on the client (design 10.7 item 13)
library(dsOMOPClient)
w365 <- omop_window(-365, 0)
rec <- omop_recipe(
  events = list(omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm")),
  populations = list(
    omop_population("c1", index = omop_index(list(cohort_definition_id = omop_param("cohort", type = "integer", default = 1)),
                                             select = "all", end = omop_end("event_end"))),
    omop_population("dm2", parent_id = "c1",
                    filters = list(omop_count_is(omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm"),
                                                 ">=", omop_param("min_days", type = "integer", default = 2), window = w365, distinct = "day")))),
  variables = list(
    dm_days = omop_variable(name = "dm_days", event = omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm"),
                            reducer = omop_reduce("count", window = w365, distinct = "day")),
    dm_rows = omop_variable(name = "dm_rows", event = omop_event("condition_occurrence", omop_param("dx", type = "concept_set"), name = "dm"),
                            reducer = omop_reduce("count", window = w365))),
  outputs = list(omop_output("two_days", rows = "dm2", format = "wide", grain = "episode")))
stopifnot(setequal(names(recipe_params(rec)), c("dx", "cohort", "min_days")))
bound <- recipe_bind(rec, dx = omop_concept_set(201820, descendants = TRUE), cohort = 1L, min_days = 2L)
plan <- recipe_to_plan(bound)   # identical to recipe 03's plan.json after normalisation
