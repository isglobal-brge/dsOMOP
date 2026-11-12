# Parametrised variant of acceptance recipe 09 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../09-recurrent-events/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 09 - recurrent events with very few outcomes, complete = TRUE (design 10.7 item 9)
resp <- omop_event("condition_occurrence", 320136, descendants = TRUE, name = "resp")
death <- omop_event("death", name = "death")
rec <- omop_recipe(
  events = list(resp, death),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("recurrent", rows = "c1", format = "survival", options = list(
    outcomes = list(resp = resp), tar = omop_window(1, omop_param("tar_end", type = "integer")), censor = list("observation_end", "death"),
    format = "recurrent_events", event_order = "all", complete = TRUE))))
bound <- recipe_bind(rec, tar_end = 90L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
