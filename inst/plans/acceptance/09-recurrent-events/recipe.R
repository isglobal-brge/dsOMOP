# Acceptance recipe 09 - recurrent events with very few outcomes, complete = TRUE (design 10.7 item 9)
library(dsOMOPClient)
resp <- omop_event("condition_occurrence", 320136, descendants = TRUE, name = "resp")
death <- omop_event("death", name = "death")
rec <- omop_recipe(
  events = list(resp, death),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("recurrent", rows = "c1", format = "survival", options = list(
    outcomes = list(resp = resp), tar = omop_window(1, 90), censor = list("observation_end", "death"),
    format = "recurrent_events", event_order = "all", complete = TRUE))))
plan <- recipe_to_plan(rec)
