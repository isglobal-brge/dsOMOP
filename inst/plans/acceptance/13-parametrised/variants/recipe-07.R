# Parametrised variant of acceptance recipe 07 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../07-monthly-panel/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 07 - numeric monthly dense panel with carry-forward (design 10.7 item 7)
weight <- omop_event("measurement", 3025315, value = "value_as_number",
                     unit = omop_unit(9529, convert = list("8739" = c(factor = 0.45359237, offset = 0))), name = "weight")
vitals <- omop_event("measurement", c(3025315, 3004249), value = "value_as_number", name = "vitals")   # two concept identities
bmi <- omop_event("measurement", 3038553, value = "value_as_number", name = "bmi")                   # no row at this site
rec <- omop_recipe(
  events = list(weight, vitals, bmi),
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("panel", rows = "c1", format = "person_period", options = list(
    representation = "dense", sources = list(weight, vitals, bmi), bins = list(calendar = "month"),
    window = omop_window(-92, 59), analyses = c("last", "count"),
    carry_forward = list(weight = list(lookback = omop_param("lookback", type = "integer"), expire = NULL), bmi = list(lookback = omop_param("lookback", type = "integer"), expire = NULL))))))
bound <- recipe_bind(rec, lookback = 180L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
