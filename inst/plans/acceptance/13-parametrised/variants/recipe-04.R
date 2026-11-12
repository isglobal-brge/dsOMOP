# Parametrised variant of acceptance recipe 04 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../04-units-censored/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 04 - unit-normalised threshold and nearest value with censored operators (design 10.7 item 4)
conv <- list("8753" = c(factor = 0.0915, offset = 2.15))
hba1c <- omop_event("measurement", 3004410, value = "value_as_number", unit = omop_unit(8554, convert = conv), name = "hba1c")
hba1c_ge65 <- omop_event("measurement", 3004410, value = "value_as_number", unit = omop_unit(8554, convert = conv),
                         where = omop_where(value >= omop_param("threshold", type = "number")), name = "hba1c_ge65")
hba1c_uncensored <- omop_event("measurement", 3004410, value = "value_as_number",
                               unit = omop_unit(8554, convert = conv, censored = "exclude"), name = "hba1c_uncensored")
hba1c_known_unit <- omop_event("measurement", 3004410, value = "value_as_number",
                               unit = omop_unit(8554, convert = conv, unknown = "exclude"), name = "hba1c_known_unit")
w180 <- omop_window(-180, 0)
rec <- omop_recipe(
  events = list(hba1c, hba1c_ge65, hba1c_uncensored, hba1c_known_unit),
  populations = list(
    omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all", end = omop_end("event_end"))),
    omop_population("with_a1c", parent_id = "c1", filters = list(omop_has(hba1c, window = w180)))),
  variables = list(
    a1c_closest = omop_variable(name = "a1c_closest", event = hba1c, reducer = omop_reduce("closest_value", window = w180, side = "before")),
    a1c_n_ge65 = omop_variable(name = "a1c_n_ge65", event = hba1c_ge65, reducer = omop_reduce("count", window = w180)),
    a1c_n_uncensored = omop_variable(name = "a1c_n_uncensored", event = hba1c_uncensored, reducer = omop_reduce("count", window = w180)),
    a1c_n_known_unit = omop_variable(name = "a1c_n_known_unit", event = hba1c_known_unit, reducer = omop_reduce("count", window = w180)),
    a1c_max = omop_variable(name = "a1c_max", event = hba1c, reducer = omop_reduce("max", window = w180))),
  outputs = list(omop_output("labs", rows = "with_a1c", format = "wide", grain = "episode")))
bound <- recipe_bind(rec, threshold = 6.5)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
