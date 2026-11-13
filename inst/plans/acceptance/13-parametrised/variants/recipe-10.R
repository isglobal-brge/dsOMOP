# Parametrised variant of acceptance recipe 10 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../10-matching/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 10 - nested case-control via omop_match() (design 10.7 item 10; canonical example B)
mi <- omop_event("condition_occurrence", omop_concept_set(4329847, descendants = TRUE, name = "mi"), name = "mi")
statin_rx <- omop_event("drug_exposure", omop_concept_set(1539403, descendants = TRUE, name = "statins"), name = "statin_rx")
diabetes <- omop_event("condition_occurrence", 201820, descendants = TRUE, name = "diabetes")
rec <- omop_recipe(
  events = list(mi, statin_rx, diabetes),
  populations = list(
    omop_population("cases", label = "First MI", index = omop_index(mi, select = "first_eligible", end = omop_end("fixed", days = 0)),
                    filters = list(omop_observed(before = 365))),
    omop_population("eligible", label = "Observed one year", filters = list(omop_observed(before = 365))),
    omop_population("cc", label = "Nested case-control", match = omop_match(
      cases = "cases", controls = "eligible", outcome = mi,
      match = list("sex", omop_age_band(5), "calendar_year", omop_strata(diabetes, window = omop_window(NULL, -1))),
      controls_per_case = 4, seed = omop_param("seed", type = "integer")))),
  variables = list(set_id = omop_population_column("set_id"), is_case = omop_population_column("is_case"),
                   w = omop_population_column("sampling_weight"), age = omop_age(), sex = omop_sex(),
                   statin_365 = omop_variable(name = "statin_365", event = statin_rx, reducer = omop_reduce("binary", window = omop_window(-365, -1)))),
  outputs = list(omop_output("cc_table", rows = "cc", format = "wide", grain = "episode",
                             columns = c("set_id", "is_case", "w", "age", "sex", "statin_365"), result_symbol = "D_cc")))
bound <- recipe_bind(rec, seed = 20261111L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
