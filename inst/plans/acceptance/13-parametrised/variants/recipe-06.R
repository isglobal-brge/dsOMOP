# Parametrised variant of acceptance recipe 06 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../06-coverage-persistence/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 06 - drug coverage, persistence and discontinuation censoring (design 10.7 item 6)
metf <- omop_event("drug_exposure", 1503297, descendants = TRUE, end = list(fallback = "days_supply"), name = "metf")
metf_era <- omop_era(metf, gap = omop_param("gap", type = "integer"), fallback = "days_supply", name = "metf_era")
metf_stock <- omop_era(metf, gap = 30, fallback = "days_supply", stockpile = TRUE, name = "metf_stock")
mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
death <- omop_event("death", name = "death")
w90 <- omop_window(1, 90)
rec <- omop_recipe(
  events = list(metf, mi, death), intervals = list(metf_era, metf_stock),
  populations = list(omop_population("users", index = omop_index(list(cohort_definition_id = 1001), select = "all", end = omop_end("event_end")))),
  variables = list(
    covered_rec = omop_variable(name = "covered_rec", event = metf_era, reducer = omop_reduce("covered_days", window = w90)),
    covered_eff = omop_variable(name = "covered_eff", event = metf_stock, reducer = omop_reduce("covered_days", window = w90, coverage = "effective_supply")),
    era_count = omop_variable(name = "era_count", event = metf_era, reducer = omop_reduce("era_count", window = w90)),
    first_era_days = omop_variable(name = "first_era_days", event = metf_era, reducer = omop_reduce("first_era_days", window = w90)),
    supply_start = omop_variable(name = "supply_start", event = metf, reducer = omop_reduce("supply_days", window = w90)),
    supply_clipped = omop_variable(name = "supply_clipped", event = metf, reducer = omop_reduce("supply_days", window = w90, fills = "clipped"))),
  outputs = list(
    omop_output("coverage", rows = "users", format = "wide", grain = "episode"),
    omop_output("persistence", rows = "users", format = "survival", options = list(
      outcomes = list(mi = mi), tar = omop_window(1, 365), format = "survival", event_order = "first",
      censor = list("observation_end", "death", omop_censor(metf_era, "first_end_after_entry", offset = 30))))))
bound <- recipe_bind(rec, gap = 30L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
