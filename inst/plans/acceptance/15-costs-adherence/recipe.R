# Acceptance recipe 15 - costs and adherence: MPR and observed-clipped PDC (design 10.7 item 15)
library(dsOMOPClient)
metf <- omop_event("drug_exposure", 1503297, descendants = TRUE, end = list(fallback = "days_supply"), name = "metf")
visits <- omop_event("visit_occurrence", name = "visits")
visit_cost <- omop_cost(visits, field = "total_cost", currency = 44818668, name = "visit_cost")
drug_cost <- omop_cost(metf, field = "total_cost", currency = 44818668, name = "drug_cost")
metf_stock <- omop_era(metf, gap = 30, fallback = "days_supply", stockpile = TRUE, name = "metf_stock")
metf_era <- omop_era(metf, gap = 30, fallback = "days_supply", name = "metf_era")
w90 <- omop_window(1, 90); w10 <- omop_window(1, 10); w60 <- omop_window(1, 60)
rec <- omop_recipe(
  events = list(metf, visits, visit_cost, drug_cost), intervals = list(metf_stock, metf_era),
  populations = list(omop_population("users", index = omop_index(list(cohort_definition_id = 1001), select = "all", end = omop_end("event_end")),
                                     coverage = omop_coverage("union", gap = 30))),
  variables = list(
    visit_cost_60 = omop_variable(name = "visit_cost_60", event = visit_cost, reducer = omop_reduce("sum", window = w60)),
    drug_cost_60 = omop_variable(name = "drug_cost_60", event = drug_cost, reducer = omop_reduce("sum", window = w60)),
    supply_90 = omop_variable(name = "supply_90", event = metf, reducer = omop_reduce("supply_days", window = w90)),
    observed_90 = omop_observed_days(w90),
    pdc_num_eff_90 = omop_variable(name = "pdc_num_eff_90", event = metf_stock, reducer = omop_reduce("covered_days", window = w90, coverage = "effective_supply", observed_only = TRUE)),
    pdc_num_rec_90 = omop_variable(name = "pdc_num_rec_90", event = metf_era, reducer = omop_reduce("covered_days", window = w90, observed_only = TRUE)),
    covered_eff_90 = omop_variable(name = "covered_eff_90", event = metf_stock, reducer = omop_reduce("covered_days", window = w90, coverage = "effective_supply")),
    covered_rec_90 = omop_variable(name = "covered_rec_90", event = metf_era, reducer = omop_reduce("covered_days", window = w90)),
    observed_10 = omop_observed_days(w10),
    pdc_num_eff_10 = omop_variable(name = "pdc_num_eff_10", event = metf_stock, reducer = omop_reduce("covered_days", window = w10, coverage = "effective_supply", observed_only = TRUE)),
    covered_eff_10 = omop_variable(name = "covered_eff_10", event = metf_stock, reducer = omop_reduce("covered_days", window = w10, coverage = "effective_supply"))),
  outputs = list(omop_output("adherence", rows = "users", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
# MPR = supply_90 / observed_90 and PDC = pdc_num_eff_90 / observed_90 are computed in dsBase or ds.omop.mutate()
