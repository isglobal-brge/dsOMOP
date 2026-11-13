# Acceptance recipe 02 - new-user active comparator, two era arms (design 10.7 item 2)
library(dsOMOPClient)
acei <- omop_era(omop_event("drug_era", 1308216, descendants = TRUE, name = "acei_era_rows"), gap = 30, name = "acei")
arb  <- omop_era(omop_event("drug_era", 1367500, descendants = TRUE, name = "arb_era_rows"), gap = 30, name = "arb")
mi   <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
death <- omop_event("death", name = "death")
rec <- omop_recipe(
  intervals = list(acei, arb), events = list(mi, death),
  populations = list(omop_population("new_users",
    index = omop_index(omop_any_of(acei, arb), select = "first_eligible", end = omop_end("interval_end")),
    coverage = omop_coverage("unique"),
    filters = list(omop_observed(before = 365),
                   omop_has_not(acei, window = omop_window(-365, -1)),
                   omop_has_not(arb, window = omop_window(-365, -1))))),
  variables = list(arm = omop_population_column("index_source"), age = omop_age(), sex = omop_sex(),
                   prior_mi = omop_variable(name = "prior_mi", event = mi, reducer = omop_reduce("binary", window = omop_window(NULL, 0)))),
  outputs = list(
    omop_output("baseline", rows = "new_users", format = "wide", grain = "episode"),
    omop_output("tte", rows = "new_users", format = "survival", options = list(
      outcomes = list(mi = mi, death = death), format = "competing_risk", event_order = "first", outcome_mode = "named",
      tars = list(itt = omop_window(1, NULL),
                  on_treatment = omop_window(omop_bound("index", 1), omop_bound("index_end", 30))),
      censor = list("observation_end",
                    omop_censor(arb, "first_start_after_entry", when_index_source = "acei"),   # switch to the other arm (A-7)
                    omop_censor(acei, "first_start_after_entry", when_index_source = "arb")),
      prior_outcome = list(lookback = 365, action = "exclude")))))
plan <- recipe_to_plan(rec)
