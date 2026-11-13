# Acceptance recipe 08 - SCCS matrix: exposure, age-band and calendar partitions (design 10.7 item 8)
library(dsOMOPClient)
aspirin <- omop_event("drug_exposure", 1124300, descendants = TRUE, end = list(fallback = "days_supply"), name = "aspirin")
aspirin_era <- omop_era(aspirin, gap = 30, fallback = "days_supply", name = "aspirin_era")
mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
rec <- omop_recipe(
  events = list(aspirin, mi), intervals = list(aspirin_era),
  populations = list(omop_population("sccs", index = omop_index(list(cohort_definition_id = 1002), select = "all", end = omop_end("event_end")))),
  outputs = list(omop_output("matrix", rows = "sccs", format = "person_period", options = list(
    representation = "dense", bins = list(width = 365), window = omop_window(0, 365),
    partition = list(boundaries = list(aspirin_era), age_bands = 5, calendar = "year"),
    outcomes = list(mi = mi), exposures = list(aspirin = aspirin_era), analyses = "count"))))
plan <- recipe_to_plan(rec)
