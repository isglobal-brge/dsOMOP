# Acceptance recipe 12 - ATLAS Circe import + baseline (design 10.7 item 12)
library(dsOMOPClient)
pop <- recipe_import_circe("circe.json", id = "t2dm_first", label = "Type 2 diabetes (Circe)")
rec <- omop_recipe(
  populations = list(pop),
  variables = list(age_group = omop_age(bands = TRUE), sex = omop_sex(), prior_obs = omop_observed_days(omop_window(-365, 0))),
  outputs = list(omop_output("baseline", rows = "t2dm_first", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)   # the importer must produce the population of plan.json
