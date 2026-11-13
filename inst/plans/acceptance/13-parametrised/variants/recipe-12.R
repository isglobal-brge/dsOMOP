# Parametrised variant of acceptance recipe 12 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../12-circe-baseline/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 12 - ATLAS Circe import + baseline (design 10.7 item 12)
pop <- recipe_import_circe("circe.json", id = "t2dm_first", label = "Type 2 diabetes (Circe)")
rec <- omop_recipe(
  populations = list(pop),
  variables = list(age_group = omop_age(bands = TRUE), sex = omop_sex(), prior_obs = omop_observed_days(omop_window(-omop_param("prior", type = "integer"), 0))),
  outputs = list(omop_output("baseline", rows = "t2dm_first", format = "wide", grain = "episode")))
bound <- recipe_bind(rec, prior = 365L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json   # the importer must produce the population of plan.json
