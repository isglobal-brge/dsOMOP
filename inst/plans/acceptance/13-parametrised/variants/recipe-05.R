# Parametrised variant of acceptance recipe 05 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../05-same-day-same-visit/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 05 - same-day repeated index events and same-visit links (design 10.7 item 5)
mi <- omop_event("condition_occurrence", 4329847, descendants = TRUE, name = "mi")
pci_v <- omop_event("procedure_occurrence", 4283095, descendants = TRUE, visit = "index", name = "pci_index_visit")
cp_v <- omop_event("condition_occurrence", 77670, descendants = TRUE, visit = "index", name = "chest_pain_index_visit")
inpatient <- omop_event("visit_occurrence", 9201, name = "inpatient")
rec <- omop_recipe(
  events = list(mi, pci_v, cp_v, inpatient),
  populations = list(omop_population("mi_all", index = omop_index(mi, select = "all", end = omop_end("event_end")),
                                     coverage = omop_coverage("unique"))),
  variables = list(
    n_same_day_mi = omop_variable(name = "n_same_day_mi", event = mi, reducer = omop_reduce("count", window = omop_window(0, 0))),
    pci_same_visit = omop_variable(name = "pci_same_visit", event = pci_v, reducer = omop_reduce("binary")),
    chest_pain_same_visit = omop_variable(name = "chest_pain_same_visit", event = cp_v, reducer = omop_reduce("binary")),
    inpatient_30 = omop_variable(name = "inpatient_30", event = inpatient, reducer = omop_reduce("count", window = omop_window(0, omop_param("follow", type = "integer")), distinct = "visit"))),
  outputs = list(omop_output("episodes", rows = "mi_all", format = "wide", grain = "episode")))
bound <- recipe_bind(rec, follow = 30L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json
