# Parametrised variant of acceptance recipe 01 (design 10.7 item 13)
# One public parameter is declared with omop_param() and bound with recipe_bind(); the bound
# plan must equal ../../01-characterisation/plan.json after normalisation.
library(dsOMOPClient)
# Acceptance recipe 01 - characterisation table (design 10.7 item 1)
w365 <- omop_window(omop_param("lookback", type = "integer", default = 365) * -1L, 0)
cond <- c(dm = 201820, prediabetes = 201826, htn1 = 320128, htn2 = 316866, mi = 4329847, mi2 = 312327,
          ckd = 46271022, af = 313217, hf = 316139, hld = 432867, obesity = 433736, pregnancy = 4299535,
          stroke = 381316, stroke2 = 372924, sepsis = 132797, cirrhosis = 4064161, asthma = 317009,
          copd = 255573, chest_pain = 77670, resp_inf = 320136, prior_bleed = 192671,
          major_bleed = 4108832, malignancy = 4112853, chronic_pain = 436096, htn_comp = 321318,
          covar2 = 4000777)
drug <- c(metformin = 1503297, insulin = 1596977, statin = 1539403, warfarin = 1310149, acei = 1308216,
          arb = 1367500, beta_blocker = 1346823, diuretic = 974166, doac = 40228152, sglt2 = 1488000)
proc <- c(dialysis = 4032243, pci = 4283095, bariatric = 4322471, cath = 4139525)
binary_vars <- function(prefix, table, ids) {
  lapply(stats::setNames(names(ids), paste0(prefix, "_", names(ids))), function(n)
    omop_variable(event = omop_event(table, ids[[n]], descendants = TRUE, name = paste0(prefix, "_", n)),
                  reducer = omop_reduce("binary", window = w365)))
}
hba1c <- omop_event("measurement", 3004410, value = "value_as_number",
                    unit = omop_unit(8554, convert = list("8753" = c(factor = 0.0915, offset = 2.15))),
                    name = "hba1c")
glucose <- omop_event("measurement", 3004501, value = "value_as_number",
                      unit = omop_unit(8840, convert = list("8753" = c(factor = 18.0182, offset = 0))),
                      name = "glucose")
rec <- omop_recipe(
  populations = list(omop_population("c1", index = omop_index(list(cohort_definition_id = 1), select = "all",
                                                             end = omop_end("event_end")))),
  variables = c(
    binary_vars("cond", "condition_occurrence", cond),
    binary_vars("drug", "drug_exposure", drug),
    binary_vars("proc", "procedure_occurrence", proc),
    list(age = omop_age(), sex = omop_sex(),
         hba1c_last = omop_variable(name = "hba1c_last", event = hba1c, reducer = omop_reduce("last_value", window = w365)),
         glucose_last = omop_variable(name = "glucose_last", event = glucose, reducer = omop_reduce("last_value", window = w365)),
         charlson = omop_score("charlson"))),
  outputs = list(omop_output("table1", rows = "c1", format = "wide", grain = "episode")))
bound <- recipe_bind(rec, lookback = 365L)
plan <- recipe_to_plan(bound)   # equals the base recipe's plan.json   # equals plan.json after normalisation
