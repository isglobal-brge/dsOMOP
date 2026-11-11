# Acceptance recipe 14 - oncology episodes with episode_event members (design 10.7 item 14)
library(dsOMOPClient)
episodes <- omop_event("episode", 32531, name = "episodes")
members <- omop_episode_members(episodes, name = "members")
member_drugs <- omop_episode_members(episodes, tables = "drug_exposure", name = "member_drugs")
specimens <- omop_event("specimen", 4001225, descendants = TRUE, name = "specimens")
rec <- omop_recipe(
  events = list(episodes, members, member_drugs, specimens),
  populations = list(omop_population("onc", label = "Treatment episodes",
                                     index = omop_index(episodes, select = "all", end = omop_end("event_end")),
                                     coverage = omop_coverage("unique"))),
  variables = list(
    n_members = omop_variable(name = "n_members", event = members, reducer = omop_reduce("count")),
    n_member_drugs = omop_variable(name = "n_member_drugs", event = member_drugs, reducer = omop_reduce("count")),
    first_member_days = omop_variable(name = "first_member_days", event = members, reducer = omop_reduce("first_date")),
    n_specimens = omop_variable(name = "n_specimens", event = specimens, reducer = omop_reduce("count", window = omop_window(-30, 365)))),
  outputs = list(omop_output("episodes", rows = "onc", format = "wide", grain = "episode")))
plan <- recipe_to_plan(rec)
