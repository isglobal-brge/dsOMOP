# Acceptance recipe 14 - oncology episodes with episode_event members (design 10.7 item 14)
library(dsOMOPClient)
episodes <- omop_event("episode", 32531, name = "episodes")
members <- omop_episode_members(episodes, name = "members")
member_drugs <- omop_episode_members(episodes, tables = "drug_exposure", name = "member_drugs")
specimens <- omop_event("specimen", 4001225, descendants = TRUE, name = "specimens")
episode_course <- omop_era(episodes, gap = 0, end = "recorded_end", fallback = "none", name = "episode_course")
rec <- omop_recipe(
  events = list(episodes, members, member_drugs, specimens), intervals = list(episode_course),
  populations = list(
    omop_population("onc", label = "Treatment episodes", index = omop_index(episodes, select = "all", end = omop_end("event_end")),
                    coverage = omop_coverage("unique")),
    omop_population("first_line", label = "First treatment line", index = omop_index(episodes, select = "first", end = omop_end("event_end")),
                    coverage = omop_coverage("unique"))),
  variables = list(
    n_members = omop_variable(name = "n_members", event = members, reducer = omop_reduce("count")),
    n_member_drugs = omop_variable(name = "n_member_drugs", event = member_drugs, reducer = omop_reduce("count")),
    first_member_days = omop_variable(name = "first_member_days", event = members, reducer = omop_reduce("first_date")),
    n_specimens = omop_variable(name = "n_specimens", event = specimens, reducer = omop_reduce("count", window = omop_window(-30, 365))),
    line_days = omop_variable(name = "line_days", event = episode_course,
                              reducer = omop_reduce("covered_days", window = omop_window(omop_bound("index", 0), omop_bound("index_end", 0))))),
  outputs = list(omop_output("episodes", rows = "onc", format = "wide", grain = "episode",
                             columns = c("n_members", "n_member_drugs", "first_member_days", "n_specimens"), result_symbol = "D_onc"),
                 omop_output("first_line", rows = "first_line", format = "wide", grain = "episode",
                             columns = c("line_days", "n_member_drugs"), result_symbol = "D_first_line")))
plan <- recipe_to_plan(rec)
