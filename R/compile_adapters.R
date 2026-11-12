# Module: Bounded domain adapters (design-2.10.3 sections 4.1, 6.4 and 11;
# amendment A-4)
#
# Two event sources that join through reviewed metadata only, never through a
# free join:
#
#   episode_members  the events linked to the episodes of an episode event
#                    through episode_event (episode_id, event_id,
#                    episode_event_field_concept_id). The discriminator maps,
#                    through the reviewed table inst/ohdsi/
#                    episode_event_fields.csv, to the member table and its key
#                    column, whose role record supplies the member's roles.
#                    One row per (episode, member event) link: an event linked
#                    to two episodes yields two rows. person_id comes from the
#                    episode and must agree with the member row's own person
#                    (a disagreement is refused, adapter_person_mismatch).
#   cost             the cost rows of a table event: cost_event_id = the
#                    event's key and cost_domain_id = the table's domain from
#                    the reviewed mapping inst/ohdsi/cost_domains.csv (never a
#                    table-name guess). One row per cost row; value = the one
#                    declared amount field; the dates are the parent event's;
#                    currency_concept_id must be single within the source
#                    (`currency` restricts it; otherwise a source mixing
#                    currencies is refused, cost_currency_mixed; amounts are
#                    never summed across currencies or converted).
#
# Both carry the parent event identity as source-qualified columns
# (parent_source_ns, parent_event_id) so that an episode-indexed consumer
# joins members by the parent episode identity and person, not merely by
# person and window. Data-dependent refusals are typed plan rejections
# transported under validation_error (reason = the construct), checked before
# any output is materialised.

.ADAPTER_PARENT_COLUMNS <- c("parent_source_ns", "parent_event_id")

# Columns every adapter adds to the event column contract.
.adapterParentSpec <- function() {
  list(parent_source_ns = .fragmentColumn("text", FALSE, "origin_identity"),
       parent_event_id = .fragmentColumn("bigint", FALSE, "origin_identity"))
}

.adapterQualified <- function(handle, bp, table) {
  .qualifyTable(handle, table, .resolveTableSchema(
    handle, table, bp$tables$schema_category[bp$tables$table_name == table][1L]))
}

.adapterTablePresent <- function(bp, table) {
  table %in% bp$tables$table_name[bp$tables$present_in_db] &&
    !is.null(bp$columns[[table]]) && nrow(bp$columns[[table]]) > 0L
}

# Run a COUNT(*) check over a fragment chain (the count never leaves the
# server: callers compare it with a structural bound only).
.adapterCount <- function(ctx, final, select) {
  sql <- .buildStatement(ctx, final, select = select, dialect = .compileDialect(ctx))
  res <- .executeQuery(ctx[["handle"]], sql)
  n <- suppressWarnings(as.numeric(res[[1L]][1L]))
  if (length(n) != 1L || is.na(n)) 0 else n
}

#' Compile an episode-membership event (frozen compiler signature)
#'
#' @param ctx Execution context.
#' @param node The IR event node (\code{episode_members} variant) with its
#'   plan name in \code{attr(node, "name")}.
#' @param episode_fragment The compiled parent episode event
#'   (\code{ev_<episode>}, from \code{.compileEvent()}).
#' @return The member fragment \code{ev_<name>} (kind event, grain row) with
#'   the member roles plus \code{member_table}, \code{episode_id},
#'   \code{episode_concept_id}, \code{parent_source_ns} and
#'   \code{parent_event_id}.
#' @keywords internal
.compileEpisodeMembers <- function(ctx, node, episode_fragment) {
  name <- .compileNodeName(node, "episode_members")
  base <- paste0("/events/", name, "/episode_members")
  existing <- .eventMeta(ctx, name)
  if (!is.null(existing)) return(.fragmentGet(ctx, existing$fragments[["final"]]))
  spec <- node[["episode_members"]]
  if (!is.list(spec)) stop("Not an episode_members event.", call. = FALSE)
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  parent <- spec[["episode"]][["event"]]
  .assertFragment(episode_fragment)
  pmeta <- .eventMeta(ctx, parent)
  if (is.null(pmeta) || !identical(pmeta$fragments[["final"]], episode_fragment$name)) {
    stop("The parent episode event must be compiled in this context first.", call. = FALSE)
  }
  if (!identical(pmeta$kind, "table") || !identical(pmeta$table, "episode")) {
    .irReject("episode_members needs an event over the episode table",
              field = paste0(base, "/episode"))
  }
  mode <- spec[["on_missing_table"]] %||% "error"
  extra <- c(list(member_table = .fragmentColumn("text", FALSE, "origin_identity"),
                  episode_id = .fragmentColumn("bigint", FALSE, "origin_identity"),
                  episode_concept_id = .fragmentColumn("integer", TRUE, "concept")),
             .adapterParentSpec())
  unavailable <- function() {
    .eventUnavailable(ctx, name, "episode_event", kind = "episode_members", extra = extra)
  }
  if (identical(pmeta$availability, "unavailable")) return(unavailable())
  bp <- .buildBlueprint(handle)
  missing_table <- function(table) {
    if (identical(mode, "unavailable")) return(TRUE)
    .irReject(paste0("table '", table, "' is not available at this site"),
              code = "unavailable_table", field = base)
  }
  if (!.adapterTablePresent(bp, "episode_event")) {
    missing_table("episode_event")
    return(unavailable())
  }
  ee_cols <- bp$columns[["episode_event"]]$column_name
  if (!all(c("episode_id", "event_id", "episode_event_field_concept_id") %in% ee_cols)) {
    missing_table("episode_event")
    return(unavailable())
  }
  fields <- .irEpisodeEventFields()
  requested <- spec[["tables"]]
  explicit <- !is.null(requested)
  tables <- if (explicit) tolower(as.character(unlist(requested))) else fields$table_name
  unknown <- setdiff(tables, fields$table_name)
  if (length(unknown)) {
    .irReject(paste0("'", unknown[[1L]], "' has no reviewed episode_event field mapping"),
              field = paste0(base, "/tables"))
  }
  members <- list()
  for (t in tables) {
    role <- .roleOf(handle, t)
    if (is.null(role)) {
      if (explicit) {
        missing_table(t)
        return(unavailable())
      }
      next
    }
    map <- fields[fields$table_name == t, , drop = FALSE]
    if (!identical(tolower(map$key_column[[1L]]), role[["event_key"]])) {
      stop("The reviewed episode_event mapping of '", t,
           "' disagrees with its role record.", call. = FALSE)
    }
    members[[t]] <- list(table = t, role = role,
                         field = as.integer(map$episode_event_field_concept_id[[1L]]))
  }
  ee <- .adapterQualified(handle, bp, "episode_event")
  tnull <- function(type) .compileTypedNull(h, type)
  ep <- episode_fragment$name
  m <- function(role, key) if (!is.null(role[[key]])) paste0("m.", role[[key]])
  selects <- vapply(members, function(mb) {
    role <- mb$role
    start <- paste0("m.", role[["start"]])
    end <- if (!is.null(role[["end"]])) paste0("COALESCE(m.", role[["end"]], ", ", start, ")") else start
    value <- m(role, "value") %||% tnull("numeric")
    unit <- m(role, "unit") %||% tnull("integer")
    rlo <- m(role, "range_low") %||% tnull("numeric")
    rhi <- m(role, "range_high") %||% tnull("numeric")
    cols <- c(
      person_id = "ep.person_id",
      event_id = .sqlLosslessKey(h, paste0("m.", role[["event_key"]])),
      source_ns = .compileTextLiteral(name),
      start_date = start, end_date = end,
      end_recorded = m(role, "end") %||% tnull("date"),
      supply = m(role, "supply") %||% tnull("integer"),
      concept_id = m(role, "domain_concept") %||% tnull("integer"),
      value_source = value, unit_source = unit,
      operator_concept_id = m(role, "operator") %||% tnull("integer"),
      range_low_source = rlo, range_high_source = rhi,
      visit_id = if (!is.null(role[["visit_occurrence"]])) {
        .sqlLosslessKey(h, paste0("m.", role[["visit_occurrence"]]))
      } else tnull("bigint"),
      value = value, unit_concept_id = unit, range_low = rlo, range_high = rhi,
      unit_unknown = .sqlIntegerLiteral(h, 0L),
      member_table = .compileTextLiteral(mb$table),
      episode_id = "ep.event_id", episode_concept_id = "ep.concept_id",
      parent_source_ns = "ep.source_ns", parent_event_id = "ep.event_id")
    paste0("SELECT ", paste(paste0(cols, " AS ", names(cols)), collapse = ", "),
           " FROM ", ep, " ep INNER JOIN ", ee, " ee ON ee.episode_id = ep.event_id",
           " AND ee.episode_event_field_concept_id = ", .sqlIntegerLiteral(h, mb$field),
           " INNER JOIN ", .adapterQualified(handle, bp, mb$table), " m ON m.",
           role[["event_key"]], " = ee.event_id AND m.", role[["person"]], " = ep.person_id")
  }, "")
  spec_cols <- c(.eventColumnSpec("integer")[c(.EVENT_SRC_COLUMNS, .EVENT_NORM_COLUMNS)], extra)
  nm_src <- .fragmentName("ev", name, "src")
  nm_final <- .fragmentName("ev", name)
  private <- c(.EVENT_PRIVATE_COLUMNS, "episode_id", .ADAPTER_PARENT_COLUMNS)
  identity <- c("source_ns", "parent_source_ns", "parent_event_id", "member_table", "event_id")
  ordering <- c("start_date", "source_ns", "member_table", "event_id", "parent_event_id")
  ukey <- c("parent_source_ns", "parent_event_id", "member_table", "event_id")
  lineage <- list(person_col = "person_id", via_roster = NULL)
  body_src <- if (length(selects)) {
    paste(selects, collapse = " UNION ALL ")
  } else {
    nulls <- vapply(names(spec_cols), function(col) {
      if (identical(col, "source_ns")) return(.compileTextLiteral(name))
      if (identical(col, "unit_unknown")) return(.sqlIntegerLiteral(h, 0L))
      tnull(spec_cols[[col]]$type)
    }, "")
    paste0("SELECT ", paste(paste0(nulls, " AS ", names(spec_cols)), collapse = ", "),
           .compileNoTableFrom(h), " WHERE 1 = 0")
  }
  f_src <- .fragment(nm_src, "event", "row", body_src, spec_cols,
                     dependencies = ep, unique_key = ukey, person_lineage = lineage,
                     source_identity_columns = identity, private_columns = private,
                     ordering = ordering)
  roles <- .episodeMemberRoles(lapply(members, function(mb) mb$role))
  catalogue <- .episodeMemberCatalogue(name, roles)
  where <- spec[["where"]]
  pred_sql <- if (!is.null(where)) .predicateSql(h, where, catalogue, "x", paste0(base, "/where"))
  body_final <- paste0("SELECT ", paste(paste0("x.", names(spec_cols)), collapse = ", "),
                       " FROM ", nm_src, " x", if (!is.null(pred_sql)) paste0(" WHERE ", pred_sql))
  f_final <- .fragment(nm_final, "event", "row", body_final, spec_cols,
                       dependencies = nm_src, unique_key = ukey, person_lineage = lineage,
                       source_identity_columns = identity, private_columns = private,
                       ordering = ordering)
  for (f in list(f_src, f_final)) {
    .sqlAssertTypedSql(h, f$select_body)
    .fragmentRegister(ctx, f)
  }
  # person_id comes from the episode; a member row of another person is
  # refused (never re-attributed, never silently dropped).
  for (mb in members) {
    role <- mb$role
    check <- paste0(
      "SELECT COUNT(*) AS n FROM ", ep, " ep INNER JOIN ", ee, " ee ON ee.episode_id = ep.event_id",
      " AND ee.episode_event_field_concept_id = ", .sqlIntegerLiteral(h, mb$field),
      " INNER JOIN ", .adapterQualified(handle, bp, mb$table), " m ON m.", role[["event_key"]],
      " = ee.event_id WHERE m.", role[["person"]], " <> ep.person_id")
    if (.adapterCount(ctx, episode_fragment, check) > 0) {
      .compileReject(paste0("event '", name, "': a ", mb$table, " member linked to an ",
                            "episode belongs to another person"),
                     reason = "adapter_person_mismatch", field = base)
    }
  }
  .eventMetaSet(ctx, name, list(
    name = name, kind = "episode_members", table = "episode_event",
    availability = "available", roles = roles, catalogue = catalogue,
    fragments = c(src = nm_src, final = nm_final), index_visit = FALSE,
    parent = parent, member_tables = names(members), columns = names(spec_cols)))
  f_final
}

# Roles some member table carries (a member role absent from every member
# table is refused in predicates).
.episodeMemberRoles <- function(role_records) {
  unique(unlist(lapply(role_records, function(r) {
    c(if (!is.null(r[["value"]])) "value", if (!is.null(r[["unit"]])) "unit",
      if (!is.null(r[["operator"]])) "operator",
      if (!is.null(r[["range_low"]])) "range", if (!is.null(r[["supply"]])) "supply",
      if (!is.null(r[["end"]])) "end_recorded",
      if (!is.null(r[["domain_concept"]])) "concept",
      if (!is.null(r[["visit_occurrence"]])) "visit")
  })))
}

.episodeMemberCatalogue <- function(name, roles) {
  .adapterCatalogue(name, roles, list(
    member_table = list(type = "text"),
    episode_concept_id = list(type = "integer", concept = TRUE)))
}

# Roles of a cost source: the amount value, the parent's concept, recorded end
# and visit link.
.costRoles <- function(parent_roles) {
  c("value", "concept", if ("end_recorded" %in% parent_roles) "end_recorded",
    if ("visit" %in% parent_roles) "visit")
}

.costCatalogue <- function(name, roles) {
  .adapterCatalogue(name, roles, list(
    currency_concept_id = list(type = "integer", concept = TRUE),
    cost_type_concept_id = list(type = "integer", concept = TRUE)))
}

# Predicate catalogue of an adapter source: the event roles some member has,
# plus the adapter's own columns.
.adapterCatalogue <- function(name, roles, extra) {
  columns <- list(
    start_date = list(type = "date"), end_date = list(type = "date"),
    end_recorded = list(type = "date"), supply = list(type = "integer"),
    concept_id = list(type = "integer", concept = TRUE),
    value_source = list(type = "numeric"), unit_source = list(type = "integer", concept = TRUE),
    operator_concept_id = list(type = "integer", concept = TRUE),
    range_low_source = list(type = "numeric"), range_high_source = list(type = "numeric"),
    value = list(type = "numeric"), unit_concept_id = list(type = "integer", concept = TRUE),
    range_low = list(type = "numeric"), range_high = list(type = "numeric"))
  columns <- c(columns, extra)
  refused <- c(person_id = "a person identifier is never filterable",
               event_id = "row identifiers are never filterable",
               source_ns = "the source namespace is internal",
               visit_id = "row identifiers are never filterable",
               episode_id = "row identifiers are never filterable",
               parent_source_ns = "the source namespace is internal",
               parent_event_id = "row identifiers are never filterable",
               unit_unknown = "unit_unknown exists only with a unit conversion")
  lacks <- function(cols, why) for (c in cols) refused[[c]] <<- why
  if (!"value" %in% roles) lacks(c("value", "value_source"), "the source has no value role")
  if (!"unit" %in% roles) lacks(c("unit_concept_id", "unit_source"), "the source has no unit role")
  if (!"operator" %in% roles) lacks("operator_concept_id", "the source has no operator role")
  if (!"range" %in% roles) lacks(c("range_low", "range_high", "range_low_source", "range_high_source"),
                                 "the source has no reference-range role")
  if (!"supply" %in% roles) lacks("supply", "the source has no days_supply role")
  if (!"end_recorded" %in% roles) lacks("end_recorded", "the source has no recorded end role")
  if (!"concept" %in% roles) lacks("concept_id", "the source has no concept column")
  .predicateCatalogue(columns, character(0), refused, what = paste0("event '", name, "'"))
}

#' Compile a cost event (frozen compiler signature)
#'
#' @param ctx Execution context.
#' @param node The IR event node (\code{cost} variant) with its plan name in
#'   \code{attr(node, "name")}.
#' @param event_fragment The compiled parent table event (\code{ev_<event>}).
#' @return The cost fragment \code{ev_<name>} (kind event, grain row): one row
#'   per cost row, \code{value} = the declared amount field, dates and
#'   concept of the parent event, \code{currency_concept_id},
#'   \code{cost_type_concept_id}, \code{parent_source_ns},
#'   \code{parent_event_id}.
#' @keywords internal
.compileCost <- function(ctx, node, event_fragment) {
  name <- .compileNodeName(node, "cost")
  base <- paste0("/events/", name, "/cost")
  existing <- .eventMeta(ctx, name)
  if (!is.null(existing)) return(.fragmentGet(ctx, existing$fragments[["final"]]))
  spec <- node[["cost"]]
  if (!is.list(spec)) stop("Not a cost event.", call. = FALSE)
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  parent <- spec[["event"]][["event"]]
  .assertFragment(event_fragment)
  pmeta <- .eventMeta(ctx, parent)
  if (is.null(pmeta) || !identical(pmeta$fragments[["final"]], event_fragment$name)) {
    stop("The parent event must be compiled in this context first.", call. = FALSE)
  }
  if (!identical(pmeta$kind, "table")) {
    .irReject("the cost source must reference a table event", field = paste0(base, "/event"))
  }
  domains <- .irCostDomains()
  row <- domains[domains$table_name == pmeta$table, , drop = FALSE]
  if (!nrow(row)) {
    .irReject("the cost source must reference a table event with a reviewed cost_domain_id mapping",
              field = paste0(base, "/event"))
  }
  field <- spec[["field"]]
  amounts <- c("total_charge", "total_cost", "total_paid", "paid_by_payer", "paid_by_patient",
               "paid_patient_copay", "paid_patient_coinsurance", "paid_patient_deductible",
               "paid_by_primary", "paid_ingredient_cost", "paid_dispensing_fee", "amount_allowed")
  if (!is.character(field) || length(field) != 1L || !field %in% amounts) {
    .irReject("unknown cost amount field", field = paste0(base, "/field"))
  }
  mode <- spec[["on_missing_table"]] %||% "error"
  extra <- c(list(currency_concept_id = .fragmentColumn("integer", TRUE, "unit"),
                  cost_type_concept_id = .fragmentColumn("integer", TRUE, "concept")),
             .adapterParentSpec())
  unavailable <- function() .eventUnavailable(ctx, name, "cost", kind = "cost", extra = extra)
  if (identical(pmeta$availability, "unavailable")) return(unavailable())
  bp <- .buildBlueprint(handle)
  if (!.adapterTablePresent(bp, "cost")) {
    if (identical(mode, "unavailable")) return(unavailable())
    .irReject("table 'cost' is not available at this site", code = "unavailable_table", field = base)
  }
  cost_cols <- bp$columns[["cost"]]$column_name
  needed <- c("cost_id", "cost_event_id", "cost_domain_id", "currency_concept_id",
              "cost_type_concept_id", field)
  if (!all(needed %in% cost_cols)) {
    .irReject(paste0("the site's cost table lacks the column(s) ",
                     paste(setdiff(needed, cost_cols), collapse = ", ")), field = base)
  }
  prole <- .roleOf(handle, pmeta$table)
  if (!identical(tolower(row$key_column[[1L]]), prole[["event_key"]])) {
    stop("The reviewed cost-domain mapping of '", pmeta$table,
         "' disagrees with its role record.", call. = FALSE)
  }
  currency <- spec[["currency"]]
  if (!is.null(currency)) {
    currency <- tryCatch(.conceptIdList(currency), error = function(e) integer(0))
    if (length(currency) != 1L) .irReject("currency is one concept id", field = paste0(base, "/currency"))
  }
  cost_type <- spec[["cost_type"]]
  if (!is.null(cost_type)) {
    cost_type <- tryCatch(.conceptIdList(cost_type), error = function(e) integer(0))
    if (!length(cost_type)) .irReject("cost_type needs concept ids", field = paste0(base, "/cost_type"))
  }
  tnull <- function(type) .compileTypedNull(h, type)
  p <- event_fragment$name
  amount <- paste0("c.", field)
  cols <- c(
    person_id = "p.person_id",
    event_id = .sqlLosslessKey(h, "c.cost_id"),
    source_ns = .compileTextLiteral(name),
    start_date = "p.start_date", end_date = "p.end_date", end_recorded = "p.end_recorded",
    supply = tnull("integer"), concept_id = "p.concept_id",
    value_source = amount, unit_source = tnull("integer"),
    operator_concept_id = tnull("integer"),
    range_low_source = tnull("numeric"), range_high_source = tnull("numeric"),
    visit_id = "p.visit_id",
    value = amount, unit_concept_id = tnull("integer"),
    range_low = tnull("numeric"), range_high = tnull("numeric"),
    unit_unknown = .sqlIntegerLiteral(h, 0L),
    currency_concept_id = "c.currency_concept_id",
    cost_type_concept_id = "c.cost_type_concept_id",
    parent_source_ns = "p.source_ns", parent_event_id = "p.event_id")
  where <- c(if (!is.null(currency)) paste0("c.currency_concept_id = ", .sqlIntegerLiteral(h, currency)),
             if (!is.null(cost_type)) .sqlIdInPredicate("c.cost_type_concept_id", cost_type))
  body_src <- paste0(
    "SELECT ", paste(paste0(cols, " AS ", names(cols)), collapse = ", "),
    " FROM ", p, " p INNER JOIN ", .adapterQualified(handle, bp, "cost"), " c",
    " ON c.cost_event_id = p.event_id AND c.cost_domain_id = ",
    .compileTextLiteral(row$cost_domain_id[[1L]]),
    if (length(where)) paste0(" WHERE ", paste(where, collapse = " AND ")))
  spec_cols <- c(.eventColumnSpec("integer")[c(.EVENT_SRC_COLUMNS, .EVENT_NORM_COLUMNS)], extra)
  nm_src <- .fragmentName("ev", name, "src")
  nm_final <- .fragmentName("ev", name)
  private <- c(.EVENT_PRIVATE_COLUMNS, .ADAPTER_PARENT_COLUMNS)
  identity <- c("source_ns", "event_id", .ADAPTER_PARENT_COLUMNS)
  lineage <- list(person_col = "person_id", via_roster = NULL)
  f_src <- .fragment(nm_src, "event", "row", body_src, spec_cols,
                     dependencies = p, unique_key = "event_id", person_lineage = lineage,
                     source_identity_columns = identity, private_columns = private,
                     ordering = .EVENT_ORDERING)
  roles <- .costRoles(pmeta$roles)
  catalogue <- .costCatalogue(name, roles)
  pred <- spec[["where"]]
  pred_sql <- if (!is.null(pred)) .predicateSql(h, pred, catalogue, "x", paste0(base, "/where"))
  body_final <- paste0("SELECT ", paste(paste0("x.", names(spec_cols)), collapse = ", "),
                       " FROM ", nm_src, " x", if (!is.null(pred_sql)) paste0(" WHERE ", pred_sql))
  f_final <- .fragment(nm_final, "event", "row", body_final, spec_cols,
                       dependencies = nm_src, unique_key = "event_id", person_lineage = lineage,
                       source_identity_columns = identity, private_columns = private,
                       ordering = .EVENT_ORDERING)
  for (f in list(f_src, f_final)) {
    .sqlAssertTypedSql(h, f$select_body)
    .fragmentRegister(ctx, f)
  }
  if (is.null(currency)) {
    mixed <- paste0("SELECT COUNT(*) AS n FROM (SELECT DISTINCT COALESCE(x.currency_concept_id, ",
                    .sqlIntegerLiteral(h, -1L), ") AS c FROM ", nm_final, " x) d")
    if (.adapterCount(ctx, f_final, mixed) > 1) {
      .compileReject(paste0("event '", name, "': the cost rows of this source mix currencies; ",
                            "restrict them with currency = <concept id>"),
                     reason = "cost_currency_mixed", field = paste0(base, "/currency"))
    }
  }
  .eventMetaSet(ctx, name, list(
    name = name, kind = "cost", table = "cost", availability = "available",
    roles = roles, catalogue = catalogue, fragments = c(src = nm_src, final = nm_final),
    index_visit = FALSE, parent = parent, field = field, currency = currency,
    columns = names(spec_cols)))
  f_final
}
