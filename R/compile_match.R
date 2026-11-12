# Module: Matching -- the exact bounded risk-set sampler (design-2.10.3
# sections 2.8 and 6.10; amendments A-20 c, f and j)
#
#   1. Eligible pair relation (SQL, complete). The cases are the cases
#      population's final roster (case time t = cohort_start_date, full case
#      identity K_c = (cases namespace, subject_id, cohort_start_date,
#      cohort_end_date, index_source, index_key)). The controls template is
#      instantiated once for the whole case roster (A-20 c): one candidates
#      fragment of the template population with a private cand_key per
#      (case row, person) pair and cohort_start_date := t, over the persons
#      with an actual recorded observation period containing t; the
#      template's coverage (its policy, compiled once on the instantiation),
#      anchors and criteria are evaluated per pair with index := t. A pair is
#      eligible when the person is not the case, is covered at t by an actual
#      record (never a tolerated gap), satisfies the template's criteria, is
#      alive at t (no death before t), has no outcome on or before t under
#      prior_outcome = "exclude" (an outcome exactly at t excludes under
#      "allow" too), no later outcome under future_cases = "ineligible", and
#      shares the case's strata at t (sex = gender_concept_id; age band =
#      floor((year(t) - year_of_birth) / w); calendar year; presence of a
#      source in a window anchored at t), a NULL stratum on either side being
#      ineligible, or matching a NULL under missing_strata = "match_missing".
#   2. One capped realisation, never truncated: the relation is fetched into R
#      once with a cap + 1 stop (cap = min(max_candidates,
#      dsomop.max_memory_rows)); cap + 1 rows refuse the plan with
#      match_candidates_exceeded before any selection, no count in the
#      message. No COUNT precedes the fetch. The case roster is fetched with
#      the same stop (dsomop.max_memory_rows).
#   3. Deterministic order: h = SHA-256 of the versioned length-prefixed
#      encoding of (seed, K_c, control); a case's controls are ordered by
#      (h, control person id) -- independent of database row order, dialect
#      and person_id magnitude.
#   4. Selection over the complete case roster in (t, K_c) order, zero-pair
#      cases included: between_sets takes the first min(k, N_c); none takes
#      the first k controls not used by an earlier set; a case short of k
#      follows insufficient (keep_available keeps what it found, drop_case
#      drops the case and, under none, commits no allocation: its tentative
#      controls go back to the pool).
#   5. Weights: cases 1; between_sets controls N_c / m_c (pool information,
#      released as assigned data under the final-support rule); none
#      controls NA.
#   6. The matched roster: one row per member with its own person, index t
#      (cohort_start_date = cohort_end_date = t for both arms), set_id = rank
#      of the case in the complete (t, K_c) order, is_case, sampling_weight,
#      index_source "match:<pop>", index_key "<serialised K_c>|case" or
#      "|control", the case's original visit/parent association (controls
#      have no index event association), and the op_* and coverage component of
#      the respective lookup (the cases' roster, the template's coverage at t;
#      one shared policy, validated). The realised rows are materialised once
#      -- a session temporary table, or a literal relation on the MySQL
#      family (MySQL cannot read a temporary table twice in one statement) --
#      and ranked into cohort_row_id like any roster.
# Only the matched roster leaves this module; the pair relation, N_c, strata
# and unmatched cases stay private.

.MATCH_ENCODING <- "dsomop-match-v1"

#' Identities of the matching fragments of a plan (amendment A-17)
#'
#' Per match population: \code{("mt", <pop>, "cases")} (the case roster with
#' its strata and visit order), \code{("mt", <pop>, "pairs")} (the eligible
#' pair relation) and \code{("mt", <pop>, "rows")} (the realised matched
#' rows). The template instantiation uses the template population's own
#' candidate identities (A-20 c).
#' @keywords internal
.matchFragmentIdentities <- function(ir) {
  out <- list()
  for (p in names(ir[["populations"]] %||% list())) {
    if (!identical(ir[["populations"]][[p]][["kind"]], "match")) next
    for (s in c("cases", "pairs", "rows")) out[[length(out) + 1L]] <- .fragmentIdentity("mt", p, s)
  }
  out
}

# --- the published encoding (design section 6.10) -----------------------------------

# One field: tag byte (0x00 null, 0x01 integer, 0x02 text, 0x03 date) ||
# u32 big-endian byte length || bytes.
.matchField <- function(tag, value = NULL) {
  bytes <- if (is.null(value)) raw(0) else charToRaw(enc2utf8(as.character(value)))
  n <- length(bytes)
  c(as.raw(tag), as.raw(c(bitwAnd(bitwShiftR(n, 24L), 255L), bitwAnd(bitwShiftR(n, 16L), 255L),
                          bitwAnd(bitwShiftR(n, 8L), 255L), bitwAnd(n, 255L))), bytes)
}

# Canonical ASCII decimal of an integer value (no leading zeros, '-' sign).
.matchDecimal <- function(x) {
  if (inherits(x, "integer64")) x <- as.character(x)
  if (is.numeric(x)) {
    if (length(x) != 1L || is.na(x) || x != trunc(x) || abs(x) > 2^53) {
      stop("A matching integer must be one whole number (larger keys as decimal text).", call. = FALSE)
    }
    x <- formatC(x, format = "f", digits = 0L)
  }
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || !grepl("^[-+]?[0-9]+$", x)) {
    stop("A matching integer must be decimal digits.", call. = FALSE)
  }
  neg <- startsWith(x, "-")
  body <- sub("^[-+]?0*([0-9])", "\\1", x)
  if (identical(body, "0")) "0" else paste0(if (neg) "-", body)
}

# Identity text is ASCII by construction (plan-grammar names, decimal keys,
# ISO dates, the separators of composite keys), for which UTF-8 NFC is the
# identity; anything else fails closed.
.matchText <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NULL)
  x <- as.character(x)
  if (length(x) != 1L || any(utf8ToInt(enc2utf8(x)) > 127L)) {
    stop("Matching identities are ASCII text.", call. = FALSE)
  }
  x
}

.matchDate <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NULL)
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  x <- substr(as.character(x), 1L, 10L)
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)) stop("A matching date is YYYY-MM-DD.", call. = FALSE)
  x
}

# The case part of E (every field but the control), reused across a pool.
.matchCasePrefix <- function(seed, namespace, subject_id, start, end, source, key) {
  end <- .matchDate(end)
  key <- .matchText(key)
  c(.matchField(0x02, .MATCH_ENCODING), .matchField(0x01, .matchDecimal(seed)),
    .matchField(0x02, .matchText(namespace)), .matchField(0x01, .matchDecimal(subject_id)),
    .matchField(0x03, .matchDate(start)),
    if (is.null(end)) .matchField(0x00) else .matchField(0x03, end),
    .matchField(0x02, .matchText(source)),
    if (is.null(key)) .matchField(0x00) else .matchField(0x02, key))
}

#' The versioned matching encoding (design section 6.10)
#'
#' \code{E = f(text, "dsomop-match-v1") || f(int, seed) || f(text, cases
#' namespace) || f(int, subject_id) || f(date, cohort_start_date) ||
#' f(date|null, cohort_end_date) || f(text, index_source) || f(text|null,
#' index_key) || f(int, control person_id)}, with \code{f(tag, v) = tag (1
#' byte: 0x00 null, 0x01 integer, 0x02 text, 0x03 date) || u32 big-endian
#' byte length || bytes}: integers as ASCII decimal without leading zeros,
#' text as UTF-8 (NFC; identities are ASCII), dates as \code{YYYY-MM-DD},
#' null as tag 0x00 with length 0.
#'
#' @return Raw vector.
#' @keywords internal
.matchEncode <- function(seed, namespace, subject_id, start, end, source, key, control) {
  c(.matchCasePrefix(seed, namespace, subject_id, start, end, source, key),
    .matchField(0x01, .matchDecimal(control)))
}

#' SHA-256 of the matching encoding, lowercase hexadecimal (design 6.10)
#'
#' The fixed test vectors of \code{tests/testthat/fixtures/match-vectors.json}
#' reproduce byte for byte.
#' @keywords internal
.matchHash <- function(seed, namespace, subject_id, start, end, source, key, control) {
  paste(as.character(openssl::sha256(.matchEncode(seed, namespace, subject_id, start, end,
                                                   source, key, control))), collapse = "")
}

# Hashes of one case's controls (the case prefix computed once).
.matchHashes <- function(prefix, controls) {
  vapply(controls, function(k) {
    paste(as.character(openssl::sha256(c(prefix, .matchField(0x01, .matchDecimal(k))))), collapse = "")
  }, "", USE.NAMES = FALSE)
}

# Ranks of decimal strings in numeric order (exact for 64-bit keys).
.matchDecimalRank <- function(x) {
  d <- vapply(x, .matchDecimal, "", USE.NAMES = FALSE)
  neg <- startsWith(d, "-")
  mag <- sub("^-", "", d)
  len <- nchar(mag)
  pos <- which(!neg)
  negs <- which(neg)
  ord <- c(negs[order(len[negs], mag[negs], decreasing = TRUE, method = "radix")],
           pos[order(len[pos], mag[pos], method = "radix")])
  rank <- integer(length(d))
  rank[ord] <- seq_along(ord)
  rank
}

#' The matching order computed in SQL (optional push-down, design 6.3, 6.10)
#'
#' The hexadecimal encoding of E built in SQL and hashed by
#' \code{.sqlSha256Hex(input = "hex")}; equal byte for byte to
#' \code{.matchHash()} (tested on every family that has it). NULL where the
#' family has no SQL SHA-256 or no encoder here: the R order is
#' authoritative and is what the sampler uses.
#'
#' @param h Handle, context, profile or dialect name.
#' @param seed,namespace Plan constants (R values).
#' @param subject,start,end,source,key SQL text expressions of the case
#'   identity (decimal text, ISO date text, text).
#' @param control SQL text expression of the control person id (decimal).
#' @return SQL expression or NULL.
#' @keywords internal
.matchHashSql <- function(h, seed, namespace, subject, start, end, source, key, control) {
  family <- .sqlFamily(h)
  if (!family %in% c("postgresql", "duckdb", "mysql")) return(NULL)
  legacy <- .sqlLegacyHandle(family)
  hexlen <- function(x) switch(family,
    postgresql = paste0("LPAD(TO_HEX(OCTET_LENGTH(", x, ")), 8, '0')"),
    duckdb = paste0("LPAD(LOWER(HEX(STRLEN(", x, "))), 8, '0')"),
    mysql = paste0("LPAD(LOWER(HEX(LENGTH(", x, "))), 8, '0')"))
  hexbytes <- function(x) switch(family,
    postgresql = paste0("ENCODE(CONVERT_TO(", x, ", 'UTF8'), 'hex')"),
    duckdb = paste0("LOWER(HEX(", x, "))"),
    mysql = paste0("LOWER(HEX(", x, "))"))
  field <- function(tag, x) .omopConcatSql(legacy, paste0("'", tag, "'"), hexlen(x), hexbytes(x))
  nullable <- function(tag, x) {
    paste0("CASE WHEN ", x, " IS NULL THEN '0000000000' ELSE ", field(tag, x), " END")
  }
  const <- paste(as.character(c(.matchField(0x02, .MATCH_ENCODING),
                                .matchField(0x01, .matchDecimal(seed)),
                                .matchField(0x02, .matchText(namespace)))), collapse = "")
  enc <- .omopConcatSql(legacy, paste0("'", const, "'"), field("01", subject), field("03", start),
                        nullable("03", end), field("02", source), nullable("02", key),
                        field("01", control))
  .sqlSha256Hex(h, enc, input = "hex")
}

# Netstring serialisation of K_c for the matched index_key: "<bytes>:<value>,"
# per field, "-," for NULL (lossless whatever the values contain).
.matchSerialiseCase <- function(namespace, subject_id, start, end, source, key) {
  fields <- list(.matchText(namespace), .matchDecimal(subject_id), .matchDate(start), .matchDate(end),
                 .matchText(source), .matchText(key))
  paste(vapply(fields, function(x) {
    if (is.null(x)) "-," else paste0(nchar(x, type = "bytes"), ":", x, ",")
  }, ""), collapse = "")
}

# --- settings ---------------------------------------------------------------------------

.matchSpec <- function(node, name) {
  m <- node[["match"]]
  base <- paste0("/populations/", name, "/match")
  one <- function(x) if (is.list(x) && length(x) == 1L && is.null(names(x))) x[[1L]] else x
  enum <- function(field, allowed, default) {
    v <- one(m[[field]] %||% default)
    if (!is.character(v) || length(v) != 1L || !v %in% allowed) {
      .irReject(paste0(field, " must be one of ", paste(allowed, collapse = ", ")),
                field = paste0(base, "/", field))
    }
    v
  }
  int <- function(field, default, minimum) {
    v <- one(m[[field]] %||% default)
    if (!is.numeric(v) || length(v) != 1L || is.na(v) || v != trunc(v) || v < minimum ||
        v > .Machine$integer.max) {
      .irReject(paste0(field, " must be an integer >= ", minimum), field = paste0(base, "/", field))
    }
    as.integer(v)
  }
  at <- one(m[["at"]] %||% "index")
  if (!identical(at, "index")) .irReject("at is index", field = paste0(base, "/at"))
  list(
    cases = one(m[["cases"]]), controls = one(m[["controls"]]), outcome = m[["outcome"]],
    match = .jsonArrayItems(m[["match"]] %||% list()),
    k = int("controls_per_case", 4L, 1L),
    prior_outcome = enum("prior_outcome", c("exclude", "allow"), "exclude"),
    future_cases = enum("future_cases", c("eligible", "ineligible"), "eligible"),
    replacement = enum("replacement", c("between_sets", "none"), "between_sets"),
    seed = int("seed", 1L, 0L),
    insufficient = enum("insufficient", c("keep_available", "drop_case"), "keep_available"),
    missing_strata = enum("missing_strata", c("ineligible", "match_missing"), "ineligible"),
    max_candidates = int("max_candidates", 1000000L, 1L))
}

# The pool cap: min(max_candidates, dsomop.max_memory_rows).
.matchCap <- function(spec) {
  min(as.numeric(spec$max_candidates), as.numeric(.extractionCap("dsomop.max_memory_rows", 1000000L)))
}

# --- bounded private realisation ------------------------------------------------------

#' Fetch a private relation into R with a cap + 1 stop (design 6.10, A-20 j)
#'
#' One statement; at most \code{cap + 1} rows are read. When the extra row
#' arrives the plan is refused with the typed, non-skippable rejection
#' \code{code} (no count in the message); a realisation is never truncated.
#'
#' @param ctx Execution context.
#' @param final Final fragment of the statement.
#' @param select Main SELECT reading it.
#' @param cap Row cap.
#' @param code Status code of the rejection.
#' @param field JSON path naming the construct.
#' @return data.frame (column names lower case; all columns as the driver
#'   returns them).
#' @keywords internal
.matchFetchBounded <- function(ctx, final, select, cap, code = "match_candidates_exceeded",
                               field = NULL) {
  sql <- .buildStatement(ctx, final, select = select, dialect = .compileDialect(ctx))
  n <- as.numeric(cap) + 1
  rows <- .withDbReconnect(ctx[["handle"]], function(conn) {
    res <- DBI::dbSendQuery(conn, sql)
    on.exit(DBI::dbClearResult(res), add = TRUE)
    DBI::dbFetch(res, n = n)
  })
  names(rows) <- tolower(names(rows))
  if (nrow(rows) > cap) {
    .compileReject("the matching realisation exceeds the server cap (max_candidates, dsomop.max_memory_rows)",
                   reason = code, code = code, field = field)
  }
  rows
}

# Text of a column fetched as text (NA stays NA).
.matchChr <- function(x) {
  x <- as.character(x)
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

# --- strata ----------------------------------------------------------------------------

# Strata expressions of one side: sex, age band, calendar year, presence of a
# source in a window anchored at t. `alias` reads the side's rows (index t in
# cohort_start_date), `person` the person-table alias, `roster`/`phase` the
# roster the alias belongs to (for windows and anchors).
.matchStrataSql <- function(ctx, spec, alias, person, roster, phase, pop, state, name) {
  h <- .compileDialect(ctx)
  # a signed year: the MySQL family returns YEAR() unsigned (a future birth year
  # would overflow the subtraction)
  year <- .sqlIntegerCast(h, .sqlYear(h, paste0(alias, ".cohort_start_date")))
  out <- character(0)
  for (i in seq_along(spec$match)) {
    s <- spec$match[[i]]
    path <- paste0("/populations/", name, "/match/match/", i - 1L)
    if (is.character(s) || (is.list(s) && length(s) == 1L && is.null(names(s)))) {
      s <- unlist(s)
      out[[i]] <- switch(s,
        sex = paste0(person, ".gender_concept_id"),
        calendar_year = year,
        .irReject("a match key is sex, calendar_year, {age_band} or {strata}", field = path))
    } else if (!is.null(s[["age_band"]])) {
      w <- as.integer(unlist(s[["age_band"]]))
      if (length(w) != 1L || is.na(w) || w < 1L) .irReject("age_band is an integer >= 1", field = path)
      x <- paste0("(", year, " - ", .sqlIntegerCast(h, paste0(person, ".year_of_birth")), ")")
      wl <- .sqlIntegerLiteral(h, w)
      # floor division for negative ages too: (x - ((x % w) + w) % w) / w
      out[[i]] <- .sqlIntegerCast(h, paste0("((", x, " - (((", x, ") % ", wl, ") + ", wl, ") % ", wl,
                                            ") / ", wl, ")"))
    } else if (!is.null(s[["strata"]])) {
      leaf <- list(source = s[["strata"]][["source"]], op = ">=", n = 1L,
                   window = s[["strata"]][["window"]], nonmissing = NULL, distinct = "row")
      has <- .populationHasSql(ctx, leaf, roster, phase, alias, pop, state, paste0(path, "/strata"))
      out[[i]] <- paste0("CASE WHEN ", has, " THEN 1 WHEN NOT (", has, ") THEN 0 END")
    } else {
      .irReject("a match key is sex, calendar_year, {age_band} or {strata}", field = path)
    }
  }
  out
}

# --- the compiler -------------------------------------------------------------------------

#' Compile a matching population (frozen compiler signature, design 6)
#'
#' @param ctx Execution context.
#' @param node The match population node with its plan name in
#'   \code{attr(node, "name")}.
#' @param cases_roster The cases population's final roster fragment.
#' @param controls_template The controls template population node (with its
#'   name in \code{attr(node, "name")}), instantiated at each case time.
#' @param outcome_fragment The compiled outcome source (complete history).
#' @param cases The cases population result (\code{.compilePopulation()});
#'   looked up in the context when NULL.
#' @param template_parent The template's parent population result, if any.
#' @return \code{list(candidates, roster, coverage, segments, pairs)}: the
#'   template instantiation (candidates of the template population), the
#'   matched roster \code{pop_<id>}, its coverage object, NULL (filled by
#'   \code{.compilePopulation()}) and the private eligible pair relation.
#' @keywords internal
.compileMatch <- function(ctx, node, cases_roster, controls_template, outcome_fragment,
                          cases = NULL, template_parent = NULL) {
  name <- .compileNodeName(node, "population")
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  spec <- .matchSpec(node, name)
  base <- paste0("/populations/", name, "/match")
  cases <- cases %||% .populationMetaGet(ctx, spec$cases)
  if (is.null(cases) || !identical(cases$roster$name, cases_roster$name)) {
    stop("The cases roster of match '", name, "' is not the compiled cases population.", call. = FALSE)
  }
  tpl <- attr(controls_template, "name", exact = TRUE) %||% spec$controls
  if (!identical(tpl, spec$controls) || !identical(controls_template[["kind"]], "criteria")) {
    .irReject("the controls template is a criteria population", field = paste0(base, "/controls"))
  }
  tpl_policy <- .coveragePolicy(controls_template[["coverage"]], paste0("/populations/", tpl, "/coverage"))
  if (!is.null(cases$coverage) && !identical(cases$coverage$policy, tpl_policy)) {
    .irReject("the cases and the controls template must share one coverage policy", field = base)
  }
  tag <- paste0("match:", name)
  rank <- .sqlIntegerLiteral(h, .populationSourceRank(ctx, tag))

  # 1a. the case roster with its visit order (set_rank) and strata ------------------
  state_c <- new.env(parent = emptyenv())
  state_c$joins <- character(0)
  state_c$deps <- cases_roster$name
  state_c$joins[["pp"]] <- paste0("LEFT JOIN ", .populationTable(ctx, "person"),
                                  " pp ON pp.person_id = r.subject_id")
  strata_c <- .matchStrataSql(ctx, spec, "r", "pp", cases_roster, "final", spec$cases, state_c, name)
  set_rank <- .sqlLosslessKey(h, .sqlRowNumber(h, character(0), c(
    .populationOrder(h, "r", "cohort_start_date"), "r.subject_id",
    .populationOrder(h, "r", c("cohort_end_date", .POP_KEY_COLUMNS)))))
  col <- .fragmentColumn
  case_cols <- list(set_rank = col("bigint", FALSE, "episode_key"),
                    case_row_id = col("bigint", FALSE, "private"),
                    subject_id = col("bigint", FALSE, "person_key"),
                    t = col("date", FALSE, "start"), case_end = col("date", TRUE, "end"),
                    index_source = col("text", TRUE, "origin_identity"),
                    index_key = col("text", TRUE, "origin_identity"),
                    index_visit_id = col("bigint", TRUE, "visit"),
                    origin_source = col("text", TRUE, "origin_identity"),
                    origin_key = col("text", TRUE, "origin_identity"),
                    op_start = col("date", TRUE, "start"), op_end = col("date", TRUE, "end"),
                    op_component_id = col("bigint", TRUE, "private"))
  for (i in seq_along(strata_c)) case_cols[[paste0("s_", i)]] <- col("bigint", TRUE, "private")
  case_select <- c(paste0(set_rank, " AS set_rank"), "r.cohort_row_id AS case_row_id",
                   "r.subject_id AS subject_id", "r.cohort_start_date AS t", "r.cohort_end_date AS case_end",
                   "r.index_source AS index_source", "r.index_key AS index_key",
                   "r.index_visit_id AS index_visit_id", "r.origin_source AS origin_source",
                   "r.origin_key AS origin_key", "r.op_start AS op_start",
                   "r.op_end AS op_end", "r.op_component_id AS op_component_id",
                   if (length(strata_c)) paste0(vapply(strata_c, function(x) .sqlLosslessKey(h, x), ""),
                                                " AS s_", seq_along(strata_c)))
  f_cases <- .fragment(.fragmentName("mt", name, "cases", ctx = ctx), "stream", "episode",
                       paste0("SELECT ", paste(case_select, collapse = ", "), " FROM ", cases_roster$name,
                              " r ", paste(unname(state_c$joins), collapse = " "),
                              " WHERE r.cohort_start_date IS NOT NULL"),
                       columns = case_cols, dependencies = unique(state_c$deps), unique_key = "set_rank",
                       person_lineage = list(person_col = "subject_id", via_roster = NULL),
                       roster_namespace = spec$cases,
                       private_columns = c("case_row_id", "index_key", "index_visit_id", "origin_source", "origin_key",
                                           "op_component_id",
                                           if (length(strata_c)) paste0("s_", seq_along(strata_c))))
  .sqlAssertTypedSql(h, f_cases$select_body)
  # A mixed union may contain unindexed persons. Filter them before ranking
  # and freezing: only episodes with a defined case time enter the sampler.
  # Both bounded fetches below must use the same case identities and strata
  # even if the underlying CDM changes between statements.
  f_cases <- .populationFreezeFragment(ctx, f_cases)
  .fragmentRegister(ctx, f_cases)

  # 1b. the template instantiated at each case time (A-20 c) --------------------------
  op <- .coverageSource(ctx)
  conds <- c("p.person_id <> c.subject_id")
  deps <- c(f_cases$name, op$name)
  scope <- .populationScopeFragment(ctx)
  if (!is.null(scope)) {
    conds <- c(conds, paste0("EXISTS (SELECT 1 FROM ", scope$name, " sp WHERE sp.person_id = p.person_id)"))
    deps <- c(deps, scope$name)
  }
  if (!is.null(template_parent)) {
    conds <- c(conds, paste0("EXISTS (SELECT 1 FROM ", template_parent$roster$name,
                             " tp WHERE tp.subject_id = p.person_id)"))
    deps <- c(deps, template_parent$roster$name)
  }
  seed_cohort <- controls_template[["cohort_definition_id"]]
  if (is.list(seed_cohort)) seed_cohort <- unlist(seed_cohort)
  if (!is.null(seed_cohort)) {
    conds <- c(conds, paste0("EXISTS (SELECT 1 FROM ", .populationCohortTable(ctx), " sc WHERE ",
                             "sc.cohort_definition_id = ", .sqlIntegerLiteral(h, as.integer(seed_cohort)),
                             " AND sc.subject_id = p.person_id)"))
  }
  # the case's set rank and strata travel with its instantiation rows, so the
  # pair relation never reads the case roster a second time
  strata_cols <- if (length(strata_c)) paste0("mt_s_", seq_along(strata_c)) else character(0)
  carry_q <- c("c.set_rank AS set_rank", "c.t AS t",
               if (length(strata_c)) paste0("c.s_", seq_along(strata_c), " AS ", strata_cols))
  pairs_q <- paste0("SELECT DISTINCT ", paste(carry_q, collapse = ", "), ", ", .sqlLosslessKey(h, "p.person_id"),
                    " AS person_id FROM ", f_cases$name, " c INNER JOIN ", op$name,
                    " p ON p.start_date <= c.t AND p.end_date >= c.t WHERE ", paste(conds, collapse = " AND "))
  inst_key <- .sqlLosslessKey(h, .sqlRowNumber(h, character(0), c("q.set_rank", "q.person_id")))
  inst_over <- c(list(subject_id = "q.person_id", cohort_start_date = "q.t", cohort_end_date = "q.t",
                      index_source = .compileTextLiteral(tag), source_rank = rank),
                 .populationNulls(h, c("index_key", "index_visit_id", "origin_source", "origin_key",
                                       "set_id", "is_case", "sampling_weight",
                                       "op_start", "op_end", "op_component_id", "key_1", "key_2", "key_3")))
  inst_over$origin_source <- .compileTextLiteral("(persons)")
  inst_extra <- c(list(mt_set = col("bigint", FALSE, "private")),
                  stats::setNames(lapply(strata_cols, function(x) col("bigint", TRUE, "private")), strata_cols))
  carry_inst <- c("mt_set", strata_cols)
  inst_body <- paste0("SELECT ", inst_key, " AS cand_key, ", paste(.populationSelect("q", inst_over), collapse = ", "),
                      ", q.set_rank AS mt_set",
                      if (length(strata_cols)) paste0(", ", paste(paste0("q.", strata_cols, " AS ", strata_cols), collapse = ", ")),
                      " FROM (", pairs_q, ") q")
  f_raw <- .populationCandidateFragment(ctx, tpl, "raw", inst_body, deps, indexed = TRUE, extra = inst_extra)
  cov_t <- .compileCoverage(ctx, tpl, tpl_policy, f_raw, "candidate")
  .coverageAssertUnambiguous(ctx, cov_t, tpl)
  cand_over <- list(op_start = "o.op_start", op_end = "o.op_end", op_component_id = "o.component_id")
  f_cand <- .populationCandidateFragment(
    ctx, tpl, NULL,
    paste0("SELECT r.cand_key AS cand_key, ", paste(.populationSelect("r", cand_over), collapse = ", "),
           ", ", paste(paste0("r.", carry_inst, " AS ", carry_inst), collapse = ", "), " FROM ", f_raw$name,
           " r INNER JOIN ", cov_t$coverage$name, " o ON o.row_key = r.cand_key WHERE o.op_count = 1"),
    c(f_raw$name, cov_t$coverage$name), indexed = TRUE, extra = inst_extra)
  f_crit <- .populationCriteriaFragment(ctx, tpl, controls_template[["where"]], f_cand, f_raw, tpl_policy,
                                        carry = carry_inst)

  # 1c. the eligible pair relation -------------------------------------------------------
  state_k <- new.env(parent = emptyenv())
  state_k$joins <- character(0)
  state_k$deps <- f_crit$name
  state_k$joins[["pk"]] <- paste0("LEFT JOIN ", .populationTable(ctx, "person"),
                                  " pk ON pk.person_id = x.subject_id")
  strata_k <- .matchStrataSql(ctx, spec, "x", "pk", f_cand, "candidate", tpl, state_k, name)
  # Bind observation to this template's selected component, never its gap
  # envelope or another coverage request's component numbering (A-20 f).
  where <- c("x.crit_true = 1", paste0(
    "EXISTS (SELECT 1 FROM ", cov_t$recorded$name,
    " mr WHERE mr.person_id = x.subject_id AND mr.component_id = x.op_component_id",
    " AND mr.start_date <= x.cohort_start_date AND mr.end_date >= x.cohort_start_date)"))
  state_k$deps <- c(state_k$deps, cov_t$recorded$name)
  death <- .roleOf(handle, "death")
  if (!is.null(death)) {
    where <- c(where, paste0("NOT EXISTS (SELECT 1 FROM ", .populationTable(ctx, "death"), " d WHERE d.",
                             death[["person"]], " = x.subject_id AND d.", death[["start"]],
                             " < x.cohort_start_date)"))
  }
  if (identical(outcome_fragment$availability, "unavailable")) {
    where <- c(where, .populationUnknownSql(h))
  } else {
    oc <- if (identical(spec$prior_outcome, "exclude")) "o.start_date <= x.cohort_start_date" else
      "o.start_date = x.cohort_start_date"
    if (identical(spec$future_cases, "ineligible")) oc <- paste0("(", oc, " OR o.start_date > x.cohort_start_date)")
    where <- c(where, paste0("NOT EXISTS (SELECT 1 FROM ", outcome_fragment$name, " o WHERE o.person_id = ",
                             "x.subject_id AND ", oc, ")"))
    state_k$deps <- c(state_k$deps, outcome_fragment$name)
  }
  for (i in seq_along(strata_k)) {
    a <- paste0("x.mt_s_", i)
    b <- .sqlLosslessKey(h, strata_k[[i]])
    where <- c(where, if (identical(spec$missing_strata, "match_missing")) {
      paste0("(", a, " = ", b, " OR (", a, " IS NULL AND ", b, " IS NULL))")
    } else paste0(a, " = ", b))
  }
  pair_cols <- list(set_rank = col("bigint", FALSE, "episode_key"),
                    cand_key = col("bigint", FALSE, "private"),
                    subject_id = col("bigint", FALSE, "person_key"),
                    op_start = col("date", TRUE, "start"), op_end = col("date", TRUE, "end"),
                    op_component_id = col("bigint", TRUE, "private"))
  pair_body <- paste0("SELECT x.mt_set AS set_rank, x.cand_key AS cand_key, x.subject_id AS subject_id, ",
                      "x.op_start AS op_start, x.op_end AS op_end, x.op_component_id AS op_component_id FROM ",
                      f_crit$name, " x ", paste(unname(state_k$joins), collapse = " "), " WHERE ",
                      paste(where, collapse = " AND "))
  f_pairs <- .fragment(.fragmentName("mt", name, "pairs", ctx = ctx), "stream", "episode", pair_body,
                       columns = pair_cols, dependencies = unique(state_k$deps),
                       unique_key = c("set_rank", "subject_id"),
                       person_lineage = list(person_col = "subject_id", via_roster = NULL),
                       private_columns = c("set_rank", "cand_key", "op_component_id"))
  .sqlAssertTypedSql(h, f_pairs$select_body)
  .fragmentRegister(ctx, f_pairs)

  # 2. one capped realisation of the cases and of the pool --------------------------------
  txt <- function(x) .sqlLosslessKeyText(h, x)
  dtxt <- function(x) .sqlDateText(h, x)
  case_rows <- .matchFetchBounded(ctx, f_cases, paste0(
    "SELECT ", txt("c.set_rank"), " AS set_rank, ", txt("c.subject_id"), " AS subject_id, ",
    dtxt("c.t"), " AS t, ", dtxt("c.case_end"), " AS case_end, c.index_source AS index_source, ",
    "c.index_key AS index_key, ", txt("c.index_visit_id"), " AS index_visit_id, ",
    "c.origin_source AS origin_source, c.origin_key AS origin_key, ",
    dtxt("c.op_start"), " AS op_start, ", dtxt("c.op_end"), " AS op_end, ",
    txt("c.op_component_id"), " AS op_component_id FROM ", f_cases$name, " c"),
    .extractionCap("dsomop.max_memory_rows", 1000000L), field = base)
  pool <- .matchFetchBounded(ctx, f_pairs, paste0(
    "SELECT ", txt("x.set_rank"), " AS set_rank, ", txt("x.subject_id"), " AS subject_id, ",
    dtxt("x.op_start"), " AS op_start, ", dtxt("x.op_end"), " AS op_end, ",
    txt("x.op_component_id"), " AS op_component_id FROM ", f_pairs$name, " x"),
    .matchCap(spec), field = base)

  # 3-5. order, selection, weights (pure R) -------------------------------------------------
  rows <- .matchSelect(spec, name, case_rows, pool)

  # 6. the matched roster -------------------------------------------------------------------
  f_rows <- .matchMaterialise(ctx, name, rows, tag)
  roster <- .populationRosterFragment(ctx, name, .rankedRosterSql(h, f_rows$name), f_rows$name, indexed = TRUE)
  covs <- Filter(Negate(is.null), list(cases$coverage, cov_t))
  cov <- .populationDerivedCoverage(ctx, name, covs, roster)
  list(candidates = f_cand, roster = roster, coverage = cov, segments = NULL, pairs = f_pairs)
}

#' Deterministic order, selection and weights over a realised pool
#'
#' Pure R (design section 6.10 steps 3-5): every case in \code{(t, K_c)}
#' order (\code{set_rank}), zero-pair cases included; controls ordered by
#' \code{(SHA-256(E), person id)}; \code{between_sets} / \code{none} with
#' allocation rollback of a dropped case; weights.
#'
#' @param spec Match settings (\code{.matchSpec()}).
#' @param name Match population id (\code{"match:<name>"} namespace).
#' @param cases data.frame(set_rank, subject_id, t, case_end, index_source,
#'   index_key, index_visit_id, origin_source, origin_key, op_start, op_end,
#'   op_component_id) as text.
#' @param pool data.frame(set_rank, subject_id, op_start, op_end,
#'   op_component_id) as text.
#' @return data.frame of matched rows: subject_id, t, set_id, role
#'   (0 case, 1 control), sampling_weight, index_key, op_start, op_end,
#'   op_component_id, index_visit_id, origin_source, origin_key.
#' @keywords internal
.matchSelect <- function(spec, name, cases, pool) {
  cases <- cases[order(as.numeric(cases$set_rank)), , drop = FALSE]
  by_case <- split(seq_len(nrow(pool)), .matchChr(pool$set_rank))
  used <- new.env(parent = emptyenv())
  out <- list()
  ns <- spec$cases
  for (i in seq_len(nrow(cases))) {
    cs <- cases[i, , drop = FALSE]
    idx <- by_case[[.matchChr(cs$set_rank)]] %||% integer(0)
    controls <- .matchChr(pool$subject_id[idx])
    n_c <- length(controls)
    chosen <- integer(0)
    if (n_c) {
      prefix <- .matchCasePrefix(spec$seed, ns, cs$subject_id, cs$t, .matchChr(cs$case_end),
                                 .matchChr(cs$index_source), .matchChr(cs$index_key))
      h <- .matchHashes(prefix, controls)
      ord <- order(h, .matchDecimalRank(controls), method = "radix")
      if (identical(spec$replacement, "between_sets")) {
        chosen <- ord[seq_len(min(spec$k, n_c))]
      } else {
        free <- ord[!vapply(controls[ord], function(k) exists(k, envir = used, inherits = FALSE), logical(1))]
        chosen <- free[seq_len(min(spec$k, length(free)))]
      }
    }
    m_c <- length(chosen)
    if (m_c < spec$k && identical(spec$insufficient, "drop_case")) next
    if (identical(spec$replacement, "none")) {
      for (k in controls[chosen]) assign(k, TRUE, envir = used)
    }
    # The analytic set key is the complete case-roster rank. Dropping an
    # insufficient case leaves a hole; later retained sets keep their keys.
    set_id <- as.integer(cs$set_rank)
    kc <- .matchSerialiseCase(ns, cs$subject_id, cs$t, .matchChr(cs$case_end), .matchChr(cs$index_source),
                              .matchChr(cs$index_key))
    out[[length(out) + 1L]] <- data.frame(
      subject_id = .matchDecimal(cs$subject_id), t = cs$t, set_id = set_id, role = 0L,
      sampling_weight = 1, index_key = paste0(kc, "|case"),
      index_visit_id = .matchChr(cs$index_visit_id), origin_source = .matchChr(cs$origin_source),
      origin_key = .matchChr(cs$origin_key), op_start = .matchChr(cs$op_start),
      op_end = .matchChr(cs$op_end), op_component_id = .matchChr(cs$op_component_id),
      stringsAsFactors = FALSE)
    if (m_c) {
      w <- if (identical(spec$replacement, "between_sets")) n_c / m_c else NA_real_
      out[[length(out) + 1L]] <- data.frame(
        subject_id = vapply(controls[chosen], .matchDecimal, "", USE.NAMES = FALSE), t = cs$t,
        set_id = set_id, role = 1L, sampling_weight = w, index_key = paste0(kc, "|control"),
        index_visit_id = NA_character_, origin_source = NA_character_, origin_key = NA_character_,
        op_start = .matchChr(pool$op_start[idx][chosen]), op_end = .matchChr(pool$op_end[idx][chosen]),
        op_component_id = .matchChr(pool$op_component_id[idx][chosen]), stringsAsFactors = FALSE)
    }
  }
  if (!length(out)) {
    return(data.frame(subject_id = character(0), t = character(0), set_id = integer(0), role = integer(0),
                      sampling_weight = numeric(0), index_key = character(0), index_visit_id = character(0),
                      origin_source = character(0), origin_key = character(0), op_start = character(0),
                      op_end = character(0), op_component_id = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# Materialise the realised matched rows once (a session temporary table, or a
# literal relation on the MySQL family) as the fragment mt_<pop>_rows.
.matchMaterialise <- function(ctx, name, rows, tag) {
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  spec <- .populationColumnSpec(TRUE)
  cols <- .POP_ROSTER_COLUMNS
  types <- vapply(spec[cols], function(c) c$type, "")
  rank <- .populationSourceRank(ctx, tag)
  value <- function(col, i) {
    v <- switch(col,
      subject_id = rows$subject_id[[i]], cohort_start_date = rows$t[[i]], cohort_end_date = rows$t[[i]],
      index_source = tag, index_key = rows$index_key[[i]], index_visit_id = rows$index_visit_id[[i]],
      origin_source = rows$origin_source[[i]], origin_key = rows$origin_key[[i]],
      set_id = rows$set_id[[i]], is_case = if (rows$role[[i]] == 0L) 1L else 0L,
      sampling_weight = rows$sampling_weight[[i]], op_start = rows$op_start[[i]],
      op_end = rows$op_end[[i]], op_component_id = rows$op_component_id[[i]], source_rank = rank,
      key_1 = rows$set_id[[i]], key_2 = rows$role[[i]], key_3 = NA)
    if (length(v) != 1L || is.na(v)) return(.compileTypedNull(h, types[[col]]))
    switch(types[[col]],
      date = .sqlDateLiteral(h, v), text = .compileTextLiteral(as.character(v)),
      integer = , bigint = .sqlIntegerLiteral(h, if (is.character(v)) v else as.integer(v)),
      numeric = .predicateNumericLiteral(as.numeric(v)))
  }
  # text columns of the materialised table are unbounded where the family has
  # such a type (a nested case identity can exceed 255 characters)
  nulls <- vapply(cols, function(col) {
    if (identical(types[[col]], "text") && .sqlFamily(h) %in% c("postgresql", "duckdb")) {
      return("CAST(NULL AS TEXT)")
    }
    .compileTypedNull(h, types[[col]])
  }, "")
  empty <- paste0("SELECT ", paste(paste0(nulls, " AS ", cols), collapse = ", "), .compileNoTableFrom(h),
                  " WHERE 1 = 0")
  if (identical(.sqlFamily(h), "mysql")) {
    body <- if (!nrow(rows)) empty else paste(vapply(seq_len(nrow(rows)), function(i) {
      paste0("SELECT ", paste(paste0(vapply(cols, value, "", i = i), " AS ", cols), collapse = ", "))
    }, ""), collapse = " UNION ALL ")
  } else {
    .sqlRequire(h, "session_table")
    base <- substr(gsub("[^a-z0-9_]", "_", tolower(paste0("dsomop_mt_", name))), 1L, 40L)
    tname <- .reserveTempTableName(handle, base)
    # Register ownership before CREATE. On an error release this operation's
    # table immediately; failed DROP remains owned for context-close retry.
    owned <- ctx[["compile_temp_tables"]] %||% character(0)
    assign("compile_temp_tables", unique(c(owned, tname)), envir = ctx)
    tryCatch({
      .createTempTable(handle, tname, empty)
      if (nrow(rows)) {
        for (from in seq(1L, nrow(rows), by = 500L)) {
          idx <- seq.int(from, min(nrow(rows), from + 499L))
          values <- vapply(idx, function(i) {
            paste0("(", paste(vapply(cols, value, "", i = i), collapse = ", "), ")")
          }, "")
          .withDbReconnect(handle, function(conn) DBI::dbExecute(conn, paste0(
            "INSERT INTO ", tname, " (", paste(cols, collapse = ", "), ") VALUES ",
            paste(values, collapse = ", "))))
        }
      }
    }, error = function(e) {
      cleanup <- tryCatch({
        .compileReleaseTempTables(ctx, tname)
        NULL
      }, error = identity)
      if (!is.null(cleanup)) {
        e$message <- paste(conditionMessage(e), conditionMessage(cleanup), sep = "\n")
        e$cleanup_error <- cleanup
      }
      stop(e)
    })
    body <- paste0("SELECT ", paste(paste0("t.", cols, " AS ", cols), collapse = ", "), " FROM ", tname, " t")
  }
  f <- .fragment(.fragmentName("mt", name, "rows", ctx = ctx), "candidates", "episode", body,
                 columns = spec, unique_key = c("subject_id", "cohort_start_date", "key_1", "key_2"),
                 person_lineage = list(person_col = "subject_id", via_roster = NULL),
                 source_identity_columns = c("index_source", "subject_id", "cohort_start_date",
                                             "cohort_end_date", "index_key"),
                 private_columns = .POP_PRIVATE_COLUMNS)
  attr(f, "population") <- name
  .sqlAssertTypedSql(h, f$select_body)
  .fragmentRegister(ctx, f)
}
