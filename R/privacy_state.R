# Sticky differential-privacy state: the secret noise root, nothing else.
#
# Every answer is a deterministic function of the root, the canonical
# question, the protected statistic and a private automatic database version
# (dsOMOP 2.10.0). Answers are identical after restarts and on every R server
# that shares the root, and a database reload re-draws every answer at once.
# No answers are stored. Provenance admission precedes every release.

.DSOMOP_DP_PROTOCOL <- "dsomop-dp-release-v3"
.DSOMOP_DP_CANONICAL_PROTOCOL <- "dsomop-dp-canonical-json-v1"
.DSOMOP_DP_RELEASE_PROTOCOL <- "dsomop-dp-semantic-release-v1"
.DSOMOP_DP_MECHANISM <- "dsomop-sticky-discrete-laplace-prf-v2"
.DSOMOP_DP_SAMPLER <- "dsomop-dp-exact-discrete-laplace-v1"
# dsOMOP 2.10.3 withdraws the per-release claim: answers are deterministic in
# a fixed public basis (fixed_synopsis) or in secret fingerprints of the
# realised bounded contributions (realised_sticky, heuristic).
.DSOMOP_PRIVACY_GUARANTEE <-
  "fixed_synopsis_or_heuristic_realised_contribution_reuse_v1"
.DSOMOP_DP_CONTRACT <- "protected_release_fixed_basis_or_realised_v1"
.DSOMOP_DP_BINDING <- "fixed_basis_or_realised_contributions_v1"
.DSOMOP_DP_STATE_ERROR <-
  "The DP release state is unavailable; contact the data custodian."

.dsomopDpHex <- function(value) {
  if (!is.raw(value)) stop("Cannot encode non-raw DP state.", call. = FALSE)
  paste(format(value), collapse = "")
}

.dsomopDpSha256 <- function(value) {
  if (!is.raw(value)) value <- charToRaw(enc2utf8(value))
  .dsomopDpHex(as.raw(openssl::sha256(value)))
}

.dsomopDpHmacRaw <- function(key, value) {
  if (!is.raw(key) || length(key) != 32L) {
    stop("The DP root is unavailable.", call. = FALSE)
  }
  if (!is.raw(value)) value <- charToRaw(enc2utf8(value))
  as.raw(openssl::sha256(value, key = key))
}

.dsomopDpHmac <- function(key, value) {
  .dsomopDpHex(.dsomopDpHmacRaw(key, value))
}

.dsomopDpCanonicalValue <- function(value) {
  if (is.null(value)) return(NULL)
  if (inherits(value, "Date")) {
    if (anyNA(value)) {
      stop("A canonical DP query cannot contain missing dates.", call. = FALSE)
    }
    return(format(value, "%Y-%m-%d"))
  }
  if (is.object(value)) {
    stop("A canonical DP query contains an unsupported value type.",
         call. = FALSE)
  }
  if (is.list(value)) {
    fields <- names(value)
    if (!is.null(fields)) {
      if (anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields)) {
        stop("A canonical DP query contains invalid object fields.",
             call. = FALSE)
      }
      value <- value[order(fields, method = "radix")]
    }
    return(lapply(value, .dsomopDpCanonicalValue))
  }
  if (!typeof(value) %in% c("logical", "integer", "double", "character")) {
    stop("A canonical DP query contains an unsupported value type.",
         call. = FALSE)
  }
  if (!is.null(names(value))) {
    stop("Canonical DP query vectors must be unnamed.", call. = FALSE)
  }
  if (anyNA(value) || (is.numeric(value) && any(!is.finite(value)))) {
    stop("A canonical DP query contains a missing or non-finite value.",
         call. = FALSE)
  }
  if (is.character(value)) return(enc2utf8(unname(value)))
  if (is.numeric(value)) {
    value <- unname(as.numeric(value))
    value[value == 0] <- 0
    return(value)
  }
  unname(value)
}

.dsomopDpCanonicalJson <- function(value) {
  value <- .dsomopDpCanonicalValue(value)
  as.character(jsonlite::toJSON(
    value, auto_unbox = TRUE, null = "null", na = "null", digits = 17,
    POSIXt = "ISO8601", UTC = TRUE, pretty = FALSE
  ))
}

.dsomopDpCanonicalSelfTest <- function() {
  observed <- .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_CANONICAL_PROTOCOL,
    method = "bounded_histogram",
    arguments = list(levels = c("a", "b"), cap = 1L, zero = -0),
    version = 1L
  )))
  expected <- "d27cf24db74bd4ceed8fa8700fa195b59a11316e092a572e75f4058ddd6e42b9"
  if (!identical(observed, expected)) {
    stop("The canonical DP encoder changed; sticky releases are disabled ",
         "until an explicit protocol version is installed.", call. = FALSE)
  }
  sampler_observed <- .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-sampler-kat-v2",
    sampler = .DSOMOP_DP_SAMPLER,
    noise = vapply(seq_len(9L), function(i) .dsomopBigDecimal(
      .dsomopDpDiscreteLaplace(
        as.raw(0:31),
        list(protocol = "dsomop-sampler-kat-v2", query = "fixed",
             component = "count"),
        coordinate = i,
        epsilon = c(0.1, 0.25, 1, 8, 0.1, 8, 1e-6, 0.25, 1)[[i]],
        sensitivity = c(1, 3, 1, 1, 65535, 3, 2^24 - 1,
                        2^53 - 1, 2^300)[[i]]
      )
    ), character(1L))
  )))
  sampler_expected <-
    "e5cbb32edc915648286b5bc06b802a6dfd49226fac0cd97cc88043b2918095f9"
  if (!identical(sampler_observed, sampler_expected)) {
    stop("The sticky-noise sampler changed; releases are disabled until an ",
         "explicit sampler protocol version is installed.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Read custodial DP options
#'
#' Since 2.6.0, the DP release channel is enabled by default. Explicit options
#' and DSOMOP_DP_* environment values must agree. Set dsomop.dp.enabled = FALSE
#' or DSOMOP_DP_ENABLED=0 to opt out. Empty domain/snapshot defaults are derived
#' from a connected resource by .dsomopDpPolicyConfig().
#' @param name DP setting name without its option prefix.
#' @param default Fallback when no custodial setting is present.
#' @return The configured setting or fallback.
#' @keywords internal
.dsomopDpOption <- function(name, default = NULL) {
  option <- paste0("dsomop.dp.", name)
  configured <- getOption(option, NULL)
  environment_names <- c(
    enabled = "DSOMOP_DP_ENABLED",
    domain = "DSOMOP_DP_DOMAIN",
    snapshot_id = "DSOMOP_DP_SNAPSHOT_ID",
    release_epsilon = "DSOMOP_DP_RELEASE_EPSILON",
    privacy_epoch = "DSOMOP_DP_PRIVACY_EPOCH",
    max_levels = "DSOMOP_DP_MAX_LEVELS",
    max_contributions = "DSOMOP_DP_MAX_CONTRIBUTIONS",
    numeric_grid = "DSOMOP_DP_NUMERIC_GRID"
  )
  environment_name <- unname(environment_names[name])
  if (length(environment_name) != 1L || is.na(environment_name)) {
    environment_name <- NULL
  }
  raw <- if (is.null(environment_name)) "" else
    Sys.getenv(environment_name, unset = "")
  if (!nzchar(raw)) {
    if (!is.null(configured)) return(configured)
    return(getOption(paste0("default.", option), default))
  }
  numeric_names <- c(
    "release_epsilon", "privacy_epoch", "max_levels",
    "max_contributions", "numeric_grid"
  )
  environment <- if (name %in% numeric_names) {
    suppressWarnings(as.numeric(raw))
  } else raw
  if (!is.null(configured)) {
    same <- if (identical(name, "enabled")) {
      identical(.dsomopDpBoolean(configured, name),
                .dsomopDpBoolean(environment, name))
    } else if (name %in% numeric_names) {
      is.numeric(configured) && length(configured) == 1L &&
        !is.na(configured) && identical(as.numeric(configured), environment)
    } else {
      is.character(configured) && length(configured) == 1L &&
        !is.na(configured) && identical(enc2utf8(configured), enc2utf8(raw))
    }
    if (!isTRUE(same)) {
      stop("Conflicting DP option and ", environment_name,
           " environment values are configured.", call. = FALSE)
    }
  }
  environment
}

.dsomopDpBoolean <- function(value, name) {
  .dsomopParsePseudonymBoolean(value, paste0("dsomop.dp.", name))
}

.dsomopDpNumber <- function(value, name, lower, upper = Inf,
                            integer = FALSE, lower_open = FALSE) {
  valid <- is.numeric(value) && length(value) == 1L && !is.na(value) &&
    is.finite(value) && value <= upper &&
    if (lower_open) value > lower else value >= lower
  if (valid && integer) valid <- value == floor(value)
  if (!isTRUE(valid)) {
    stop("dsomop.dp.", name, " has an invalid server-owned value.",
         call. = FALSE)
  }
  if (integer) as.integer(value) else as.numeric(value)
}

.dsomopDpString <- function(value, name, required = TRUE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      (required && !nzchar(value))) {
    stop("dsomop.dp.", name, " must be one ",
         if (required) "non-empty " else "", "string.", call. = FALSE)
  }
  enc2utf8(value)
}

.dsomopDpEnabled <- function() {
  state <- .dsomopDpState()
  enabled <- .dsomopDpBoolean(.dsomopDpOption("enabled", TRUE), "enabled")
  runtime <- state$dp_runtime
  if (is.list(runtime) && !identical(runtime$enabled, enabled)) {
    stop("DP enablement changed during this R session; restart the session ",
         "after changing server privacy configuration.", call. = FALSE)
  }
  enabled
}

.coerceDsomopDpRoot <- function(value, label = "DP noise root") {
  if (is.raw(value) && length(value) == 32L) return(value)
  if (is.character(value) && length(value) == 1L && !is.na(value) &&
      grepl("^[0-9a-fA-F]{64}$", value)) {
    return(.coerceDsomopSecret(value, label))
  }
  stop("The ", label, " must be exactly 32 raw CSPRNG bytes or 64 ",
       "hexadecimal characters.", call. = FALSE)
}

.dsomopDpRootId <- function(root) {
  paste0("dpk_", substr(.dsomopDpSha256(root), 1L, 40L))
}

# The root is created on first use and never replaced: losing it re-draws every
# answer, so a damaged root fails closed. A custodian-supplied root writes
# nothing and lets several R servers give identical answers.
.dsomopDpNoiseRoot <- function() {
  resolve <- function() {
    environment <- Sys.getenv("DSOMOP_DP_NOISE_ROOT", unset = "")
    configured <- getOption(
      "dsomop.dp.noise_root",
      getOption("default.dsomop.dp.noise_root", NULL)
    )
    environment_root <- if (nzchar(environment)) {
      .coerceDsomopDpRoot(environment)
    } else NULL
    configured_root <- if (!is.null(configured)) {
      .coerceDsomopDpRoot(configured)
    } else NULL
    if (!is.null(environment_root) && !is.null(configured_root) &&
        !identical(environment_root, configured_root)) {
      stop("Conflicting DP noise roots are configured.", call. = FALSE)
    }
    injected <- environment_root %||% configured_root
    if (!is.null(injected)) {
      return(list(
        key = injected, key_id = .dsomopDpRootId(injected),
        provider = "custodial_injected", material_exposed = FALSE
      ))
    }
    # The reason goes to the server log; analysts see one generic error.
    created <- FALSE
    root <- tryCatch({
      path <- .dsomopSecretPath("dp_noise_root")
      created <- !file.exists(path) && !.dsomopIsSymlink(path)
      .ensureDsomopSecret(
        "dp_noise_root", path = path, require_existing = FALSE,
        replace_invalid = FALSE
      )
    }, error = function(e) {
      message("[dsomop-dp-state] Custodian recovery required: ",
              conditionMessage(e))
      stop(structure(list(message = .DSOMOP_DP_STATE_ERROR, call = NULL),
                     class = c("dsomop_dp_state_error", "error", "condition")))
    })
    if (created) {
      message("[dsomop-dp-state] Custodian: created the DP noise root. Keep ",
              "the dsOMOP state directory on persistent storage: every DP ",
              "answer re-draws if the root is lost.")
    }
    list(
      key = root, key_id = .dsomopDpRootId(root),
      provider = "persistent_file", material_exposed = FALSE
    )
  }
  resolve()
}

.dsomopDpSubkey <- function(root, domain, purpose) {
  .dsomopDpHmacRaw(root, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-subkey-v1",
    domain = domain,
    purpose = purpose
  )))
}

# Keep DSLite's independent server sessions separate within its shared process.
# Secrets remain in package-local state, never in a serializable OMOP handle.
.dsomopDpState <- function() {
  for (frame in rev(sys.frames())) {
    name <- attr(frame, "name", exact = TRUE)
    if (is.character(name) && length(name) == 1L && !is.na(name) &&
        grepl("^DSLiteEnv_[0-9]{4}$", name)) {
      if (is.null(.pkg_state$dp_sessions)) {
        .pkg_state$dp_sessions <- new.env(parent = emptyenv())
      }
      if (is.null(.pkg_state$dp_sessions[[name]])) {
        .pkg_state$dp_sessions[[name]] <- new.env(parent = emptyenv())
      }
      return(.pkg_state$dp_sessions[[name]])
    }
  }
  .pkg_state
}

.dsomopDpDerivedIdentity <- function(handle, derive_snapshot = TRUE) {
  unavailable <- function() {
    stop("Cannot derive DP identity from the connected resource; ",
         "configure dsomop.dp.domain and dsomop.dp.snapshot_id explicitly ",
         "or provide unambiguous persistent resource coordinates.", call. = FALSE)
  }
  if (is.null(handle)) unavailable()
  # Only database coordinates are admitted; URL credentials, user names and
  # connection/security options cannot enter public hashes or rotate noise.
  identity <- tryCatch({
    parsed <- handle$resource_client$getParsed()
    database <- parsed$database
    if (!is.character(database) || length(database) != 1L ||
        is.na(database) || !nzchar(database)) stop("Missing database")
    dialect <- .normalizeDBMS(parsed$dbms)
    if (dialect %in% c("sqlite", "duckdb")) {
      if (database == ":memory:") stop("No persistent database identity")
      database <- normalizePath(path.expand(database), winslash = "/",
                                mustWork = TRUE)
    }
    host <- tolower(parsed$host %||% "")
    if (grepl("[@/?#]", host) ||
        (!dialect %in% c("sqlite", "duckdb") && !nzchar(host))) {
      stop("Ambiguous database host")
    }
    default_ports <- c(postgresql = 5432L, redshift = 5439L,
                       mysql = 3306L, mariadb = 3306L, sqlserver = 1433L,
                       synapse = 1433L, pdw = 1433L, oracle = 1521L,
                       snowflake = 443L)
    port <- parsed$port
    if (is.null(port) && dialect %in% names(default_ports)) {
      port <- unname(default_ports[dialect])
    }
    schema <- function(value) {
      if (dialect == "sqlite") "" else value %||% ""
    }
    list(protocol = "dsomop-dp-resource-v1", dbms = dialect,
         host = host, port = port, database = database,
         cdm_schema = schema(handle$cdm_schema),
         vocabulary_schema = schema(handle$vocab_schema))
  }, error = function(e) NULL)
  if (is.null(identity)) unavailable()
  resource_id <- .dsomopDpSha256(.dsomopDpCanonicalJson(identity))
  domain <- paste0("resource_", resource_id)
  if (!derive_snapshot) return(list(domain = domain))
  read_rows <- function(table, fields) {
    sql <- .renderSql(handle, "SELECT * FROM @qualified",
                      qualified = .qualifyTable(handle, table, schema =
                        if (table == "vocabulary") handle$vocab_schema else NULL))
    rows <- .withDbReconnect(handle, function(conn) DBI::dbGetQuery(conn, sql))
    names(rows) <- tolower(names(rows))
    if (!nrow(rows) || !all(fields %in% names(rows))) stop("Invalid metadata")
    values <- lapply(rows[fields], function(value) {
      value <- as.character(value)
      if (anyNA(value) || any(!nzchar(trimws(value)))) stop("Missing metadata")
      enc2utf8(value)
    })
    rows <- lapply(seq_len(nrow(rows)), function(i) lapply(values, `[`, i))
    canonical <- vapply(rows, .dsomopDpCanonicalJson, character(1))
    rows[order(canonical, method = "radix")]
  }
  metadata <- tryCatch(read_rows("cdm_source", c(
    "cdm_source_name", "cdm_release_date", "cdm_version", "vocabulary_version"
  )), error = function(e) NULL)
  if (!is.null(metadata)) {
    protocol <- if (length(metadata) == 1L) {
      metadata <- metadata[[1L]]
      "dsomop-dp-cdm-snapshot-v1"
    } else "dsomop-dp-cdm-snapshot-multi-v1"
  } else {
    metadata <- tryCatch(read_rows("vocabulary", c(
      "vocabulary_id", "vocabulary_version"
    )), error = function(e) NULL)
    protocol <- if (is.null(metadata)) {
      "dsomop-dp-cdm-snapshot-resource-only-v1"
    } else "dsomop-dp-cdm-snapshot-fallback-v1"
    # Reloads are tracked by the private database version of every answer.
  }
  list(domain = domain,
       snapshot_id = paste0("cdm_", .dsomopDpSha256(.dsomopDpCanonicalJson(list(
         protocol = protocol, resource = resource_id, metadata = metadata
       )))))
}

# --- Automatic data generation of the fixed synopses (dsOMOP 2.10.3) ----------

# OMOP clinical tables of the CDM schema whose exact row count and largest
# identifier make up the automatic data generation. NA: the table has no single
# identifier column, so its exact row count alone enters.
.DSOMOP_DP_GENERATION_TABLES <- c(
  person = "person_id", observation_period = "observation_period_id",
  visit_occurrence = "visit_occurrence_id", visit_detail = "visit_detail_id",
  condition_occurrence = "condition_occurrence_id",
  drug_exposure = "drug_exposure_id",
  procedure_occurrence = "procedure_occurrence_id",
  device_exposure = "device_exposure_id", measurement = "measurement_id",
  observation = "observation_id", death = "person_id", note = "note_id",
  note_nlp = "note_nlp_id", specimen = "specimen_id",
  fact_relationship = NA_character_, location = "location_id",
  care_site = "care_site_id", provider = "provider_id",
  payer_plan_period = "payer_plan_period_id", cost = "cost_id",
  drug_era = "drug_era_id", dose_era = "dose_era_id",
  condition_era = "condition_era_id", episode = "episode_id",
  episode_event = "episode_id", metadata = "metadata_id",
  cdm_source = NA_character_
)

# Public description of the automatic generation and of what it cannot see
# (status and fixed-synopsis labels).
.DSOMOP_DP_GENERATION_SOURCE <- "automatic_session_fingerprint"
.DSOMOP_DP_GENERATION_BASIS <- paste0(
  "HMAC under the noise root of the exact row count and largest identifier ",
  "of every present OMOP clinical table, the cdm_source rows, the vocabulary ",
  "versions and the concept table's row count and largest identifier; ",
  "derived once per R session from the connected database, held in memory ",
  "only and never released")
.DSOMOP_DP_GENERATION_RESIDUAL <- paste0(
  "An in-place edit that changes no row count and no largest identifier of ",
  "these tables, and a data change made while an R session is open, leave ",
  "that session's generation unchanged. The fixed-synopsis components whose ",
  "bounded contributions changed still redraw one by one (their noise is ",
  "keyed on the realised contributions too), so no exact difference is ",
  "released, but comparing answers from before and after such an edit can ",
  "show which components changed (not by how much). Custodians who edit data ",
  "in place can redraw every fixed synopsis by changing ",
  "dsomop.dp.privacy_epoch.")

# Exact canonical text of one aggregate or metadata value (integer,
# integer64, double or text); NULL aggregates of empty tables are "". An
# aggregate (exact = TRUE) must be an exact integer; a metadata double keeps
# all 17 significant digits.
.dsomopDpGenerationText <- function(x, exact = TRUE) {
  if (length(x) != 1L) stop("A generation aggregate is malformed.")
  if (inherits(x, "integer64")) return(as.character(x))
  if (is.na(x)) return("")
  if (is.integer(x)) return(format(x, scientific = FALSE, trim = TRUE))
  if (is.numeric(x)) {
    if (!isTRUE(exact)) return(sprintf("%.17g", x))
    if (!is.finite(x) || x != floor(x)) {
      stop("A generation aggregate is not an exact integer.")
    }
    return(sprintf("%.0f", x))
  }
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC"))
  }
  enc2utf8(as.character(x))
}

#' Read the exact data-state snapshot of the automatic generation
#'
#' Per clinical table of \code{.DSOMOP_DP_GENERATION_TABLES}: presence, the
#' exact \code{COUNT(*)} and the \code{MAX()} of its identifier (when the
#' column exists); the complete \code{cdm_source} rows; the vocabulary versions
#' and the \code{concept} count and largest identifier of the vocabulary
#' schema. Every value is exact text, so equal data give equal snapshots on
#' every driver and worker. Errors propagate (the caller makes the generation
#' unavailable): there is no partial or fallback snapshot.
#'
#' @param handle CDM handle.
#' @return List of exact character values (never released).
#' @keywords internal
.dsomopDpGenerationSnapshot <- function(handle) {
  count_max <- function(table, column, schema = NULL) {
    columns <- tolower(.listColumnsRaw(handle, table, schema = schema)$column_name)
    use_max <- !is.na(column) && column %in% columns
    sql <- paste0("SELECT COUNT(*) AS n",
                  if (use_max) paste0(", MAX(", column, ") AS m"),
                  " FROM ", .qualifyTable(handle, table, schema = schema))
    row <- .executeQuery(handle, .sql_translate(sql, handle$target_dialect))
    if (nrow(row) != 1L) stop("A generation aggregate is malformed.")
    list(n = .dsomopDpGenerationText(row$n[[1L]]),
         max = if (use_max) .dsomopDpGenerationText(row$m[[1L]]) else NULL)
  }
  rows_of <- function(table, schema = NULL) {
    rows <- .executeQuery(handle, .sql_translate(
      paste0("SELECT * FROM ", .qualifyTable(handle, table, schema = schema)),
      handle$target_dialect))
    if (!nrow(rows)) return(character(0))
    fields <- sort(names(rows), method = "radix")
    text <- vapply(seq_len(nrow(rows)), function(i) {
      .dsomopFrame(stats::setNames(lapply(fields, function(f) {
        .dsomopDpGenerationText(rows[[f]][[i]], exact = FALSE)
      }), fields))
    }, character(1L))
    sort(text, method = "radix")
  }
  present <- unique(tolower(.listTablesRaw(handle, handle$cdm_schema)))
  tables <- lapply(names(.DSOMOP_DP_GENERATION_TABLES), function(table) {
    if (!table %in% present) return(list(present = FALSE))
    c(list(present = TRUE),
      count_max(table, .DSOMOP_DP_GENERATION_TABLES[[table]]),
      if (identical(table, "cdm_source")) list(rows = rows_of(table)))
  })
  names(tables) <- names(.DSOMOP_DP_GENERATION_TABLES)
  vocab_schema <- handle$vocab_schema %||% handle$cdm_schema
  vocab_present <- unique(tolower(.listTablesRaw(handle, vocab_schema)))
  vocabulary <- list(
    vocabulary = if ("vocabulary" %in% vocab_present) {
      rows <- .executeQuery(handle, .sql_translate(paste0(
        "SELECT vocabulary_id, vocabulary_version FROM ",
        .qualifyTable(handle, "vocabulary", schema = vocab_schema)),
        handle$target_dialect))
      sort(vapply(seq_len(nrow(rows)), function(i) {
        .dsomopFrame(
          .dsomopDpGenerationText(rows$vocabulary_id[[i]], exact = FALSE),
          .dsomopDpGenerationText(rows$vocabulary_version[[i]], exact = FALSE))
      }, character(1L)), method = "radix")
    } else NULL,
    concept = if ("concept" %in% vocab_present) {
      count_max("concept", "concept_id", schema = vocab_schema)
    } else NULL
  )
  list(tables = tables, vocabulary = vocabulary)
}

#' Automatic data generation of the fixed synopses
#'
#' A keyed fingerprint of the connected CDM's data state
#' (\code{.dsomopDpGenerationSnapshot}), \code{HMAC(K_generation,
#' frame(protocol, snapshot))} with a subkey of the noise root, so it needs no
#' custodial configuration. It is derived on the fly the first time an R
#' session needs it for a handle and held in that session's memory only: never
#' written to disk, a table or a cache file, and never released in payloads,
#' labels, errors, status or logs. Equal data give the same generation in
#' every session and on every R server sharing the noise root; a reload that
#' adds or removes rows of any clinical table changes it, and with it the noise
#' of every fixed synopsis. Any read failure makes the generation unavailable
#' for that session (the fixed route is then not used, and DP-only mode
#' refuses); there is no partial or fallback value, so a transient error can
#' never yield a second generation of the same data. What it cannot see is
#' \code{.DSOMOP_DP_GENERATION_RESIDUAL}.
#'
#' @param handle CDM handle (the request's).
#' @param policy DP policy (noise root and corpus).
#' @return List: \code{available}, \code{id} (64 hex; internal only).
#' @keywords internal
.dsomopDpDataGeneration <- function(handle, policy) {
  state <- .dsomopDpState()
  key_id <- policy$noise_root$key_id %||% ""
  cache <- state$dp_generations %||% list()
  for (entry in cache) {
    if (identical(entry$handle, handle) && identical(entry$key_id, key_id) &&
        identical(entry$domain, policy$domain)) {
      return(entry$value)
    }
  }
  value <- tryCatch({
    if (!is.environment(handle)) stop("No connected OMOP resource.")
    root <- policy$noise_root$key
    if (!is.raw(root) || length(root) != 32L) stop("No noise root.")
    snapshot <- .dsomopDpGenerationSnapshot(handle)
    key <- .dsomopDpSubkey(root, policy$domain, "data-generation/v1")
    list(available = TRUE,
         id = .dsomopDpHmac(key, .dsomopFrame(
           "dsomop-data-generation-v1", snapshot$tables,
           snapshot$vocabulary)))
  }, error = function(e) {
    message("[dsomop-dp-generation] The automatic data generation could not ",
            "be derived; this R session releases no fixed synopses: ",
            conditionMessage(e))
    list(available = FALSE, id = NA_character_)
  })
  # Session memory only; a handful of handles per session at most.
  state$dp_generations <- utils::tail(c(cache, list(list(
    handle = handle, key_id = key_id, domain = policy$domain,
    value = value))), 16L)
  value
}

#' Resolve the server-owned DP release policy
#'
#' A missing domain is the SHA-256 fingerprint of the canonical resource
#' coordinates. The public \code{snapshot_id} is only a display label for
#' clients (the SHA-256 of the resource identity and the cdm_source metadata:
#' cdm_source_name, cdm_release_date, cdm_version, vocabulary_version, with the
#' documented fallbacks); since dsOMOP 2.10.3 no key, route or admission
#' depends on it. The deprecated \code{dsomop.dp.snapshot_id} option only
#' replaces that label. Fixed synopses are keyed on the automatic data
#' generation instead (\code{.dsomopDpDataGeneration}), which needs no
#' configuration. The noise root is the only persistent DP state; ephemeral
#' state directories are test-only.
#' @keywords internal
.dsomopDpPolicyConfig <- function() {
  state <- .dsomopDpState()
  enabled <- .dsomopDpEnabled()
  if (!enabled) {
    return(list(
      enabled = FALSE, protocol = .DSOMOP_DP_PROTOCOL,
      mechanism = .DSOMOP_DP_MECHANISM
    ))
  }
  domain <- .dsomopDpString(.dsomopDpOption("domain", ""), "domain",
                            required = FALSE)
  # Deprecated: a display label only (dsOMOP 2.10.3).
  snapshot_id <- .dsomopDpString(.dsomopDpOption("snapshot_id", ""),
                                 "snapshot_id", required = FALSE)
  if (!nzchar(domain)) {
    derived <- .dsomopDpDerivedIdentity(state$dp_handle,
                                        derive_snapshot = !nzchar(snapshot_id))
    domain <- derived$domain
    if (!nzchar(snapshot_id)) snapshot_id <- derived$snapshot_id
  } else if (!nzchar(snapshot_id)) {
    # The label never blocks the service: without derivable resource
    # coordinates it is the fingerprint of the custodial domain.
    derived <- tryCatch(.dsomopDpDerivedIdentity(state$dp_handle),
                        error = function(e) NULL)
    snapshot_id <- derived$snapshot_id %||% paste0("cdm_", .dsomopDpSha256(
      .dsomopFrame("dsomop-dp-snapshot-label-v1", domain)))
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}\\z", domain, perl = TRUE)) {
    stop("dsomop.dp.domain contains unsupported characters.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]{0,255}\\z", snapshot_id,
             perl = TRUE)) {
    stop("dsomop.dp.snapshot_id contains unsupported characters.",
         call. = FALSE)
  }
  policy <- list(
    enabled = TRUE,
    schema_version = 4L,
    protocol = .DSOMOP_DP_PROTOCOL,
    canonical_protocol = .DSOMOP_DP_CANONICAL_PROTOCOL,
    release_protocol = .DSOMOP_DP_RELEASE_PROTOCOL,
    mechanism = .DSOMOP_DP_MECHANISM,
    sampler = .DSOMOP_DP_SAMPLER,
    privacy_contract = .DSOMOP_DP_CONTRACT,
    adjacency = "add_remove_person",
    domain = domain,
    snapshot_id = snapshot_id,
    release_epsilon = .dsomopDpNumber(
      .dsomopDpOption("release_epsilon", 0.1), "release_epsilon", 1e-6, 8
    ),
    release_delta = 0,
    privacy_epoch = as.numeric(.dsomopDpNumber(
      .dsomopDpOption("privacy_epoch", 1), "privacy_epoch", 1,
      .Machine$integer.max, integer = TRUE
    )),
    max_levels = .dsomopDpNumber(
      .dsomopDpOption("max_levels", 1000), "max_levels", 1, 100000,
      integer = TRUE
    ),
    max_contributions = .dsomopDpNumber(
      .dsomopDpOption("max_contributions", 10), "max_contributions", 1,
      10000, integer = TRUE
    ),
    numeric_grid = .dsomopDpNumber(
      .dsomopDpOption("numeric_grid", 65535), "numeric_grid", 255,
      2^24 - 1, integer = TRUE
    )
  )
  policy$policy_hash <- .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    schema_version = policy$schema_version,
    protocol = policy$protocol,
    canonical_protocol = policy$canonical_protocol,
    release_protocol = policy$release_protocol,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    privacy_contract = policy$privacy_contract,
    adjacency = policy$adjacency,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    release_epsilon = policy$release_epsilon,
    release_delta = policy$release_delta,
    privacy_epoch = policy$privacy_epoch,
    max_levels = policy$max_levels,
    max_contributions = policy$max_contributions,
    numeric_grid = policy$numeric_grid
  )))
  policy
}

.dsomopDpPolicy <- function(require_enabled = TRUE) {
  state <- .dsomopDpState()
  if (!is.list(state$dp_runtime) &&
      !isTRUE(state$dp_bootstrap_in_progress)) {
    .dsomopDpEnsureRuntime()
  }
  config <- .dsomopDpPolicyConfig()
  if (isTRUE(require_enabled) && !isTRUE(config$enabled)) {
    stop("Differential-privacy releases are disabled by the data custodian.",
         call. = FALSE)
  }
  if (!isTRUE(config$enabled)) return(config)

  runtime <- state$dp_runtime
  if (is.list(runtime)) {
    if (!identical(runtime$policy_hash, config$policy_hash)) {
      stop("DP policy changed during this R session; restart the session after ",
           "changing the snapshot or privacy parameters.", call. = FALSE)
    }
    return(runtime$policy)
  }

  root <- .dsomopDpNoiseRoot()
  config$noise_root <- root
  # The provenance MAC key; the release keys (person tokens, contribution
  # fingerprints, noise) are derived per use in tools_release.R. Nothing but
  # the root persists: the automatic data generation of the fixed synopses is
  # derived per R session and held in memory (.dsomopDpDataGeneration).
  config$keys <- list(
    provenance = .dsomopDpSubkey(root$key, config$domain,
                                 "person-local-provenance/v1")
  )
  state$dp_runtime <- list(
    enabled = TRUE, policy_hash = config$policy_hash, policy = config
  )
  config
}

.dsomopDpDormantStatus <- function() {
  list(
    enabled = NA, ready = FALSE, sticky_noise = FALSE,
    bootstrap = "pending_first_sticky_use",
    protocol = .DSOMOP_DP_PROTOCOL,
    mechanism = .DSOMOP_DP_MECHANISM
  )
}

.dsomopDpPublicStatus <- function(initialize = TRUE) {
  if (!.dsomopDpEnabled()) {
    return(list(
      enabled = FALSE, ready = FALSE, sticky_noise = FALSE,
      protocol = .DSOMOP_DP_PROTOCOL,
      mechanism = .DSOMOP_DP_MECHANISM
    ))
  }
  policy <- if (isTRUE(initialize)) .dsomopDpPolicy() else
    .dsomopDpPolicyConfig()
  status <- list(
    enabled = TRUE,
    ready = isTRUE(initialize),
    sticky_noise = TRUE,
    protocol = policy$protocol,
    canonical_protocol = policy$canonical_protocol,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    privacy_guarantee = .DSOMOP_PRIVACY_GUARANTEE,
    privacy_contract = policy$privacy_contract,
    adjacency = policy$adjacency,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    data_generation = .DSOMOP_DP_GENERATION_SOURCE,
    data_generation_basis = .DSOMOP_DP_GENERATION_BASIS,
    data_generation_residual = .DSOMOP_DP_GENERATION_RESIDUAL,
    release_epsilon = policy$release_epsilon,
    release_delta = policy$release_delta,
    privacy_epoch = policy$privacy_epoch,
    max_levels = policy$max_levels,
    max_contributions = policy$max_contributions,
    numeric_grid = policy$numeric_grid,
    history_dependent = FALSE,
    history_based_admission = FALSE,
    privacy_call_quota = "none",
    persistent_state = "noise_root_only",
    release_binding = .DSOMOP_DP_BINDING,
    service_capacity = "none"
  )
  if (!isTRUE(initialize)) return(status)
  status$noise_key_id <- policy$noise_root$key_id
  status$noise_provider <- policy$noise_root$provider
  status$noise_domain_id <- paste0(
    "dpn_",
    substr(.dsomopDpSha256(.dsomopDpCanonicalJson(list(
      protocol = "dsomop-dp-noise-domain-v1",
      noise_key_id = policy$noise_root$key_id
    ))), 1L, 40L)
  )
  status
}

.dsomopDpBootstrap <- function() {
  state <- .dsomopDpState()
  .dsomopDpCanonicalSelfTest()
  status <- .dsomopDpPublicStatus(initialize = .dsomopDpEnabled())
  if (!isTRUE(status$enabled)) {
    state$dp_runtime <- list(enabled = FALSE)
  }
  state$dp_status <- status
  status
}

.dsomopDpEnsureRuntime <- function(handle = NULL) {
  state <- .dsomopDpState()
  previous_handle <- state$dp_handle
  committed <- FALSE
  if (!is.null(handle)) state$dp_handle <- handle
  on.exit({
    if (!committed) state$dp_handle <- previous_handle
  }, add = TRUE)
  if (is.list(state$dp_runtime)) {
    .dsomopDpPolicy(require_enabled = FALSE)
    committed <- TRUE
    return(invisible(state$dp_status))
  }
  if (isTRUE(state$dp_bootstrap_in_progress)) {
    stop("Recursive DP service bootstrap was detected.", call. = FALSE)
  }
  state$dp_bootstrap_in_progress <- TRUE
  on.exit(state$dp_bootstrap_in_progress <- FALSE, add = TRUE)
  status <- .dsomopDpBootstrap()
  committed <- TRUE
  invisible(status)
}

#' Deprecated: dsOMOP stores no DP answers
#'
#' dsOMOP 2.7.1 to 2.9.x kept a permanent first-answer store that custodians
#' could initialize with this function. Since 2.10.0 every DP answer is a
#' deterministic function of the secret noise root, the question and the data,
#' so there is no store to initialize. The function only signals that.
#' Existing \code{release-bindings} files are no longer read.
#'
#' @param store_id Ignored.
#' @return \code{NA_character_}, invisibly.
#' @export
omopInitializeReleaseStore <- function(store_id = NULL) {
  .Deprecated(msg = paste0(
    "dsOMOP 2.10.0 stores no DP answers: they are deterministic in the ",
    "noise root, the question and the data, so there is no release store to ",
    "initialize."
  ))
  invisible(NA_character_)
}
