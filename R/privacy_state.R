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
.DSOMOP_PRIVACY_GUARANTEE <-
  "sticky_person_bounded_discrete_laplace_per_release_v1"
.DSOMOP_DP_CONTRACT <- "fixed_per_release_seed_data_versioned_v1"
.DSOMOP_DP_BINDING <- "seed_data_versioned_v1"
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

# Clinical tables whose largest identifier tracks database reloads. The
# lookups use the primary-key index, so they stay cheap on large databases.
.DSOMOP_DP_VERSIONED_TABLES <- c(
  person = "person_id", observation_period = "observation_period_id",
  visit_occurrence = "visit_occurrence_id", visit_detail = "visit_detail_id",
  condition_occurrence = "condition_occurrence_id",
  drug_exposure = "drug_exposure_id",
  procedure_occurrence = "procedure_occurrence_id",
  device_exposure = "device_exposure_id", measurement = "measurement_id",
  observation = "observation_id", note = "note_id", specimen = "specimen_id",
  condition_era = "condition_era_id", drug_era = "drug_era_id",
  dose_era = "dose_era_id", payer_plan_period = "payer_plan_period_id",
  cost = "cost_id", episode = "episode_id"
)

#' Private automatic database version of the DP answers
#'
#' The largest identifier of every present clinical table, keyed with the
#' protected subkey: a reload changes it, so every answer re-draws at once and
#' nobody can tell which statistics changed. It is never released. A read
#' failure stops the release rather than producing a different version. A
#' custodian who configures \code{dsomop.dp.snapshot_id} declares versions
#' explicitly, and then no database is read.
#'
#' @param handle CDM handle, or NULL.
#' @param key Protected subkey.
#' @param custodial_snapshot Logical; the custodian configured the snapshot.
#' @return Hex version string.
#' @keywords internal
.dsomopDpDataVersion <- function(handle, key, custodial_snapshot = FALSE) {
  if (isTRUE(custodial_snapshot)) {
    return(.dsomopDpHmac(key, "dsomop-dp-data-version-v1/custodial-snapshot"))
  }
  if (is.null(handle)) {
    stop("DP releases need the connected OMOP resource (or a custodial ",
         "dsomop.dp.snapshot_id).", call. = FALSE)
  }
  present <- tolower(.listTablesRaw(handle, handle$cdm_schema))
  tables <- sort(intersect(names(.DSOMOP_DP_VERSIONED_TABLES), present),
                 method = "radix")
  values <- lapply(stats::setNames(tables, tables), function(table) {
    column <- .DSOMOP_DP_VERSIONED_TABLES[[table]]
    sql <- .sql_translate(paste0("SELECT MAX(", column, ") AS m FROM ",
                                 .qualifyTable(handle, table)),
                          handle$target_dialect)
    value <- .executeQuery(handle, sql)$m[[1L]]
    if (is.null(value) || is.na(value)) "" else
      format(value, scientific = FALSE, trim = TRUE)
  })
  .dsomopDpHmac(key, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-data-version-v1", tables = values
  )))
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

#' Resolve the server-owned DP release policy
#'
#' Missing domain and snapshot_id values are SHA-256 fingerprints of canonical
#' resource coordinates and cdm_source metadata (cdm_source_name,
#' cdm_release_date, cdm_version, vocabulary_version). Explicit values win.
#' Complete source rows are canonically sorted; incomplete or absent source
#' metadata falls back to sorted vocabulary versions, then resource identity
#' alone. Data reloads need no epoch bump: the private database version of
#' every answer tracks them (\code{.dsomopDpDataVersion}), and a custodial
#' snapshot_id replaces that automatic version. The noise root is the only
#' persistent DP state; ephemeral state directories are test-only.
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
  snapshot_id <- .dsomopDpString(.dsomopDpOption("snapshot_id", ""),
                                 "snapshot_id", required = FALSE)
  custodial_snapshot <- nzchar(snapshot_id)
  if (!nzchar(domain) || !nzchar(snapshot_id)) {
    derived <- .dsomopDpDerivedIdentity(state$dp_handle,
                                        derive_snapshot = !nzchar(snapshot_id))
    if (!nzchar(domain)) domain <- derived$domain
    if (!nzchar(snapshot_id)) snapshot_id <- derived$snapshot_id
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$", domain)) {
    stop("dsomop.dp.domain contains unsupported characters.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]{0,255}$", snapshot_id)) {
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
    custodial_snapshot = custodial_snapshot,
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
  config$keys <- list(
    provenance = .dsomopDpSubkey(root$key, config$domain,
                                 "person-local-provenance/v1"),
    query = .dsomopDpSubkey(root$key, config$domain,
                            "semantic-query-id/v1"),
    protected = .dsomopDpSubkey(root$key, config$domain,
                                "protected-fingerprint/v1"),
    noise = .dsomopDpSubkey(root$key, config$domain, "noise/v1")
  )
  # Nothing but the root persists: the private database version is read from
  # the data once per R session.
  config$data_version <- .dsomopDpDataVersion(
    state$dp_handle, config$keys$protected,
    custodial_snapshot = config$custodial_snapshot
  )
  state$dp_runtime <- list(
    enabled = TRUE, policy_hash = config$policy_hash, policy = config
  )
  config
}

.dsomopDpNoisyInteger <- function(value, policy, release_context, component,
                                  epsilon, sensitivity, lower = 0,
                                  upper = 2^53 - 1) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value != floor(value) || value < 0 || value > 2^53 - 1) {
    stop("A bounded DP integer statistic is not exactly representable.",
         call. = FALSE)
  }
  context <- list(
    release = release_context,
    mechanism = policy$mechanism,
    sampler = policy$sampler,
    privacy_epoch = policy$privacy_epoch,
    allocation = list(epsilon = epsilon, delta = 0,
                      sensitivity = sensitivity),
    component = component
  )
  noise <- .dsomopDpDiscreteLaplace(
    policy$keys$noise, context, coordinate = 1L,
    epsilon = epsilon, sensitivity = sensitivity
  )
  # Add and clip while still exact, including noise beyond the R integer range.
  # Only the bounded result is converted to the existing public numeric type.
  noisy <- .dsomopBigAdd(value, noise)
  if (.dsomopBigCompare(noisy, lower) < 0L) return(lower)
  if (.dsomopBigCompare(noisy, upper) > 0L) return(upper)
  .dsomopBigNumeric(noisy)
}

.dsomopDpSemanticId <- function(policy, semantic) {
  .dsomopDpHmac(policy$keys$query, .dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_CANONICAL_PROTOCOL,
    semantic = semantic
  )))
}

.dsomopDpSnapshotBinding <- function(policy, bounded_snapshot) {
  json <- .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-bounded-sufficient-statistic-v1",
    value = bounded_snapshot
  ))
  list(
    snapshot_id = .dsomopDpPublicSnapshotId(policy),
    protected_fingerprint = .dsomopDpHmac(policy$keys$protected, paste0(
      "dsOMOP/dp/protected-fingerprint/v1\u001f", json
    ))
  )
}

.dsomopDpPublicSnapshotId <- function(policy) {
  .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-public-snapshot-v1",
    domain = policy$domain,
    snapshot_id = policy$snapshot_id
  )))
}

.dsomopDpReleaseId <- function(policy, semantic, snapshot) {
  .dsomopDpHmac(policy$keys$query, .dsomopDpCanonicalJson(list(
    protocol = policy$release_protocol,
    domain = policy$domain,
    policy_hash = policy$policy_hash,
    snapshot_id = snapshot$snapshot_id,
    protected_fingerprint = snapshot$protected_fingerprint,
    privacy_epoch = policy$privacy_epoch,
    data_version = policy$data_version,
    semantic = semantic
  )))
}

.dsomopDpRelease <- function(policy, semantic, bounded_snapshot,
                             sensitivity, payload_fn) {
  if (!is.function(payload_fn)) stop("payload_fn must be a function.",
                                     call. = FALSE)
  semantic <- list(
    protocol = policy$protocol,
    mechanism = policy$mechanism,
    privacy_contract = policy$privacy_contract,
    adjacency = policy$adjacency,
    sensitivity = sensitivity,
    statistic = semantic
  )
  # Deterministic in the root, the question, the protected statistic and the
  # private database version: repeating a request returns the same answer
  # without any stored state.
  release <- function() {
    semantic_query_id <- .dsomopDpSemanticId(policy, semantic)
    snapshot <- .dsomopDpSnapshotBinding(policy, bounded_snapshot)
    release_id <- .dsomopDpReleaseId(policy, semantic, snapshot)
    release_context <- list(
      release_id = release_id,
      semantic_query_id = semantic_query_id,
      snapshot_id = snapshot$snapshot_id,
      protected_fingerprint = snapshot$protected_fingerprint,
      policy_hash = policy$policy_hash
    )
    value <- payload_fn(
      epsilon = policy$release_epsilon,
      policy = policy,
      release_context = release_context
    )
    reserved <- c(
      "protocol", "mechanism", "adjacency", "epsilon", "delta",
      "sensitivity", "privacy_contract", "sticky", "sampler"
    )
    if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
        anyDuplicated(names(value)) ||
        any(c("noise_root", "seed", "raw_noise") %in% names(value)) ||
        any(reserved %in% names(value))) {
      stop("The DP mechanism returned an invalid payload.", call. = FALSE)
    }
    payload <- c(value, list(
      protocol = policy$protocol,
      mechanism = policy$mechanism,
      adjacency = policy$adjacency,
      epsilon = policy$release_epsilon,
      delta = policy$release_delta,
      sensitivity = sensitivity,
      privacy_contract = policy$privacy_contract,
      sticky = TRUE,
      sampler = policy$sampler
    ))
    jsonlite::fromJSON(
      .dsomopDpCanonicalJson(payload), simplifyVector = TRUE
    )
  }
  release()
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
    release_epsilon = policy$release_epsilon,
    release_delta = policy$release_delta,
    privacy_epoch = policy$privacy_epoch,
    max_levels = policy$max_levels,
    max_contributions = policy$max_contributions,
    numeric_grid = policy$numeric_grid,
    history_dependent = FALSE,
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
