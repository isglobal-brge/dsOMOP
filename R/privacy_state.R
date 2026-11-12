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
         "configure dsomop.dp.domain explicitly or provide unambiguous ",
         "persistent resource coordinates.", call. = FALSE)
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
    # Presence through the catalogue first: a failing SELECT would abort the
    # request's read snapshot on PostgreSQL and DuckDB (read_snapshot.R).
    schema <- if (table == "vocabulary") handle$vocab_schema %||%
      handle$cdm_schema else handle$cdm_schema
    if (!table %in% tolower(.listTablesRaw(handle, schema))) {
      stop("Missing metadata table")
    }
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
  }
  # A display label only (dsOMOP 2.10.3): no key, hash or admission reads it.
  list(domain = domain,
       snapshot_id = paste0("cdm_", .dsomopDpSha256(.dsomopDpCanonicalJson(list(
         protocol = protocol, resource = resource_id, metadata = metadata
       )))))
}

#' Resolve the server-owned DP release policy
#'
#' A missing domain is the SHA-256 fingerprint of the canonical resource
#' coordinates. The public \code{snapshot_id} is display metadata only, for
#' clients that print it (the SHA-256 of the resource identity and the
#' cdm_source metadata: cdm_source_name, cdm_release_date, cdm_version,
#' vocabulary_version, with the documented fallbacks): since dsOMOP 2.10.3 it
#' enters no key, policy hash, provenance check, route or admission. The
#' deprecated \code{dsomop.dp.snapshot_id} option only replaces that label.
#' Fixed synopses are keyed on the per-request person-membership generation
#' (\code{.dsomopDpDataGeneration}), which needs no configuration. The noise
#' root is the only persistent DP state; ephemeral state directories are
#' test-only.
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
  # Deprecated: a display label only (dsOMOP 2.10.3). A malformed custodial
  # label is ignored rather than refused, so it can never block the service.
  snapshot_id <- tryCatch(.dsomopDpString(.dsomopDpOption("snapshot_id", ""),
                                          "snapshot_id", required = FALSE),
                          error = function(e) "")
  if (nzchar(snapshot_id) &&
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]{0,255}\\z", snapshot_id,
             perl = TRUE)) {
    snapshot_id <- ""
  }
  if (!nzchar(domain)) {
    derived <- .dsomopDpDerivedIdentity(state$dp_handle,
                                        derive_snapshot = TRUE)
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
    # The snapshot label is display metadata: it never enters the hash, so
    # changing or emptying it can change no admission (review R01).
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
           "changing the privacy domain, epoch, calibration or caps.",
           call. = FALSE)
    }
    policy <- runtime$policy
    # The display label follows the current configuration; nothing else
    # reads it.
    policy$snapshot_id <- config$snapshot_id
    return(policy)
  }

  root <- .dsomopDpNoiseRoot()
  config$noise_root <- root
  # The provenance MAC key; the release keys (person tokens, contribution
  # fingerprints, noise) are derived per use in tools_release.R. Nothing but
  # the root persists: the person-membership generation of the fixed
  # synopses is recomputed inside each request's read snapshot and never
  # retained (privacy_generation.R).
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
