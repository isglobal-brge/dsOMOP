# Sticky differential-privacy state.
#
# Privacy releases have one deliberately small state contract: a persistent
# 256-bit custodial root. No release history is stored or consulted. HMAC
# domain separation derives independent keys for provenance, semantic identity,
# protected-snapshot binding, and noise.  Recomputing an identical release is
# therefore sticky without memoization or database state.

.DSOMOP_DP_PROTOCOL <- "dsomop-dp-release-v2"
.DSOMOP_DP_CANONICAL_PROTOCOL <- "dsomop-dp-canonical-json-v1"
.DSOMOP_DP_RELEASE_PROTOCOL <- "dsomop-dp-semantic-release-v1"
.DSOMOP_DP_MECHANISM <- "dsomop-sticky-discrete-laplace-prf-v1"
.DSOMOP_DP_SAMPLER <- "hmac-inverse-cdf-52bit-v1"
.DSOMOP_PRIVACY_GUARANTEE <-
  "sticky_person_bounded_discrete_laplace_per_release_v1"
.DSOMOP_DP_CONTRACT <- "fixed_per_release_semantic_prf_v1"

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
    protocol = "dsomop-dp-sampler-kat-v1",
    noise = .dsomopDpDiscreteLaplace(
      as.raw(0:31),
      list(protocol = "dsomop-sampler-kat-v1", query = "fixed",
           component = "count"),
      coordinate = 7L, epsilon = 0.25, sensitivity = 3
    )
  )))
  sampler_expected <-
    "f41d43840fc89a32939f8413d825b14d5900a13ae80e0f37b35111294e4acaf0"
  if (!identical(sampler_observed, sampler_expected)) {
    stop("The sticky-noise sampler changed; releases are disabled until an ",
         "explicit sampler protocol version is installed.", call. = FALSE)
  }
  invisible(TRUE)
}

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
  enabled <- .dsomopDpBoolean(.dsomopDpOption("enabled", FALSE), "enabled")
  runtime <- .pkg_state$dp_runtime
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

.dsomopDpNoiseRoot <- function() {
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
  root <- .ensureDsomopSecret("dp_noise_root", replace_invalid = TRUE)
  list(
    key = root, key_id = .dsomopDpRootId(root),
    provider = "persistent_file", material_exposed = FALSE
  )
}

.dsomopDpSubkey <- function(root, domain, purpose) {
  .dsomopDpHmacRaw(root, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-subkey-v1",
    domain = domain,
    purpose = purpose
  )))
}

.dsomopDpPolicyConfig <- function() {
  enabled <- .dsomopDpEnabled()
  if (!enabled) {
    return(list(
      enabled = FALSE, protocol = .DSOMOP_DP_PROTOCOL,
      mechanism = .DSOMOP_DP_MECHANISM
    ))
  }
  domain <- .dsomopDpString(.dsomopDpOption("domain", ""), "domain")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$", domain)) {
    stop("dsomop.dp.domain contains unsupported characters.", call. = FALSE)
  }
  snapshot_id <- .dsomopDpString(
    .dsomopDpOption("snapshot_id", ""), "snapshot_id"
  )
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._:@+-]{0,255}$", snapshot_id)) {
    stop("dsomop.dp.snapshot_id contains unsupported characters.",
         call. = FALSE)
  }
  policy <- list(
    enabled = TRUE,
    schema_version = 2L,
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
  if (!is.list(.pkg_state$dp_runtime) &&
      !isTRUE(.pkg_state$dp_bootstrap_in_progress)) {
    .dsomopDpEnsureRuntime()
  }
  config <- .dsomopDpPolicyConfig()
  if (isTRUE(require_enabled) && !isTRUE(config$enabled)) {
    stop("Differential-privacy releases are disabled by the data custodian.",
         call. = FALSE)
  }
  if (!isTRUE(config$enabled)) return(config)

  runtime <- .pkg_state$dp_runtime
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
  .pkg_state$dp_runtime <- list(
    enabled = TRUE, policy_hash = config$policy_hash, policy = config
  )
  config
}

.dsomopDpUniform <- function(key, context, coordinate, draw) {
  digest <- .dsomopDpHmacRaw(key, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-dp-hmac-stream-v1",
    context = context,
    coordinate = as.numeric(coordinate),
    draw = as.numeric(draw)
  )))
  bytes <- as.integer(digest[seq_len(7L)])
  high48 <- 0
  for (byte in bytes[seq_len(6L)]) high48 <- high48 * 256 + byte
  integer52 <- high48 * 16 + floor(bytes[[7L]] / 16)
  (integer52 + 1) / (2^52 + 1)
}

.dsomopDpDiscreteLaplace <- function(key, context, coordinate, epsilon,
                                     sensitivity) {
  if (!is.numeric(epsilon) || length(epsilon) != 1L || !is.finite(epsilon) ||
      epsilon <= 0 || epsilon > 8 || !is.numeric(sensitivity) ||
      length(sensitivity) != 1L || !is.finite(sensitivity) ||
      sensitivity <= 0) {
    stop("The discrete-Laplace allocation is invalid.", call. = FALSE)
  }
  log_alpha <- -epsilon / sensitivity
  geometric <- function(draw) {
    if (log_alpha < log(.Machine$double.xmin)) return(0)
    u <- .dsomopDpUniform(key, context, coordinate, draw)
    floor(log1p(-u) / log_alpha)
  }
  geometric(1L) - geometric(2L)
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
  min(upper, max(lower, value + noise))
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
    persistent_state = "noise_root_only"
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
  .dsomopDpCanonicalSelfTest()
  status <- .dsomopDpPublicStatus(initialize = .dsomopDpEnabled())
  if (!isTRUE(status$enabled)) {
    .pkg_state$dp_runtime <- list(enabled = FALSE)
  }
  .pkg_state$dp_status <- status
  status
}

.dsomopDpEnsureRuntime <- function() {
  if (is.list(.pkg_state$dp_runtime)) {
    .dsomopDpEnabled()
    return(invisible(.pkg_state$dp_status))
  }
  if (isTRUE(.pkg_state$dp_bootstrap_in_progress)) {
    stop("Recursive DP service bootstrap was detected.", call. = FALSE)
  }
  .pkg_state$dp_bootstrap_in_progress <- TRUE
  on.exit(.pkg_state$dp_bootstrap_in_progress <- FALSE, add = TRUE)
  invisible(.dsomopDpBootstrap())
}
