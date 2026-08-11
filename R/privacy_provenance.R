# Authenticated provenance for person-local sticky-noise inputs.
#
# `omop.table` proves pseudonymization, not contribution locality.  Only
# audited producers call the sealing helper below.  The MAC binds the complete
# frame (except the capsule itself) to the current privacy domain/snapshot, so a
# generic assign method cannot copy a valid attribute onto modified data.

.DSOMOP_DP_PROVENANCE_PROTOCOL <- "dsomop-dp-person-local-provenance-v2"
.DSOMOP_DP_LINEAGE_PROTOCOL <- "dsomop-dp-semantic-lineage-v1"
.DSOMOP_DP_DATASET_PROTOCOL <- "dsomop-dp-dataset-identity-v1"
.DSOMOP_DP_FRAME_DIGEST_PROTOCOL <- "dsomop-dp-frame-digest-chunked-v1"
.DSOMOP_DP_FRAME_DIGEST_CHUNK_ROWS <- 8192L
.DSOMOP_DP_FILTER_STATE_ATTRIBUTE <- "dsomop_dp_filter_state"

.dsomopDpFilterState <- function(x, provenance = NULL, policy = NULL) {
  state <- attr(x, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE, exact = TRUE)
  if (is.null(state)) return(NULL)
  valid <- is.list(state) && identical(
    names(state), c("version", "base_lineage", "filter_tree")
  ) && identical(state$version, 1L) &&
    is.character(state$base_lineage) && length(state$base_lineage) == 1L &&
    !is.na(state$base_lineage) &&
    grepl("^[0-9a-f]{64}$", state$base_lineage)
  if (!isTRUE(valid)) {
    stop("Authenticated DP filter state is malformed.", call. = FALSE)
  }
  normalized <- .dsomopDpNormalizeFilterTree(state$filter_tree)
  if (!identical(normalized, state$filter_tree)) {
    stop("Authenticated DP filter state is not canonical.", call. = FALSE)
  }
  if (is.null(policy)) policy <- .dsomopDpPolicy()
  if (is.null(provenance)) {
    provenance <- .dsomopDpVerifyPersonLocal(x, policy = policy)
  }
  expected_lineage <- .dsomopDpLineageId(
    policy, provenance$dataset_id,
    list(
      producer = "manipulate/filter",
      episode_domain = provenance$episode_domain,
      operation = list(
        kind = "filter_conjunction",
        version = 1L,
        parent = state$base_lineage,
        filter_tree = state$filter_tree
      )
    )
  )
  if (!identical(expected_lineage, provenance$lineage_id)) {
    stop("Authenticated DP filter state does not match its provenance MAC.",
         call. = FALSE)
  }
  state
}

.dsomopDpSetFilterState <- function(x, state) {
  attr(x, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- state
  x
}

.dsomopDpFrameDigest <- function(x) {
  if (!is.data.frame(x)) {
    stop("Only data frames can carry DP person-local provenance.",
         call. = FALSE)
  }
  attr(x, "dsomop_dp_provenance") <- NULL
  frame_attributes <- attributes(x)
  frame_attributes[["dsomop_dp_provenance"]] <- NULL
  # Names and row names are encoded by every ordered data chunk. The remaining
  # frame attributes are bound separately because `[.data.frame` is allowed to
  # drop custom top-level metadata while subsetting.
  frame_attributes[["names"]] <- NULL
  frame_attributes[["row.names"]] <- NULL
  attributes_hash <- .dsomopDpSha256(serialize(
    frame_attributes, connection = NULL, version = 3L, xdr = TRUE
  ))
  n_rows <- nrow(x)
  starts <- if (n_rows == 0L) 1L else seq.int(
    1L, n_rows, by = .DSOMOP_DP_FRAME_DIGEST_CHUNK_ROWS
  )
  chunk_hashes <- unname(vapply(starts, function(start) {
    rows <- if (n_rows == 0L) integer(0) else seq.int(
      start,
      min(n_rows, start + .DSOMOP_DP_FRAME_DIGEST_CHUNK_ROWS - 1L)
    )
    chunk <- x[rows, , drop = FALSE]
    attr(chunk, "dsomop_dp_provenance") <- NULL
    .dsomopDpSha256(serialize(
      chunk, connection = NULL, version = 3L, xdr = TRUE
    ))
  }, character(1L)))
  .dsomopDpSha256(.dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_FRAME_DIGEST_PROTOCOL,
    serialization = "r-v3-xdr",
    nrow = n_rows,
    ncol = ncol(x),
    chunk_rows = .DSOMOP_DP_FRAME_DIGEST_CHUNK_ROWS,
    frame_attributes_sha256 = attributes_hash,
    chunks_sha256 = chunk_hashes
  )))
}

.dsomopDpProvenanceText <- function(value, name, nullable = FALSE) {
  if (is.null(value) && nullable) return(NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value) || nchar(value, type = "bytes") > 256L ||
      !grepl("^[A-Za-z0-9][A-Za-z0-9._:@+/-]*$", value)) {
    stop("Invalid DP provenance ", name, ".", call. = FALSE)
  }
  enc2utf8(value)
}

# Normalize audited operation semantics before deriving an opaque lineage id.
# Data frames are deliberately forbidden: lineage identifies the computation,
# never the private values produced by that computation. Named atomic vectors
# are represented explicitly because aliases and their order can be semantic,
# while the canonical DP encoder intentionally rejects named vectors.
.dsomopDpLineageValue <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.data.frame(value)) {
    stop("DP lineage semantics cannot contain data frames.", call. = FALSE)
  }
  if (inherits(value, "Date")) {
    if (anyNA(value)) stop("DP lineage dates cannot be missing.", call. = FALSE)
    value <- format(value, "%Y-%m-%d")
  } else if (inherits(value, c("POSIXct", "POSIXlt"))) {
    if (anyNA(value)) stop("DP lineage datetimes cannot be missing.",
                           call. = FALSE)
    value <- format(as.POSIXct(value, tz = "UTC"),
                    "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  } else if (is.factor(value)) {
    value <- as.character(value)
  } else if (is.list(value)) {
    value <- unclass(value)
    fields <- names(value)
    normalized <- lapply(unname(value), .dsomopDpLineageValue)
    if (!is.null(fields)) {
      if (anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields)) {
        stop("DP lineage semantics contain invalid object fields.",
             call. = FALSE)
      }
      # Canonical JSON sorts named objects. Preserve explicit list order here;
      # callers normalize only the operations proven order-insensitive.
      return(list(
        container_type = "ordered_named_list",
        field_names = unname(enc2utf8(as.character(fields))),
        field_values = normalized
      ))
    }
    return(normalized)
  } else if (is.object(value)) {
    stop("DP lineage semantics contain an unsupported object.",
         call. = FALSE)
  }
  if (!is.atomic(value) || is.raw(value) ||
      !typeof(value) %in% c("logical", "integer", "double", "character")) {
    stop("DP lineage semantics contain an unsupported value.",
         call. = FALSE)
  }
  extra_attributes <- setdiff(names(attributes(value) %||% list()), "names")
  if (length(extra_attributes) > 0L) {
    stop("DP lineage semantics contain unsupported atomic attributes.",
         call. = FALSE)
  }
  value_names <- names(value)
  value <- unname(value)
  if (!is.null(value_names)) {
    return(list(
      value_type = typeof(value),
      names = unname(enc2utf8(as.character(value_names))),
      values = value
    ))
  }
  value
}

.dsomopDpDatasetIdentity <- function(handle) {
  resource_identity <- handle$person_key_identity
  if (!is.character(resource_identity) || length(resource_identity) != 1L ||
      is.na(resource_identity) || !nzchar(resource_identity)) {
    return(NULL)
  }
  dialect <- tolower(handle$target_dialect %||% "")
  bare <- identical(dialect, "sqlite")
  schema <- function(value) {
    if (bare || is.null(value) || !nzchar(value)) "" else enc2utf8(value)
  }
  list(
    resource_identity = enc2utf8(resource_identity),
    target_dialect = dialect,
    cdm_schema = schema(handle$cdm_schema),
    vocabulary_schema = schema(handle$vocab_schema %||% handle$cdm_schema),
    results_schema = schema(.effectiveResultsSchema(handle))
  )
}

.dsomopDpDatasetId <- function(policy, dataset_identity) {
  if (is.null(dataset_identity)) return(NULL)
  .dsomopDpHmac(policy$keys$provenance, .dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_DATASET_PROTOCOL,
    identity = .dsomopDpLineageValue(dataset_identity)
  )))
}

.dsomopDpLineageId <- function(policy, dataset_id, semantic) {
  normalized <- .dsomopDpLineageValue(semantic)
  .dsomopDpHmac(policy$keys$provenance, .dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_LINEAGE_PROTOCOL,
    adjacency = policy$adjacency,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    privacy_epoch = policy$privacy_epoch,
    dataset_id = dataset_id,
    semantic = normalized
  )))
}

.dsomopDpLineageObject <- function(value) {
  if (is.null(value)) return(NULL)
  if (is.list(value) && length(value) == 0L) return(list())
  if (!is.list(value) || is.null(names(value)) || anyNA(names(value)) ||
      any(!nzchar(names(value))) || anyDuplicated(names(value))) {
    stop("DP lineage expected one uniquely named object.", call. = FALSE)
  }
  value[order(names(value), method = "radix")]
}

.dsomopDpLineageKey <- function(value) {
  .dsomopDpCanonicalJson(.dsomopDpLineageValue(value))
}

.dsomopDpLineageSet <- function(value) {
  if (is.null(value)) return(NULL)
  value <- unname(unlist(value, use.names = FALSE))
  if (length(value) == 0L) return(value)
  if (anyNA(value)) {
    stop("DP lineage sets cannot contain missing values.", call. = FALSE)
  }
  normalized <- .dsomopDpLineageValue(value)
  if (is.list(normalized)) {
    stop("DP lineage set values must be atomic.", call. = FALSE)
  }
  sort(unique(normalized), na.last = NA, method = "radix")
}

.dsomopDpOrderLineageNodes <- function(nodes) {
  keys <- vapply(nodes, .dsomopDpLineageKey, character(1L))
  keep <- !duplicated(keys)
  nodes <- nodes[keep]
  keys <- keys[keep]
  unname(nodes[order(keys, method = "radix")])
}

.dsomopDpNormalizeFilterTree <- function(node) {
  if (is.null(node)) return(NULL)
  if (!is.list(node) || length(node) == 0L) {
    stop("DP filter lineage requires a non-empty filter tree.", call. = FALSE)
  }
  fields <- names(node)
  groups <- intersect(fields %||% character(0), c("and", "or"))
  if (length(groups) == 1L && length(fields) == 1L) {
    children <- node[[groups[[1L]]]]
    if (!is.list(children) || length(children) == 0L) {
      stop("DP filter lineage received an empty logical group.", call. = FALSE)
    }
    group <- groups[[1L]]
    normalized <- lapply(children, .dsomopDpNormalizeFilterTree)
    # AND and OR are associative. Flatten nested groups of the same kind before
    # de-duplicating and sorting so parenthesization and input order cannot mint
    # a fresh sticky-noise identity for the same Boolean expression.
    normalized <- unlist(lapply(normalized, function(child) {
      if (identical(names(child), group)) child[[group]] else list(child)
    }), recursive = FALSE)
    normalized <- .dsomopDpOrderLineageNodes(normalized)
    return(stats::setNames(list(normalized), group))
  }
  if (is.null(fields)) {
    normalized <- lapply(node, .dsomopDpNormalizeFilterTree)
    normalized <- unlist(lapply(normalized, function(child) {
      if (identical(names(child), "and")) child$and else list(child)
    }), recursive = FALSE)
    return(list(and = .dsomopDpOrderLineageNodes(normalized)))
  }
  if ("type" %in% fields) {
    params <- node$params %||% list()
    if (!is.list(params)) {
      stop("DP cohort-filter lineage requires named params.", call. = FALSE)
    }
    if (!is.null(params$concept_ids) || !is.null(params$concept_id)) {
      params$concept_ids <- .dsomopDpLineageSet(
        params$concept_ids %||% params$concept_id
      )
      params$concept_id <- NULL
    }
    if (!is.null(params$visit_concept_ids) ||
        !is.null(params$visit_concept_id)) {
      params$visit_concept_ids <- .dsomopDpLineageSet(
        params$visit_concept_ids %||% params$visit_concept_id
      )
      params$visit_concept_id <- NULL
    }
    if (!is.null(params$groups)) {
      params$groups <- .dsomopDpLineageSet(params$groups)
    }
    if (!is.null(params$table) && is.character(params$table)) {
      params$table <- tolower(params$table)
    }
    return(list(
      type = tolower(node$type),
      params = .dsomopDpLineageObject(params)
    ))
  }
  if (all(c("var", "op") %in% fields)) {
    op <- tolower(node$op)
    aliases <- c(
      "==" = "in", "eq" = "in", "!=" = "not_in", "ne" = "not_in",
      ">=" = "gte", "gte" = "gte", "<=" = "lte", "lte" = "lte",
      ">" = "gt", "gt" = "gt", "<" = "lt", "lt" = "lt"
    )
    if (op %in% names(aliases)) op <- unname(aliases[[op]])
    value <- node$value
    if (op %in% c("in", "not_in")) {
      value <- .dsomopDpLineageSet(value)
    }
    return(list(
      var = tolower(node$var), op = op, value = value,
      safe_scope = if (is.null(node$safe_scope)) NULL else
        .dsomopDpLineageObject(node$safe_scope)
    ))
  }
  stop("DP filter lineage received an unsupported filter node.", call. = FALSE)
}

.dsomopDpNormalizeConceptSet <- function(value) {
  if (is.null(value)) return(NULL)
  if (!is.list(value)) return(.dsomopDpLineageSet(value))
  if (!is.null(value$ids)) value$ids <- .dsomopDpLineageSet(value$ids)
  if (!is.null(value$concepts)) {
    value$concepts <- .dsomopDpLineageSet(value$concepts)
  }
  if (!is.null(value$exclude)) {
    value$exclude <- .dsomopDpLineageSet(value$exclude)
  }
  .dsomopDpLineageObject(value)
}

.dsomopDpNormalizeVisitFilter <- function(value) {
  if (is.null(value)) return(NULL)
  if (!is.list(value)) {
    stop("DP visit-filter lineage requires a named object.", call. = FALSE)
  }
  if (!is.null(value$concept_ids)) {
    value$concept_ids <- .dsomopDpLineageSet(value$concept_ids)
  }
  .dsomopDpLineageObject(value)
}

.dsomopDpProvenanceMac <- function(x, contract, policy) {
  .dsomopDpHmac(policy$keys$provenance, .dsomopDpCanonicalJson(list(
    protocol = .DSOMOP_DP_PROVENANCE_PROTOCOL,
    contract = contract,
    frame_digest = .dsomopDpFrameDigest(x)
  )))
}

.dsomopDpSealPersonLocal <- function(x, producer, episode_domain = NULL,
                                     lineage = NULL, policy = NULL,
                                     dataset_identity = NULL,
                                     dataset_id = NULL) {
  attr(x, "dsomop_dp_provenance") <- NULL
  if (!.dsomopDpEnabled()) return(x)
  if (!is.data.frame(x) || !.is_omop.table(x)) return(x)
  if (is.null(policy)) policy <- .dsomopDpPolicy()
  producer <- .dsomopDpProvenanceText(producer, "producer")
  episode_domain <- .dsomopDpProvenanceText(
    episode_domain, "episode domain", nullable = TRUE
  )
  pseudonymization <- .assertOmopTablePseudonymization(
    x, caller = ".dsomopDpSealPersonLocal"
  )
  if (!isTRUE(pseudonymization$resource_scoped)) return(x)
  if (!is.null(dataset_identity) && !is.null(dataset_id)) {
    stop("DP sealing received two dataset identities.", call. = FALSE)
  }
  if (is.null(dataset_id)) {
    dataset_id <- .dsomopDpDatasetId(policy, dataset_identity)
  }
  if (is.null(dataset_id)) return(x)
  if (!is.character(dataset_id) || length(dataset_id) != 1L ||
      is.na(dataset_id) || !grepl("^[0-9a-f]{64}$", dataset_id)) {
    stop("Invalid DP dataset identity.", call. = FALSE)
  }
  keys <- .omopPersonKeys(x)
  if (length(keys) != 1L) {
    stop("DP provenance requires exactly one pseudonymous person key.",
         call. = FALSE)
  }
  protected <- attr(x, "dsomop_protected", exact = TRUE) %||% character(0)
  if (!keys[[1L]] %in% protected) {
    stop("DP provenance requires an intact protected person key.",
         call. = FALSE)
  }
  if (is.null(lineage)) {
    lineage <- list(kind = "audited_producer", producer = producer)
  }
  lineage_id <- .dsomopDpLineageId(policy, dataset_id, list(
    producer = producer,
    episode_domain = episode_domain,
    operation = lineage
  ))
  contract <- list(
    protocol = .DSOMOP_DP_PROVENANCE_PROTOCOL,
    adjacency = policy$adjacency,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    privacy_epoch = policy$privacy_epoch,
    person_key = keys[[1L]],
    dataset_id = dataset_id,
    producer = producer,
    episode_domain = episode_domain,
    lineage_id = lineage_id
  )
  capsule <- c(contract, list(
    mac = .dsomopDpProvenanceMac(x, contract, policy)
  ))
  attr(x, "dsomop_dp_provenance") <- capsule
  x
}

.dsomopDpVerifyPersonLocal <- function(x, policy = NULL,
                                       allow_missing = FALSE) {
  capsule <- attr(x, "dsomop_dp_provenance", exact = TRUE)
  if (is.null(capsule) && isTRUE(allow_missing)) return(NULL)
  if (is.null(policy)) policy <- .dsomopDpPolicy()
  expected_names <- c(
    "protocol", "adjacency", "domain", "snapshot_id", "privacy_epoch",
    "person_key", "dataset_id", "producer", "episode_domain", "lineage_id",
    "mac"
  )
  if (!is.list(capsule) || is.null(names(capsule)) ||
      !identical(names(capsule), expected_names)) {
    stop("The omop.table lacks a valid person-local DP provenance capsule; ",
         "recreate it through an audited dsOMOP plan or loader.",
         call. = FALSE)
  }
  text_fields <- c("protocol", "adjacency", "domain", "snapshot_id",
                   "person_key", "dataset_id", "producer", "lineage_id", "mac")
  if (any(!vapply(capsule[text_fields], function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(value)
  }, logical(1L))) ||
      (!is.null(capsule$episode_domain) &&
       (!is.character(capsule$episode_domain) ||
        length(capsule$episode_domain) != 1L ||
        is.na(capsule$episode_domain) || !nzchar(capsule$episode_domain))) ||
      !is.numeric(capsule$privacy_epoch) ||
      length(capsule$privacy_epoch) != 1L ||
      is.na(capsule$privacy_epoch) || !is.finite(capsule$privacy_epoch) ||
      capsule$privacy_epoch < 1 ||
      capsule$privacy_epoch != floor(capsule$privacy_epoch)) {
    stop("The omop.table DP provenance capsule is malformed.", call. = FALSE)
  }
  if (!grepl("^[0-9a-f]{64}$", capsule$lineage_id) ||
      !grepl("^[0-9a-f]{64}$", capsule$dataset_id)) {
    stop("The omop.table DP provenance lineage is malformed.",
         call. = FALSE)
  }
  keys <- .omopPersonKeys(x)
  coherent <- identical(capsule$protocol,
                        .DSOMOP_DP_PROVENANCE_PROTOCOL) &&
    identical(capsule$adjacency, policy$adjacency) &&
    identical(capsule$domain, policy$domain) &&
    identical(capsule$snapshot_id, policy$snapshot_id) &&
    identical(capsule$privacy_epoch, policy$privacy_epoch) &&
    length(keys) == 1L && identical(capsule$person_key, keys[[1L]]) &&
    capsule$person_key %in%
      (attr(x, "dsomop_protected", exact = TRUE) %||% character(0))
  if (!coherent) {
    stop("The omop.table DP provenance does not match the current privacy ",
         "domain, snapshot, privacy epoch, or person-key contract.",
         call. = FALSE)
  }
  contract <- capsule[setdiff(expected_names, "mac")]
  expected_mac <- .dsomopDpProvenanceMac(x, contract, policy)
  if (!identical(capsule$mac, expected_mac)) {
    stop("The omop.table DP provenance MAC does not match its content.",
         call. = FALSE)
  }
  capsule
}

.dsomopDpScopePersonLocal <- function(scope, policy) {
  if (is.null(scope)) return(TRUE)
  if (.is_omop.table(scope)) {
    return(!is.null(.dsomopDpVerifyPersonLocal(
      scope, policy = policy, allow_missing = TRUE
    )))
  }
  if (is.data.frame(scope)) return(FALSE)
  if (is.list(scope)) {
    if (length(scope) == 0L) return(TRUE)
    return(all(vapply(scope, .dsomopDpScopePersonLocal, logical(1L),
                      policy = policy)))
  }
  # A scalar may name an arbitrary persistent or temporary cohort. Until cohort
  # references carry their own handle-bound lineage capsule, they cannot prove
  # person-local preprocessing merely by having passed an nfilter size gate.
  FALSE
}

.dsomopDpFilterTreePersonLocal <- function(node) {
  if (is.null(node) || length(node) == 0L) return(TRUE)
  if (!is.list(node)) return(FALSE)
  fields <- names(node)
  if (!is.null(fields) && "type" %in% fields) {
    return(
      is.character(node$type) && length(node$type) == 1L &&
        !is.na(node$type) && !identical(tolower(node$type), "cohort")
    )
  }
  if (!is.null(fields)) {
    groups <- intersect(fields, c("and", "or"))
    if (length(groups) != 1L || length(fields) != 1L) return(FALSE)
    children <- node[[groups[[1L]]]]
  } else {
    children <- node
  }
  is.list(children) && length(children) > 0L && all(vapply(
    children, .dsomopDpFilterTreePersonLocal, logical(1L)
  ))
}

.dsomopDpPlanCohortPersonLocal <- function(cohort) {
  if (is.null(cohort)) return(TRUE)
  if (!is.list(cohort) || !is.null(cohort$cohort_definition_id) ||
      identical(tolower(cohort$type %||% ""), "cohort_table")) {
    return(FALSE)
  }
  if (!is.null(cohort$filter_tree) &&
      !.dsomopDpFilterTreePersonLocal(cohort$filter_tree)) {
    return(FALSE)
  }
  # Inline `spec` and non-persistent filter trees are executed by the audited
  # per-person cohort DSL. Empty cohort declarations mean unrestricted base.
  is.null(cohort$filter_tree) ||
    .dsomopDpFilterTreePersonLocal(cohort$filter_tree)
}

.dsomopDpPopulationLocality <- function(plan) {
  populations <- plan$populations %||% list()
  base_safe <- .dsomopDpPlanCohortPersonLocal(plan$cohort)
  if (!is.list(populations) || length(populations) == 0L) {
    return(c(base = base_safe))
  }
  ids <- names(populations)
  if (is.null(ids) || anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    return(stats::setNames(
      rep(FALSE, length(populations)), ids %||% character(0)
    ))
  }
  safe <- stats::setNames(logical(length(populations)), ids)
  for (id in ids) {
    population <- populations[[id]]
    if (!is.list(population)) next
    kind <- tolower(population$kind %||% "criteria")
    if (identical(kind, "setop") || !is.null(population$setop)) {
      members <- as.character(unlist(
        population$setop$members %||% character(0), use.names = FALSE
      ))
      op <- tolower(population$setop$op %||% "union")
      if (identical(op, "difference")) op <- "setdiff"
      valid_setop <- length(members) >= 2L && !anyNA(members) &&
        all(nzchar(members)) && !anyDuplicated(members) &&
        op %in% c("union", "intersect", "setdiff")
      safe[[id]] <- valid_setop && all(members %in% ids) &&
        all(vapply(members, function(member) {
          isTRUE(safe[[member]])
        }, logical(1L)))
      next
    }
    local <- is.null(population$cohort_definition_id) &&
      .dsomopDpFilterTreePersonLocal(population$filter_tree)
    if (identical(id, "base")) local <- local && base_safe
    safe[[id]] <- local
  }
  if (!"base" %in% names(safe)) safe <- c(base = base_safe, safe)
  safe
}

.dsomopDpPlanOutputPersonLocal <- function(plan, output_name, policy) {
  if (!is.list(plan) || !is.list(plan$outputs) ||
      is.null(plan$outputs[[output_name]])) {
    return(FALSE)
  }
  out <- plan$outputs[[output_name]]
  scope <- plan$scope
  scope_source <- if (is.list(scope) && !is.data.frame(scope) &&
      !.is_omop.table(scope)) {
    scope$tables_frames %||% scope$cohort
  } else scope
  if (!.dsomopDpScopePersonLocal(scope_source, policy)) return(FALSE)
  # A plan-level persistent cohort is folded into the global scope and therefore
  # taints every population. A filter/spec cohort affects the base branch.
  if (is.list(plan$cohort) &&
      (!is.null(plan$cohort$cohort_definition_id) ||
       identical(tolower(plan$cohort$type %||% ""), "cohort_table"))) {
    return(FALSE)
  }
  population_id <- out$population_id %||% "base"
  locality <- .dsomopDpPopulationLocality(plan)
  if (!is.character(population_id) || length(population_id) != 1L ||
      is.na(population_id) || !population_id %in% names(locality) ||
      !isTRUE(locality[[population_id]])) {
    return(FALSE)
  }
  type <- tolower(out$type %||% "event_level")
  if (identical(type, "event_level")) {
    representation_spec <- out$representation
    representation <- tolower(if (is.character(representation_spec)) {
      representation_spec
    } else representation_spec$format %||% "long")
    if (identical(representation, "long")) return(TRUE)
    if (identical(representation, "wide")) {
      concept_set <- out$filters$concept_set %||% out$concept_set
      return(!is.null(concept_set))
    }
    if (identical(representation, "features")) {
      return(length(out$representation$features %||% list()) > 0L)
    }
    return(FALSE)
  }
  if (identical(type, "person_level")) {
    tables <- out$tables %||% list()
    safe <- vapply(names(tables), function(table) {
      entry <- tables[[table]]
      if (is.list(entry) && "features" %in% names(entry)) {
        return(length(entry$features %||% list()) > 0L)
      }
      tolower(table) %in% c("person", "death")
    }, logical(1L))
    return(all(safe))
  }
  if (identical(type, "baseline")) {
    return(!"age_at_index" %in% tolower(out$derived %||% character(0)))
  }
  type %in% c("survival", "cohort_membership", "intervals_long")
}

.dsomopDpScopeLineageSemantic <- function(scope, policy) {
  if (is.null(scope)) return(NULL)
  if (.is_omop.table(scope)) {
    provenance <- .dsomopDpVerifyPersonLocal(scope, policy = policy)
    return(list(kind = "frame_parent", lineage_id = provenance$lineage_id))
  }
  if (is.data.frame(scope) || !is.list(scope)) {
    stop("A DP plan scope lacks authenticated semantic lineage.",
         call. = FALSE)
  }
  scope <- Filter(Negate(is.null), unname(scope))
  unname(lapply(scope, .dsomopDpScopeLineageSemantic, policy = policy))
}

.dsomopPlanCohortIdSemantic <- function(value, field = "cohort_definition_id",
                                        minimum = 0L) {
  if (is.null(value)) return(NULL)
  if (is.list(value) && !is.data.frame(value) && length(value) == 1L) {
    value <- value[[1L]]
  }
  numeric_value <- if (is.character(value) && length(value) == 1L &&
      !is.na(value) && grepl("^[0-9]+$", value)) {
    suppressWarnings(as.numeric(value))
  } else if (is.numeric(value) && length(value) == 1L && !is.na(value)) {
    as.numeric(value)
  } else {
    NA_real_
  }
  if (!is.finite(numeric_value) || numeric_value != floor(numeric_value) ||
      numeric_value < minimum || numeric_value > .Machine$integer.max) {
    stop("Plan semantic ", field, " must be one exact integer >= ",
         minimum, ".", call. = FALSE)
  }
  as.integer(numeric_value)
}

.dsomopDpPlanCohortSemantic <- function(cohort) {
  if (is.null(cohort)) return(NULL)
  list(
    type = tolower(cohort$type %||% if (!is.null(cohort$spec) ||
                                         !is.null(cohort$filter_tree)) {
      "spec"
    } else "none"),
    cohort_definition_id = .dsomopPlanCohortIdSemantic(
      cohort$cohort_definition_id
    ),
    spec = cohort$spec,
    filter_tree = if (is.null(cohort$filter_tree)) NULL else
      .dsomopDpNormalizeFilterTree(cohort$filter_tree),
    episode_policy = tolower(cohort$episode_policy %||% "any_episode")
  )
}

.dsomopDpPlanPopulationSemantic <- function(plan, population_id,
                                             seen = character(0)) {
  if (population_id %in% seen) {
    stop("A DP plan population lineage contains a cycle.", call. = FALSE)
  }
  populations <- plan$populations %||% list()
  if (length(populations) == 0L) {
    if (!identical(population_id, "base")) {
      stop("A DP plan output references an unknown population.",
           call. = FALSE)
    }
    return(list(
      kind = "criteria",
      cohort = .dsomopDpPlanCohortSemantic(plan$cohort)
    ))
  }
  population <- populations[[population_id]]
  if (!is.list(population)) {
    stop("A DP plan output references an invalid population.",
         call. = FALSE)
  }
  kind <- tolower(population$kind %||% "criteria")
  if (identical(kind, "setop") || !is.null(population$setop)) {
    members <- as.character(unlist(
      population$setop$members %||% character(0), use.names = FALSE
    ))
    if (length(members) < 2L || anyNA(members) || any(!nzchar(members)) ||
        anyDuplicated(members)) {
      stop("A DP plan set operation requires at least two distinct members.",
           call. = FALSE)
    }
    member_semantics <- lapply(
      members, .dsomopDpPlanPopulationSemantic,
      plan = plan, seen = c(seen, population_id)
    )
    op <- tolower(population$setop$op %||% "union")
    if (identical(op, "difference")) op <- "setdiff"
    if (!op %in% c("union", "intersect", "setdiff")) {
      stop("A DP plan set operation has an unsupported operator.",
           call. = FALSE)
    }
    keys <- vapply(member_semantics, .dsomopDpLineageKey, character(1L))
    if (anyDuplicated(keys)) {
      stop("A DP plan set operation has semantically duplicate members.",
           call. = FALSE)
    }
    if (identical(op, "union")) {
      member_semantics <- member_semantics[order(keys, method = "radix")]
    }
    return(list(
      kind = "setop",
      op = op,
      members = unname(member_semantics)
    ))
  }
  list(
    kind = "criteria",
    cohort = if (identical(population_id, "base")) {
      .dsomopDpPlanCohortSemantic(plan$cohort)
    } else NULL,
    cohort_definition_id = .dsomopPlanCohortIdSemantic(
      population$cohort_definition_id,
      field = paste0("population '", population_id,
                     "' cohort_definition_id")
    ),
    filter_tree = if (is.null(population$filter_tree)) NULL else
      .dsomopDpNormalizeFilterTree(population$filter_tree),
    episode_policy = tolower(population$episode_policy %||% "any_episode"),
    index_event = if (is.null(population$index_event)) NULL else
      .dsomopDpLineageObject(population$index_event)
  )
}

.dsomopDpEffectiveDateHandling <- function(value = NULL) {
  normalized <- .normalizeDateHandling(value)
  if (is.null(normalized)) {
    normalized <- .normalizeDateHandling(
      getOption("dsomop.default_date_handling", "remove")
    )
  }
  normalized
}

.dsomopDpNormalizeTemporal <- function(temporal) {
  if (is.null(temporal)) return(NULL)
  .validateTemporalSpec(temporal)
  list(
    index_window = if (is.null(temporal$index_window)) NULL else list(
      start = temporal$index_window$start,
      end = temporal$index_window$end
    ),
    calendar = if (is.null(temporal$calendar)) NULL else list(
      start = temporal$calendar$start,
      end = temporal$calendar$end
    ),
    event_select = if (is.null(temporal$event_select)) NULL else list(
      order = tolower(temporal$event_select$order),
      n = as.integer(temporal$event_select$n %||% 1L),
      by = tolower(temporal$event_select$by %||% "grain")
    ),
    min_gap = if (is.null(temporal$min_gap)) NULL else
      .normalizeMinGap(temporal$min_gap)
  )
}

.dsomopDpNormalizeFeatures <- function(features) {
  if (is.null(features)) return(NULL)
  if (!is.list(features)) {
    stop("DP feature lineage requires a feature list.", call. = FALSE)
  }
  feature_names <- names(features)
  unname(lapply(seq_along(features), function(index) {
    spec <- features[[index]]
    if (is.list(spec)) {
      if (!is.null(spec$concept_set)) {
        spec$concept_set <- .dsomopDpNormalizeConceptSet(spec$concept_set)
      }
      if (!is.null(spec$filters)) {
        spec$filters <- .dsomopDpNormalizeFilterTree(spec$filters)
      }
    }
    list(
      feature_name = if (is.null(feature_names)) NULL else feature_names[[index]],
      spec = spec
    )
  }))
}

.dsomopDpPersonTableSequence <- function(tables) {
  # Derived-only person-level recipes intentionally carry `tables = list()`.
  # That is a fixed empty source-table sequence, not a malformed unnamed
  # sequence; the audited derived-column specifications supply the data path.
  if (is.null(tables) || (is.list(tables) && length(tables) == 0L)) {
    return(list())
  }
  if (!is.list(tables) || is.null(names(tables))) {
    stop("DP person-level lineage requires named source tables.", call. = FALSE)
  }
  unname(Map(function(table, entry) {
    if (is.list(entry) && !is.null(entry$features)) {
      entry$concept_set <- .dsomopDpNormalizeConceptSet(entry$concept_set)
      entry$features <- .dsomopDpNormalizeFeatures(entry$features)
      if (!is.null(entry$filters)) {
        entry$filters <- .dsomopDpNormalizeFilterTree(entry$filters)
      }
      entry$concept_col <- if (is.null(entry$concept_col)) NULL else
        tolower(entry$concept_col)
      entry$visit <- .dsomopDpNormalizeVisitFilter(entry$visit)
    }
    list(table = tolower(table), spec = entry)
  }, names(tables), unname(tables)))
}

.dsomopDpIntervalsConcepts <- function(tables, concept_filter) {
  if (is.null(concept_filter)) return(list())
  if (!is.list(concept_filter)) {
    stop("DP intervals lineage requires named per-table concept filters.",
         call. = FALSE)
  }
  concept_names <- names(concept_filter)
  if (is.null(concept_names) || any(!nzchar(concept_names)) ||
      anyDuplicated(tolower(concept_names))) {
    stop("DP intervals lineage requires unique per-table concept filters.",
         call. = FALSE)
  }
  concept_names <- tolower(concept_names)
  unname(lapply(tables, function(table) {
    index <- match(tolower(table), concept_names)
    value <- if (is.na(index)) NULL else concept_filter[[index]]
    list(
      table = tolower(table),
      concepts = .dsomopDpNormalizeConceptSet(value)
    )
  }))
}

.dsomopDpCombinedFilter <- function(...) {
  trees <- Filter(function(tree) !is.null(tree) && length(tree) > 0L,
                  list(...))
  if (length(trees) == 0L) return(NULL)
  if (length(trees) == 1L) {
    return(.dsomopDpNormalizeFilterTree(trees[[1L]]))
  }
  .dsomopDpNormalizeFilterTree(list(and = trees))
}

.dsomopDpIntervalsFilters <- function(tables, source_filters, global_filter) {
  if (!is.null(source_filters)) {
    if (!is.list(source_filters) || is.null(names(source_filters)) ||
        any(!nzchar(names(source_filters))) ||
        anyDuplicated(tolower(names(source_filters)))) {
      stop("DP intervals lineage requires unique per-table source filters.",
           call. = FALSE)
    }
    source_names <- tolower(names(source_filters))
  } else {
    source_names <- character(0)
  }
  unname(lapply(tables, function(table) {
    index <- match(tolower(table), source_names)
    source_filter <- if (is.na(index)) NULL else source_filters[[index]]
    list(
      table = tolower(table),
      filter = .dsomopDpCombinedFilter(source_filter, global_filter)
    )
  }))
}

.dsomopDpSurvivalOutcomes <- function(output) {
  advanced <- !is.null(output$outcomes)
  outcomes <- if (advanced) output$outcomes else list(outcome = output$outcome)
  if (!is.list(outcomes) || length(outcomes) == 0L ||
      is.null(names(outcomes)) || any(!nzchar(names(outcomes))) ||
      anyDuplicated(names(outcomes))) {
    stop("DP survival lineage requires uniquely named outcomes.",
         call. = FALSE)
  }
  global_filter <- output$filters$custom
  unname(lapply(seq_along(outcomes), function(index) {
    endpoint <- outcomes[[index]]
    if (!is.list(endpoint)) {
      stop("DP survival lineage received an invalid outcome.", call. = FALSE)
    }
    list(
      name = names(outcomes)[[index]],
      priority = as.integer(index),
      table = tolower(endpoint$table %||% ""),
      concept_set = .dsomopDpNormalizeConceptSet(endpoint$concept_set),
      filter = .dsomopDpCombinedFilter(endpoint$filters, global_filter)
    )
  }))
}

.dsomopDpMultistateSemantic <- function(output, tie_policy = "priority") {
  if (!identical(tolower(output$format %||% "survival"), "multi_state")) {
    return(NULL)
  }
  outcomes <- output$outcomes
  if (!is.list(outcomes) || length(outcomes) == 0L ||
      is.null(names(outcomes)) || any(!nzchar(names(outcomes))) ||
      anyDuplicated(names(outcomes))) {
    stop("DP multi-state lineage requires uniquely named outcomes.",
         call. = FALSE)
  }
  normalized_outcomes <- lapply(names(outcomes), function(name) {
    list(name = name)
  })
  spec <- .normalizeMultistateSpec(
    normalized_outcomes,
    transitions = output$transitions,
    initial_state = output$initial_state,
    state_hierarchy = output$state_hierarchy,
    state_step = output$state_step
  )
  list(
    initial_state = spec$initial_state,
    transitions = spec$transitions,
    state_hierarchy = spec$state_hierarchy,
    state_step = if (identical(tie_policy, "sequential")) {
      spec$state_step
    } else {
      NULL
    }
  )
}

.dsomopDpPlanOutputSemantic <- function(output) {
  type <- tolower(output$type %||% "event_level")
  if (identical(type, "event_level")) {
    filters <- output$filters %||% list()
    concept_set <- filters$concept_set
    if (is.list(concept_set) && !is.null(concept_set$ids)) {
      concept_set <- concept_set$ids
    }
    concept_set <- .dsomopDpNormalizeConceptSet(
      concept_set %||% output$concept_set
    )
    representation <- output$representation %||% list(format = "long")
    if (is.character(representation)) {
      representation <- list(format = representation)
    }
    return(list(
      type = type,
      table = tolower(output$table %||% ""),
      columns = output$columns,
      concept_set = concept_set,
      time_window = if (is.null(filters$time_window)) NULL else list(
        date_column = filters$time_window$date_column,
        start_date = filters$time_window$start_date,
        end_date = filters$time_window$end_date
      ),
      custom_filter = if (is.null(filters$custom)) NULL else
        .dsomopDpNormalizeFilterTree(filters$custom),
      visit_filter = .dsomopDpNormalizeVisitFilter(
        filters$visit %||% output$visit_filter
      ),
      concept_col = tolower(
        filters$concept_col %||% output$concept_col %||% ""
      ),
      temporal = .dsomopDpNormalizeTemporal(output$temporal),
      date_handling = .dsomopDpEffectiveDateHandling(output$date_handling),
      representation = list(
        format = tolower(representation$format %||% "long"),
        grain = tolower(representation$grain %||% "person"),
        features = if (identical(
          tolower(representation$format %||% "long"), "features"
        )) .dsomopDpNormalizeFeatures(representation$features) else NULL
      )
    ))
  }
  if (identical(type, "person_level")) {
    return(list(
      type = type,
      tables = .dsomopDpPersonTableSequence(output$tables),
      derived_columns = .normalizeDerivedColumnSpecs(
        output$derived_columns
      ),
      custom_filter = if (is.null(output$filters$custom)) NULL else
        .dsomopDpNormalizeFilterTree(output$filters$custom),
      date_handling = .dsomopDpEffectiveDateHandling()
    ))
  }
  if (identical(type, "baseline")) {
    return(list(
      type = type,
      columns = output$columns,
      derived = output$derived,
      age_breaks = output$age_breaks,
      custom_filter = if (is.null(output$filters$custom)) NULL else
        .dsomopDpNormalizeFilterTree(output$filters$custom)
    ))
  }
  if (identical(type, "survival")) {
    advanced <- !is.null(output$outcomes)
    format <- tolower(output$format %||% "survival")
    tie_policy <- tolower(output$tie_policy %||% "priority")
    multi_state <- .dsomopDpMultistateSemantic(output, tie_policy)
    outcomes <- .dsomopDpSurvivalOutcomes(output)
    if (!is.null(multi_state)) {
      outcome_names <- vapply(outcomes, `[[`, character(1L), "name")
      outcome_order <- multi_state$transitions$states[
        multi_state$transitions$states %in% outcome_names
      ]
      outcomes <- outcomes[match(outcome_order, outcome_names)]
      outcomes <- lapply(seq_along(outcomes), function(index) {
        outcomes[[index]]$priority <- as.integer(index)
        outcomes[[index]]
      })
    }
    censoring <- output$censoring %||% if (advanced) {
      list(cohort_end = TRUE, observation_period_end = TRUE, death = TRUE)
    } else {
      list(cohort_end = TRUE, observation_period_end = TRUE, death = FALSE)
    }
    return(list(
      type = type,
      legacy = !advanced,
      outcomes = outcomes,
      format = format,
      tar = list(
        start_offset = as.integer(output$tar$start_offset %||% 0L),
        end_offset = if (is.null(output$tar$end_offset)) NULL else
          as.integer(output$tar$end_offset)
      ),
      event_order = tolower(output$event_order %||%
                              if (identical(format, "multi_state")) {
                                "all"
                              } else {
                                "first"
                              }),
      washout_days = as.integer(output$washout_days %||% 0L),
      tie_policy = tie_policy,
      multi_state = multi_state,
      censoring = list(
        cohort_end = isTRUE(censoring$cohort_end %||% TRUE),
        observation_period_end = isTRUE(
          censoring$observation_period_end %||% TRUE
        ),
        death = isTRUE(censoring$death %||% TRUE),
        admin_date = censoring$admin_date %||% NULL
      )
    ))
  }
  if (identical(type, "cohort_membership")) {
    return(list(
      type = type,
      date_handling = .dsomopDpEffectiveDateHandling(output$date_handling)
    ))
  }
  if (identical(type, "intervals_long")) {
    tables <- unname(tolower(as.character(unlist(
      output$tables, use.names = FALSE
    ))))
    return(list(
      type = type,
      tables = tables,
      concept_filter = .dsomopDpIntervalsConcepts(
        tables, output$concept_filter
      ),
      source_filters = .dsomopDpIntervalsFilters(
        tables, output$source_filters, output$filters$custom
      ),
      window = if (is.null(output$window)) {
        list(reference = "cohort_episode")
      } else {
        .dsomopDpLineageObject(output$window)
      },
      interval_match = tolower(output$interval_match %||% "overlaps"),
      event_select = tolower(output$event_select %||% "all"),
      select_n = as.integer(output$select_n %||% 1L),
      select_by = tolower(output$select_by %||% "episode_source"),
      anchor = as.integer(output$anchor %||% 0L)
    ))
  }
  if (type %in% c("temporal_covariates", "person_period")) {
    return(list(
      type = type,
      table = tolower(output$table %||% ""),
      concept_set = .dsomopDpNormalizeConceptSet(output$concept_set),
      bin_width = as.integer(output$bin_width %||% 30L),
      window_start = as.integer(output$window_start %||% -365L),
      window_end = as.integer(output$window_end %||% 0L),
      analyses = sort(unique(tolower(unname(output$analyses %||% "binary")))),
      filter = .dsomopDpCombinedFilter(output$filters$custom),
      grain = if (identical(type, "person_period")) {
        tolower(output$grain %||% "episode")
      } else NULL,
      time_origin = if (identical(type, "person_period")) {
        tolower(output$time_origin %||% "index")
      } else NULL
    ))
  }
  if (identical(type, "concept_dictionary")) {
    sources <- output$source_outputs
    if (!is.null(sources)) {
      sources <- sort(unique(as.character(unlist(sources, use.names = FALSE))))
    }
    return(list(type = type, source_outputs = sources))
  }
  stop("This plan output has no audited DP lineage contract.",
       call. = FALSE)
}

.dsomopDpPlanLineageSemantic <- function(plan, output_name, policy,
                                         component = NULL) {
  output <- plan$outputs[[output_name]]
  population_id <- output$population_id %||% "base"
  scope <- plan$scope
  scope_source <- if (is.list(scope) && !is.data.frame(scope) &&
      !.is_omop.table(scope)) {
    scope$tables_frames %||% scope$cohort
  } else scope
  options <- plan$options %||% list()
  scope_lineage <- .dsomopDpScopeLineageSemantic(scope_source, policy)
  if (!is.null(scope_lineage) &&
      identical(scope_lineage$kind %||% NULL, "frame_parent")) {
    scope_lineage <- list(scope_lineage)
  }
  list(
    kind = "plan_output",
    contract_version = 2L,
    component = component,
    scope = list(
      combine = tolower(if (is.list(scope) && !is.data.frame(scope)) {
        scope$combine %||% "union"
      } else "union"),
      sources = unname(scope_lineage %||% list())
    ),
    index_anchor = .dsomopDpPlanCohortSemantic(plan$cohort),
    population = .dsomopDpPlanPopulationSemantic(plan, population_id),
    output = .dsomopDpPlanOutputSemantic(output),
    public_output_contract = .stagedSemanticContract(
      plan, output_name, component
    ),
    options = list(
      translate_concepts = isTRUE(options$translate_concepts %||% TRUE),
      block_sensitive = isTRUE(options$block_sensitive %||% TRUE)
    )
  )
}

.dsomopDpSealPlanOutput <- function(x, plan, output_name,
                                    dataset_identity = NULL,
                                    component = NULL) {
  attr(x, "dsomop_dp_provenance") <- NULL
  if (!.dsomopDpEnabled() || !is.data.frame(x) || !.is_omop.table(x)) {
    return(x)
  }
  policy <- .dsomopDpPolicy()
  if (!.dsomopDpPlanOutputPersonLocal(plan, output_name, policy)) return(x)
  out <- plan$outputs[[output_name]]
  type <- tolower(out$type %||% "event_level")
  representation <- if (identical(type, "event_level")) {
    if (is.character(out$representation)) {
      tolower(out$representation)
    } else tolower(out$representation$format %||% "long")
  } else {
    "long"
  }
  lineage <- tryCatch(
    .dsomopDpPlanLineageSemantic(
      plan, output_name, policy, component = component
    ),
    error = function(e) NULL
  )
  if (is.null(lineage)) return(x)
  dataset_id <- .dsomopDpDatasetId(policy, dataset_identity)
  if (is.null(dataset_id)) return(x)
  episode_domain <- if ("cohort_row_id" %in% names(x)) {
    .dsomopDpLineageId(policy, dataset_id, list(
      kind = "plan_episode_domain",
      scope = lineage$scope,
      index_anchor = lineage$index_anchor,
      population = lineage$population
    ))
  } else NULL
  .dsomopDpSealPersonLocal(
    x, producer = paste(
      "plan", type, representation, component %||% "primary", sep = "/"
    ),
    episode_domain = episode_domain, lineage = lineage, policy = policy,
    dataset_id = dataset_id
  )
}

.DSOMOP_DP_SAFE_ASSIGN_QUERIES <- c(
  "condition_occurrence.load", "death.load", "drug_exposure.load",
  "measurement.load", "observation.load", "procedure_occurrence.load"
)

.DSOMOP_DP_SAFE_ASSIGN_SQL <- c(
  condition_occurrence.load =
    "e3bab37c5703c2f51e75ea4a13e08ae8726339b6fd72e0e97c75a17c0e9ce2ac",
  death.load =
    "24aedbb21fab7a7f398016839619161af50f8e6502ecc9ed39ffb090c3791b6a",
  drug_exposure.load =
    "90c69fb06285fb9742a697ac7d5f86f0429f62b346cd4366c5ab8ba46e04198e",
  measurement.load =
    "dba2a35842bf8c418a2e754c6fb0e6a0e5b36db1fded8478cd43c96b553a0202",
  observation.load =
    "2ac7580b5cce532d0eddd5968af041b919297e9eea8e04857ddfb6e707d1c81c",
  procedure_occurrence.load =
    "54942097e5642223bfb7d5768267d064fb1971e6d971a499b8a53f82ce176d70"
)

.dsomopDpSealAnalysisAssign <- function(x, entry, scope_present = FALSE,
                                        effective_params = list(),
                                        public_config = list(),
                                        dataset_identity = NULL) {
  attr(x, "dsomop_dp_provenance") <- NULL
  query_id <- entry$meta$query_id %||% ""
  sql <- entry$compute$sql %||% NULL
  sql_hash <- if (is.character(sql) && length(sql) == 1L && !is.na(sql)) {
    .dsomopDpSha256(sql)
  } else ""
  safe <- identical(entry$meta$adapter, "query") &&
    query_id %in% .DSOMOP_DP_SAFE_ASSIGN_QUERIES &&
    identical(unname(.DSOMOP_DP_SAFE_ASSIGN_SQL[[query_id]]), sql_hash) &&
    identical(scope_present, FALSE)
  if (!.dsomopDpEnabled() || !safe) return(x)
  .dsomopDpSealPersonLocal(
    x,
    producer = paste0("query/", query_id, "/", substr(sql_hash, 1L, 16L)),
    lineage = list(
      kind = "query_library_assign",
      query_id = query_id,
      sql_sha256 = sql_hash,
      effective_params = .dsomopDpLineageObject(effective_params),
      public_config = public_config
    ),
    dataset_identity = dataset_identity
  )
}

.dsomopDpResealUnary <- function(result, source, producer, args = list()) {
  attr(result, "dsomop_dp_provenance") <- NULL
  attr(result, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- NULL
  if (!.dsomopDpEnabled()) return(result)
  policy <- .dsomopDpPolicy()
  provenance <- .dsomopDpVerifyPersonLocal(
    source, policy = policy, allow_missing = TRUE
  )
  if (is.null(provenance)) return(result)
  if (identical(producer, "manipulate/filter")) {
    filter_state <- .dsomopDpFilterState(
      source, provenance = provenance, policy = policy
    )
    operator <- tolower(args$operator %||% "")
    # Equality/membership aliases have the same row semantics here, including
    # the explicit drop of NA comparisons. Give each pair one lineage.
    if (operator %in% c("==", "eq")) operator <- "in"
    if (operator %in% c("!=", "ne")) operator <- "not_in"
    args$operator <- operator
    if (operator %in% c("in", "not_in")) {
      args$value <- .dsomopDpLineageSet(args$value)
    }
    filter_leaf <- list(
      var = args$variable, op = args$operator, value = args$value
    )
    base_lineage <- if (is.null(filter_state)) {
      provenance$lineage_id
    } else filter_state$base_lineage
    filter_children <- if (is.null(filter_state)) {
      list(filter_leaf)
    } else list(filter_state$filter_tree, filter_leaf)
    filter_tree <- .dsomopDpNormalizeFilterTree(list(and = filter_children))
    filter_state <- list(
      version = 1L,
      base_lineage = base_lineage,
      filter_tree = filter_tree
    )
    result <- .dsomopDpSetFilterState(result, filter_state)
    return(.dsomopDpSealPersonLocal(
      result,
      producer = producer,
      episode_domain = provenance$episode_domain,
      lineage = list(
        kind = "filter_conjunction",
        version = 1L,
        parent = base_lineage,
        filter_tree = filter_tree
      ),
      policy = policy,
      dataset_id = provenance$dataset_id
    ))
  }
  .dsomopDpSealPersonLocal(
    result,
    producer = producer,
    episode_domain = provenance$episode_domain,
    lineage = list(
      kind = "unary_transform",
      parent = provenance$lineage_id,
      operation = producer,
      arguments = args
    ),
    policy = policy,
    dataset_id = provenance$dataset_id
  )
}

.dsomopDpSealInheritedLineage <- function(result, provenance, producer,
                                           policy) {
  attr(result, "dsomop_dp_provenance") <- NULL
  producer <- .dsomopDpProvenanceText(producer, "producer")
  .assertOmopTablePseudonymization(
    result, caller = ".dsomopDpSealInheritedLineage"
  )
  contract <- list(
    protocol = .DSOMOP_DP_PROVENANCE_PROTOCOL,
    adjacency = policy$adjacency,
    domain = policy$domain,
    snapshot_id = policy$snapshot_id,
    privacy_epoch = policy$privacy_epoch,
    person_key = provenance$person_key,
    dataset_id = provenance$dataset_id,
    producer = producer,
    episode_domain = provenance$episode_domain,
    lineage_id = provenance$lineage_id
  )
  attr(result, "dsomop_dp_provenance") <- c(contract, list(
    mac = .dsomopDpProvenanceMac(result, contract, policy)
  ))
  result
}

.dsomopDpResealProjection <- function(result, source) {
  attr(result, "dsomop_dp_provenance") <- NULL
  attr(result, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- NULL
  if (!.dsomopDpEnabled()) return(result)
  policy <- .dsomopDpPolicy()
  provenance <- .dsomopDpVerifyPersonLocal(
    source, policy = policy, allow_missing = TRUE
  )
  if (is.null(provenance)) return(result)
  valid <- identical(nrow(result), nrow(source)) &&
    identical(rownames(result), rownames(source)) &&
    !anyDuplicated(names(result)) && all(names(result) %in% names(source)) &&
    identical(.omopPersonKeys(result), .omopPersonKeys(source)) &&
    all(vapply(names(result), function(column) {
      identical(result[[column]], source[[column]])
    }, logical(1L)))
  if (!isTRUE(valid)) {
    stop("A DP projection changed rows or retained column values.",
         call. = FALSE)
  }
  result <- .dsomopDpSetFilterState(
    result,
    .dsomopDpFilterState(source, provenance = provenance, policy = policy)
  )
  .dsomopDpSealInheritedLineage(
    result, provenance, "manipulate/select", policy
  )
}

.dsomopDpResealConceptFactors <- function(result, source, applied_levels) {
  if (!is.list(applied_levels)) {
    stop("Concept-factor provenance requires a level-contract list.",
         call. = FALSE)
  }
  if (length(applied_levels) == 0L) return(source)
  attr(result, "dsomop_dp_provenance") <- NULL
  attr(result, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- NULL
  if (is.null(names(applied_levels)) ||
      anyNA(names(applied_levels)) || any(!nzchar(names(applied_levels))) ||
      anyDuplicated(names(applied_levels))) {
    stop("Concept-factor provenance requires uniquely named level contracts.",
         call. = FALSE)
  }
  applied_columns <- sort(names(applied_levels), method = "radix")
  applied_levels <- applied_levels[applied_columns]
  valid_levels <- vapply(applied_levels, function(levels) {
    is.character(levels) && length(levels) > 0L && !anyNA(levels) &&
      all(nzchar(levels)) && !anyDuplicated(levels)
  }, logical(1L))
  if (any(!valid_levels)) {
    stop("Concept-factor provenance received an invalid level contract.",
         call. = FALSE)
  }
  if (!.dsomopDpEnabled()) {
    return(result)
  }
  policy <- .dsomopDpPolicy()
  provenance <- .dsomopDpVerifyPersonLocal(
    source, policy = policy, allow_missing = TRUE
  )
  if (is.null(provenance)) {
    return(result)
  }
  if (length(applied_columns) == 0L) return(source)
  normalize_attributes <- function(value, remove = character(0)) {
    value <- value %||% list()
    value[remove] <- NULL
    if (length(value) == 0L) return(list())
    fields <- names(value)
    if (!is.null(fields)) value <- value[order(fields, method = "radix")]
    value
  }
  source_attributes <- attributes(source)
  result_attributes <- attributes(result)
  source_attributes[["dsomop_dp_provenance"]] <- NULL
  result_attributes[["dsomop_dp_provenance"]] <- NULL
  source_attributes[[.DSOMOP_DP_FILTER_STATE_ATTRIBUTE]] <- NULL
  result_attributes[[.DSOMOP_DP_FILTER_STATE_ATTRIBUTE]] <- NULL
  source_attributes <- normalize_attributes(source_attributes)
  result_attributes <- normalize_attributes(result_attributes)
  valid <- identical(nrow(result), nrow(source)) &&
    identical(names(result), names(source)) &&
    identical(rownames(result), rownames(source)) &&
    identical(source_attributes, result_attributes) &&
    all(applied_columns %in% names(source)) &&
    identical(.omopPersonKeys(result), .omopPersonKeys(source)) &&
    all(vapply(.omopPersonKeys(source), function(column) {
      identical(result[[column]], source[[column]])
    }, logical(1L)))
  if (isTRUE(valid)) {
    valid <- all(vapply(names(source), function(column) {
      before <- source[[column]]
      after <- result[[column]]
      if (!column %in% applied_columns) return(identical(before, after))
      expected_levels <- unname(applied_levels[[column]])
      before_values <- as.character(before)
      before_attributes <- normalize_attributes(
        attributes(before), c("class", "levels")
      )
      after_attributes <- normalize_attributes(
        attributes(after), c("class", "levels")
      )
      identical(class(after), "factor") &&
        identical(unname(levels(after)), expected_levels) &&
        all(is.na(before_values) | before_values %in% expected_levels) &&
        identical(as.character(after), before_values) &&
        identical(before_attributes, after_attributes)
    }, logical(1L)))
  }
  if (!isTRUE(valid)) {
    stop("Concept-factor recoding changed rows, identifiers, values, or ",
         "unauthorized column metadata.", call. = FALSE)
  }
  # Factor-level metadata changes representation semantics. Until that
  # transformation has one proven canonical identity, keep the recoded table
  # usable outside DP releases but withdraw authenticated DP provenance.
  result
}

.dsomopDpResealBind <- function(result, x, y) {
  attr(result, "dsomop_dp_provenance") <- NULL
  attr(result, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- NULL
  if (!.dsomopDpEnabled()) return(result)
  policy <- .dsomopDpPolicy()
  left <- .dsomopDpVerifyPersonLocal(
    x, policy = policy, allow_missing = TRUE
  )
  right <- .dsomopDpVerifyPersonLocal(
    y, policy = policy, allow_missing = TRUE
  )
  if (is.null(left) || is.null(right)) return(result)
  if (!identical(left$dataset_id, right$dataset_id)) return(result)
  # Row-bind has partition and duplicate representations that are not yet
  # canonicalized strongly enough for sticky releases.
  result
}

.dsomopDpMergePreflight <- function(x, y, by) {
  if (!.dsomopDpEnabled()) return(NULL)
  policy <- .dsomopDpPolicy()
  left <- .dsomopDpVerifyPersonLocal(
    x, policy = policy, allow_missing = TRUE
  )
  right <- .dsomopDpVerifyPersonLocal(
    y, policy = policy, allow_missing = TRUE
  )
  episode_join <- "cohort_row_id" %in% by
  if (episode_join && (is.null(left) || is.null(right) ||
      !identical(left$dataset_id, right$dataset_id) ||
      is.null(left$episode_domain) ||
      !identical(left$episode_domain, right$episode_domain))) {
    stop("omopMergeDS: cohort_row_id inputs do not share one authenticated ",
         "episode domain.", call. = FALSE)
  }
  list(policy = policy, left = left, right = right)
}

.dsomopDpResealMerge <- function(result, x, y, by, type,
                                  preflight = NULL) {
  attr(result, "dsomop_dp_provenance") <- NULL
  attr(result, .DSOMOP_DP_FILTER_STATE_ATTRIBUTE) <- NULL
  if (!.dsomopDpEnabled()) return(result)
  if (is.null(preflight)) {
    preflight <- .dsomopDpMergePreflight(x, y, by)
  }
  policy <- preflight$policy
  left <- preflight$left
  right <- preflight$right
  if (is.null(left) || is.null(right)) return(result)
  if (!identical(left$dataset_id, right$dataset_id)) return(result)
  # Join cardinality and operand representations are not canonicalized strongly
  # enough for sticky releases. Keep the ordinary result but fail DP closed.
  result
}
