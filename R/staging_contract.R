# Server-local staged dataset interoperability.

.stagedDescriptorPseudonymization <- function(contract) {
  if (is.null(contract)) return(NULL)
  if (!is.list(contract) || !isTRUE(contract$available) ||
      !is.character(contract$key_id) || length(contract$key_id) != 1L ||
      is.na(contract$key_id) ||
      !grepl("^dsomop-person-key-v1:[0-9a-f]{64}$", contract$key_id)) {
    stop("A staged dataset requires a valid public pseudonymization contract.",
         call. = FALSE)
  }
  epoch <- contract$epoch
  provider <- contract$provider %||% "legacy_handle"
  if (!is.character(provider) || length(provider) != 1L || is.na(provider) ||
      !provider %in% c("scoped", "injected", "file", "legacy_file",
                       "legacy", "legacy_handle")) {
    stop("The staged pseudonymization provider class is invalid.",
         call. = FALSE)
  }
  contract_version <- contract$contract_version
  if (!is.numeric(contract_version) || length(contract_version) != 1L ||
      is.na(contract_version) || !is.finite(contract_version) ||
      contract_version < 0 || contract_version != floor(contract_version)) {
    stop("The staged pseudonymization contract version is invalid.",
         call. = FALSE)
  }
  if (!is.null(epoch) &&
      (!is.numeric(epoch) || length(epoch) != 1L || is.na(epoch) ||
       !is.finite(epoch) || epoch < 1 || epoch != floor(epoch))) {
    stop("The staged pseudonymization epoch is invalid.", call. = FALSE)
  }
  list(
    token_protocol = "dsomop-person-token-v2",
    key_contract_version = as.integer(contract_version),
    key_id = contract$key_id,
    epoch = if (is.null(epoch)) NULL else as.integer(epoch),
    resource_scoped = provider %in%
      c("scoped", "injected", "file", "legacy_file")
  )
}

.validateStagedPseudonymization <- function(contract) {
  required <- c(
    "token_protocol", "key_contract_version", "key_id", "epoch",
    "resource_scoped"
  )
  if (!is.list(contract) || is.null(names(contract)) ||
      any(!nzchar(names(contract))) || anyDuplicated(names(contract)) ||
      !setequal(names(contract), required) || length(contract) != length(required) ||
      !identical(contract$token_protocol, "dsomop-person-token-v2") ||
      !is.numeric(contract$key_contract_version) ||
      length(contract$key_contract_version) != 1L ||
      is.na(contract$key_contract_version) ||
      !is.finite(contract$key_contract_version) ||
      contract$key_contract_version < 0 ||
      contract$key_contract_version != floor(contract$key_contract_version) ||
      !is.character(contract$key_id) || length(contract$key_id) != 1L ||
      is.na(contract$key_id) ||
      !grepl("^dsomop-person-key-v1:[0-9a-f]{64}$", contract$key_id) ||
      !is.logical(contract$resource_scoped) ||
      length(contract$resource_scoped) != 1L ||
      is.na(contract$resource_scoped)) {
    stop("Invalid staged pseudonymization contract.", call. = FALSE)
  }
  epoch <- contract$epoch
  if (!is.null(epoch) &&
      (!is.numeric(epoch) || length(epoch) != 1L || is.na(epoch) ||
       !is.finite(epoch) || epoch < 1 || epoch != floor(epoch))) {
    stop("Invalid staged pseudonymization epoch.", call. = FALSE)
  }
  list(
    token_protocol = "dsomop-person-token-v2",
    key_contract_version = as.integer(contract$key_contract_version),
    key_id = contract$key_id,
    epoch = if (is.null(epoch)) NULL else as.integer(epoch),
    resource_scoped = contract$resource_scoped
  )
}

#' Normalize a public pseudonymization contract
#'
#' Accepts either the handle-facing public contract or its canonical token
#' contract. Keeping this conversion in one place prevents in-memory tables and
#' staged descriptors from publishing subtly different compatibility metadata.
#'
#' @param contract Public pseudonymization contract.
#' @return Canonical non-secret token contract.
#' @keywords internal
.canonicalPseudonymizationContract <- function(contract) {
  if (is.list(contract) &&
      identical(contract$token_protocol, "dsomop-person-token-v2")) {
    return(.validateStagedPseudonymization(contract))
  }
  .validateStagedPseudonymization(
    .stagedDescriptorPseudonymization(contract)
  )
}

.stagedScalarString <- function(value, what) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    stop("Invalid staged descriptor ", what, ".", call. = FALSE)
  }
  value
}

.stagedExpiry <- function(value) {
  value <- .stagedScalarString(value, "expiry")
  if (!grepl(
    paste0("^[0-9]{4}-(0[1-9]|1[0-2])-([0-2][0-9]|3[01])T",
           "([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]\\.[0-9]{3}Z\\z"),
    value, perl = TRUE
  )) {
    stop("Invalid staged descriptor expiry.", call. = FALSE)
  }
  parsed <- suppressWarnings(as.POSIXct(
    value, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
  ))
  if (length(parsed) != 1L || is.na(parsed)) {
    stop("Invalid staged descriptor expiry.", call. = FALSE)
  }
  parsed
}

.stagedIsAbsolutePath <- function(path) {
  if (.Platform$OS.type == "windows") {
    grepl("^(?:[A-Za-z]:[/\\\\]|[/\\\\]{2})", path, perl = TRUE)
  } else {
    startsWith(path, "/")
  }
}

# POSIX `test -f` checks the inode type without opening the path, so a FIFO,
# socket or device cannot block the resolver. Symlinks are rejected separately.
.stagedIsRegularFile <- function(path) {
  test_bin <- Sys.which("test")
  if (!nzchar(test_bin)) {
    stop("Cannot verify the staged dataset file type.", call. = FALSE)
  }
  status <- suppressWarnings(tryCatch(
    system2(test_bin, c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE),
    error = function(e) NA_integer_
  ))
  if (length(status) != 1L || is.na(status)) {
    stop("Cannot verify the staged dataset file type.", call. = FALSE)
  }
  identical(as.integer(status), 0L)
}

.stagedScopeItems <- function(value, field) {
  if (is.null(value)) return(list())
  items <- if (is.data.frame(value) || .is_omop.table(value)) {
    list(value)
  } else if (is.list(value)) {
    unname(value)
  } else {
    as.list(unname(value))
  }
  if (any(vapply(items, is.null, logical(1L)))) {
    stop("A staged ", field, " declaration cannot contain NULL sources.",
         call. = FALSE)
  }
  items
}

.stagedScopeCohortSemantic <- function(value) {
  if ((is.numeric(value) && length(value) == 1L && !is.na(value)) ||
      (is.character(value) && length(value) == 1L && !is.na(value) &&
       grepl("^[0-9]+$", value))) {
    return(list(
      kind = "cohort_definition_id",
      value = .dsomopPlanCohortIdSemantic(
        value, field = "scope cohort_definition_id", minimum = 1L
      )
    ))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop("A staged scope cohort must be one cohort definition id or cohort ",
         "table name.", call. = FALSE)
  }
  list(
    kind = "cohort_table",
    value = .validateIdentifier(value, "staged scope cohort")
  )
}

.stagedPlanScopeSemantic <- function(plan) {
  scope <- plan$scope
  if (is.null(scope)) return(NULL)
  if (!is.list(scope) || is.data.frame(scope) || .is_omop.table(scope)) {
    stop("A staged plan scope must be a declarative scope list.",
         call. = FALSE)
  }
  combine <- tolower(.stagedScalarString(
    scope[["combine"]] %||% "union", "staged scope combine"
  ))
  if (!combine %in% c("union", "intersect")) {
    stop("A staged scope combine must be 'union' or 'intersect'.",
         call. = FALSE)
  }
  cohort_sources <- lapply(
    .stagedScopeItems(scope[["cohort"]], "scope cohort"),
    .stagedScopeCohortSemantic
  )
  table_sources <- lapply(
    .stagedScopeItems(scope[["tables"]], "scope table"),
    function(value) {
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        stop("Every staged scope table must be one public workspace symbol.",
             call. = FALSE)
      }
      list(
        kind = "workspace_table",
        symbol = .validateIdentifier(value, "staged scope table")
      )
    }
  )
  sources <- c(cohort_sources, table_sources)
  if (length(sources) == 0L) return(NULL)
  list(combine = combine, sources = unname(sources))
}

# A portable staged contract can bind live workspace scope only through the
# public declarations retained in plan$scope. Never derive compatibility from
# frame contents, attributes, DP lineage ids, resource handles or site names.
.validateStagedScopeDeclaration <- function(plan) {
  scope <- plan$scope
  if (is.null(scope)) return(invisible(TRUE))
  if (!is.list(scope) || is.data.frame(scope) || .is_omop.table(scope)) {
    stop("Staged output requires a portable declarative scope.",
         call. = FALSE)
  }
  .stagedPlanScopeSemantic(plan)
  declared_cohorts <- lapply(
    .stagedScopeItems(scope[["cohort"]], "scope cohort"),
    .stagedScopeCohortSemantic
  )
  declared_tables <- .stagedScopeItems(scope[["tables"]], "scope table")
  live <- scope[["tables_frames"]]
  if (is.null(live)) {
    if (length(declared_tables) > 0L) {
      stop("Staged workspace scope tables were not resolved from their public ",
           "declarations.", call. = FALSE)
    }
    return(invisible(TRUE))
  }
  live_sources <- Filter(
    Negate(is.null), .stagedScopeItems(live, "resolved scope")
  )
  live_is_table <- vapply(live_sources, .is_omop.table, logical(1L))
  live_cohorts <- lapply(
    live_sources[!live_is_table], .stagedScopeCohortSemantic
  )
  if (!identical(unname(live_cohorts), unname(declared_cohorts)) ||
      sum(live_is_table) != length(declared_tables)) {
    stop("Staged scope sources do not match the portable cohort/table ",
         "declarations in plan$scope.", call. = FALSE)
  }
  invisible(TRUE)
}

# Build a public, deterministic snapshot of the output semantics that another
# server-side package must compare before combining staged datasets. The opaque
# query digest binds the selected cohort/population/scope and every output-level
# selection/format option without publishing a second copy of the plan contract.
.stagedSemanticContract <- function(plan, output_name, component = NULL) {
  if (!is.list(plan) || !is.list(plan$outputs)) {
    stop("A staged semantic contract requires one extraction plan.",
         call. = FALSE)
  }
  output_name <- .validateIdentifier(output_name, "staged semantic output")
  output <- plan$outputs[[output_name]]
  if (!is.list(output)) {
    stop("A staged semantic contract requires a named plan output.",
         call. = FALSE)
  }
  output_type <- tolower(.stagedScalarString(
    output$type %||% "event_level", "semantic output type"
  ))
  allowed_types <- c(
    "person_level", "event_level", "baseline", "survival",
    "concept_dictionary", "cohort_membership", "intervals_long",
    "temporal_covariates", "person_period"
  )
  if (!output_type %in% allowed_types) {
    stop("Invalid staged semantic output type.", call. = FALSE)
  }

  representation <- output$representation
  representation_format <- if (is.list(representation)) {
    representation$format
  } else {
    representation
  }
  representation_grain <- if (is.list(representation)) {
    representation$grain
  } else {
    NULL
  }
  output_format <- switch(
    output_type,
    event_level = tolower(.stagedScalarString(
      representation_format %||% "long", "semantic output format"
    )),
    person_level = "wide",
    baseline = "wide",
    survival = {
      survival_format <- tolower(.stagedScalarString(
        output$format %||% "survival", "semantic survival format"
      ))
      if (survival_format %in% c("survival", "competing_risk")) {
        "wide"
      } else if (survival_format %in%
                 c("recurrent_events", "counting_process")) {
        "long"
      } else {
        stop("Invalid staged survival format.", call. = FALSE)
      }
    },
    concept_dictionary = "reference",
    cohort_membership = "long",
    intervals_long = "long",
    temporal_covariates = "sparse_temporal",
    person_period = "sparse_person_period"
  )

  index_age <- FALSE
  if (identical(output_type, "person_level")) {
    derived <- output$derived_columns %||% list()
    index_age <- any(vapply(derived, function(spec) {
      is.list(spec) && identical(tolower(spec$kind %||% ""), "age") &&
        identical(tolower(spec$reference %||% "today"), "index") &&
        is.null(spec$reference_date)
    }, logical(1)))
  }
  grain <- switch(
    output_type,
    person_level = if (index_age) "episode" else "person",
    event_level = if (identical(output_format, "long")) {
      if (!is.null(output$temporal$index_window)) "episode_event" else "event"
    } else {
      tolower(.stagedScalarString(
        representation_grain %||% "person", "semantic output grain"
      ))
    },
    baseline = "episode",
    survival = switch(
      tolower(output$format %||% "survival"),
      survival = if (is.null(output$outcomes)) "episode" else "episode_outcome",
      competing_risk = "episode",
      recurrent_events = "episode_event",
      counting_process = "episode_interval",
      stop("Invalid staged survival format.", call. = FALSE)
    ),
    concept_dictionary = "concept",
    cohort_membership = "episode",
    intervals_long = "episode_interval",
    temporal_covariates = "episode",
    person_period = "episode"
  )

  canonical_date_handling <- function(spec = NULL) {
    spec <- .normalizeDateHandling(
      spec %||% getOption("dsomop.default_date_handling", "remove")
    )
    ans <- list(mode = spec$mode, reference = spec$reference)
    if (!is.null(spec$bin_width)) ans$bin_width <- spec$bin_width
    if (!is.null(spec$date_columns)) {
      ans$date_columns <- sort(unique(tolower(spec$date_columns)))
    }
    ans
  }
  date_handling <- switch(
    output_type,
    event_level = canonical_date_handling(output$date_handling),
    person_level = canonical_date_handling(),
    cohort_membership = canonical_date_handling(output$date_handling),
    baseline = list(mode = "remove", reference = "index"),
    survival = list(mode = "relative", reference = "index",
                    unit = "calendar_day"),
    intervals_long = list(mode = "relative", reference = "index",
                          unit = "calendar_day"),
    temporal_covariates = list(
      mode = "relative_binned", reference = "index", unit = "calendar_day",
      bin_width = as.integer(output$bin_width %||% 30L),
      window_start = as.integer(output$window_start %||% -365L),
      window_end = as.integer(output$window_end %||% 0L)
    ),
    person_period = list(
      mode = "relative_binned", reference = "index", unit = "calendar_day",
      bin_width = as.integer(output$bin_width %||% 30L),
      window_start = as.integer(output$window_start %||% -365L),
      window_end = as.integer(output$window_end %||% 0L)
    ),
    concept_dictionary = list(mode = "not_applicable")
  )

  if (!is.null(component)) {
    component <- .stagedScalarString(component, "semantic output component")
    component_lower <- tolower(component)
    if (identical(component_lower, "personref")) {
      output_format <- "linkage"
      grain <- if (output_type %in% c("temporal_covariates", "person_period") ||
                   identical(tolower(representation_grain %||% ""),
                             "episode")) "episode" else "person"
    } else if (identical(output_type, "survival") &&
               component_lower %in% c("events", "risk_sets")) {
      if (identical(component_lower, "events")) {
        output_format <- "long"
        grain <- "episode_event"
      } else {
        output_format <- "wide"
        grain <- "episode"
      }
    } else {
      component_shape <- switch(
        component_lower,
        covariateref = c("reference", "concept"),
        timeref = c("reference", "time_bin"),
        personperiods = c("long", "episode_period"),
        temporalcovariates = c("sparse", "episode_time_concept"),
        NULL
      )
      if (!is.null(component_shape)) {
        output_format <- component_shape[[1L]]
        grain <- component_shape[[2L]]
      }
    }
  }

  settings <- .omopDisclosureSettings()
  uses_age <- identical(output_type, "baseline") &&
    "age_at_index" %in% tolower(output$derived %||% character(0))
  uses_age <- uses_age || (identical(output_type, "person_level") && index_age)
  age_breaks <- if (uses_age) {
    output$age_breaks %||% settings$age_breaks
  } else {
    NULL
  }
  population_id <- .stagedScalarString(
    output$population_id %||% "base", "staged output population_id"
  )
  query_semantics_sha256 <- .dsomopDpSha256(.dsomopDpCanonicalJson(
    .dsomopDpLineageValue(list(
      protocol = "dsomop-staged-query-semantics-v2",
      population_id = population_id,
      index_anchor = .dsomopDpPlanCohortSemantic(plan$cohort),
      population = .dsomopDpPlanPopulationSemantic(plan, population_id),
      scope = .stagedPlanScopeSemantic(plan),
      output = .dsomopDpPlanOutputSemantic(output)
    ))
  ))
  list(
    contract_version = "dsomop-staged-semantics-v2",
    output_type = output_type,
    output_format = output_format,
    component = component,
    grain = grain,
    date_handling = date_handling,
    query_semantics_sha256 = query_semantics_sha256,
    harmonization_contract_version = settings$harmonization_contract_version,
    age_semantics = settings$age_semantics,
    age_breaks = if (is.null(age_breaks)) NULL else as.integer(age_breaks),
    date_semantics = settings$date_semantics,
    date_granularity = settings$date_granularity,
    datetime_timezone = settings$datetime_timezone,
    week_start = settings$week_start
  )
}

# Validate and normalize the public semantic contract carried by every v2 plan
# descriptor. Consumers compare the returned canonical list, never a partial or
# caller-selected subset of fields.
.validateStagedSemanticContract <- function(contract) {
  fail <- function(detail = "") {
    stop("Invalid staged semantic contract", detail, ".", call. = FALSE)
  }
  required <- c(
    "contract_version", "output_type", "output_format", "component", "grain",
    "date_handling", "query_semantics_sha256",
    "harmonization_contract_version", "age_semantics", "age_breaks",
    "date_semantics", "date_granularity", "datetime_timezone", "week_start"
  )
  if (!is.list(contract) || is.null(names(contract)) ||
      any(!nzchar(names(contract))) || anyDuplicated(names(contract)) ||
      !setequal(names(contract), required) || length(contract) != length(required)) {
    fail()
  }
  scalar <- function(value, name) {
    if (!is.character(value) || length(value) != 1L || is.na(value) ||
        !nzchar(value)) fail(paste0(" ", name))
    value
  }

  contract_version <- scalar(contract$contract_version, "version")
  if (!identical(contract_version, "dsomop-staged-semantics-v2")) fail(" version")
  output_type <- scalar(contract$output_type, "output_type")
  allowed_types <- c(
    "person_level", "event_level", "baseline", "survival",
    "concept_dictionary", "cohort_membership", "intervals_long",
    "temporal_covariates", "person_period"
  )
  if (!output_type %in% allowed_types || output_type != tolower(output_type)) {
    fail(" output_type")
  }
  output_format <- scalar(contract$output_format, "output_format")
  allowed_formats <- c(
    "wide", "long", "features", "sparse", "reference", "linkage",
    "sparse_temporal", "sparse_person_period"
  )
  if (!output_format %in% allowed_formats || output_format != tolower(output_format)) {
    fail(" output_format")
  }
  component <- contract$component
  allowed_components <- c(
    "covariates", "covariateRef", "personRef", "temporalCovariates",
    "timeRef", "personPeriods", "events", "risk_sets"
  )
  if (!is.null(component) &&
      (!is.character(component) || length(component) != 1L ||
       is.na(component) || !component %in% allowed_components)) {
    fail(" component")
  }
  grain <- scalar(contract$grain, "grain")
  if (!grain %in% c(
    "person", "episode", "episode_outcome", "event", "episode_event", "concept",
    "episode_interval", "episode_period", "episode_time_concept", "time_bin"
  )) fail(" grain")
  if (!is.null(component)) {
    component_types <- list(
      covariates = "event_level",
      covariateRef = c("event_level", "temporal_covariates", "person_period"),
      personRef = c("event_level", "temporal_covariates", "person_period"),
      temporalCovariates = c("temporal_covariates", "person_period"),
      timeRef = c("temporal_covariates", "person_period"),
      personPeriods = "person_period",
      events = "survival",
      risk_sets = "survival"
    )
    component_shape <- switch(component,
      covariates = list(format = "sparse", grain = c("person", "episode")),
      covariateRef = list(format = "reference", grain = "concept"),
      personRef = list(format = "linkage", grain = c("person", "episode")),
      temporalCovariates = list(format = "sparse", grain = "episode_time_concept"),
      timeRef = list(format = "reference", grain = "time_bin"),
      personPeriods = list(format = "long", grain = "episode_period"),
      events = list(format = "long", grain = "episode_event"),
      risk_sets = list(format = "wide", grain = "episode")
    )
    if (!output_type %in% component_types[[component]] ||
        !identical(output_format, component_shape$format) ||
        !grain %in% component_shape$grain) {
      fail(" component shape")
    }
  } else {
    valid_shape <- switch(output_type,
      person_level = identical(output_format, "wide") &&
        grain %in% c("person", "episode"),
      event_level = if (identical(output_format, "long")) {
        grain %in% c("event", "episode_event")
      } else {
        output_format %in% c("wide", "features", "sparse") &&
          grain %in% c("person", "episode")
      },
      baseline = identical(output_format, "wide") && identical(grain, "episode"),
      survival = (identical(output_format, "wide") &&
                    grain %in% c("episode", "episode_outcome")) ||
        (identical(output_format, "long") &&
           grain %in% c("episode_event", "episode_interval")),
      concept_dictionary = identical(output_format, "reference") &&
        identical(grain, "concept"),
      cohort_membership = identical(output_format, "long") &&
        identical(grain, "episode"),
      intervals_long = identical(output_format, "long") &&
        identical(grain, "episode_interval"),
      temporal_covariates = identical(output_format, "sparse_temporal") &&
        identical(grain, "episode"),
      person_period = identical(output_format, "sparse_person_period") &&
        identical(grain, "episode"),
      FALSE
    )
    if (!isTRUE(valid_shape)) fail(" output shape")
  }

  date_handling <- contract$date_handling
  if (!is.list(date_handling) || is.null(names(date_handling)) ||
      any(!nzchar(names(date_handling))) || anyDuplicated(names(date_handling))) {
    fail(" date_handling")
  }

  query_semantics_sha256 <- scalar(
    contract$query_semantics_sha256, "query_semantics_sha256"
  )
  if (!grepl("^[0-9a-f]{64}$", query_semantics_sha256)) {
    fail(" query_semantics_sha256")
  }
  mode <- scalar(date_handling$mode, "date_handling mode")
  if (identical(mode, "relative_binned")) {
    if (!setequal(names(date_handling), c(
      "mode", "reference", "unit", "bin_width", "window_start", "window_end"
    )) || !identical(date_handling$reference, "index") ||
        !identical(date_handling$unit, "calendar_day")) {
      fail(" date_handling")
    }
    ints <- lapply(date_handling[c("bin_width", "window_start", "window_end")],
                   function(x) suppressWarnings(as.integer(x)))
    nums <- lapply(date_handling[c("bin_width", "window_start", "window_end")],
                   function(x) suppressWarnings(as.numeric(x)))
    valid_int <- vapply(seq_along(ints), function(i) {
      length(ints[[i]]) == 1L && !is.na(ints[[i]]) &&
        length(nums[[i]]) == 1L && is.finite(nums[[i]]) &&
        nums[[i]] == ints[[i]]
    }, logical(1))
    if (!all(valid_int) || ints$bin_width < 1L ||
        ints$window_start > ints$window_end) fail(" date_handling")
    date_handling <- list(
      mode = mode, reference = "index", unit = "calendar_day",
      bin_width = ints$bin_width, window_start = ints$window_start,
      window_end = ints$window_end
    )
  } else if (identical(mode, "not_applicable")) {
    if (!identical(names(date_handling), "mode")) fail(" date_handling")
    date_handling <- list(mode = mode)
  } else if (identical(mode, "relative") && "unit" %in% names(date_handling)) {
    if (!setequal(names(date_handling), c("mode", "reference", "unit")) ||
        !identical(date_handling$reference, "index") ||
        !identical(date_handling$unit, "calendar_day")) {
      fail(" date_handling")
    }
    date_handling <- list(mode = mode, reference = "index", unit = "calendar_day")
  } else {
    normalized <- tryCatch(.normalizeDateHandling(date_handling),
                           error = function(e) NULL)
    if (is.null(normalized)) fail(" date_handling")
    date_handling <- list(mode = normalized$mode, reference = normalized$reference)
    if (!is.null(normalized$bin_width)) {
      date_handling$bin_width <- normalized$bin_width
    }
    if (!is.null(normalized$date_columns)) {
      date_handling$date_columns <- sort(unique(tolower(normalized$date_columns)))
    }
  }

  settings <- .omopDisclosureSettings()
  harmonization_fields <- c(
    "harmonization_contract_version", "age_semantics", "date_semantics",
    "date_granularity", "datetime_timezone", "week_start"
  )
  harmonization <- lapply(harmonization_fields, function(name) {
    value <- scalar(contract[[name]], name)
    if (!identical(value, settings[[name]])) fail(paste0(" ", name))
    value
  })
  names(harmonization) <- harmonization_fields
  if (is.null(contract$age_breaks)) {
    age_breaks <- NULL
  } else {
    age_numeric <- suppressWarnings(as.numeric(contract$age_breaks))
    age_breaks <- suppressWarnings(as.integer(contract$age_breaks))
    if (length(age_breaks) < 2L || anyNA(age_breaks) ||
        any(!is.finite(age_numeric)) || any(age_numeric != age_breaks) ||
        age_breaks[[1L]] != 0L || any(diff(age_breaks) <= 0L) ||
        !all(age_breaks %in% as.integer(settings$age_breaks))) {
      fail(" age_breaks")
    }
  }

  list(
    contract_version = contract_version,
    output_type = output_type,
    output_format = output_format,
    component = component,
    grain = grain,
    date_handling = date_handling,
    query_semantics_sha256 = query_semantics_sha256,
    harmonization_contract_version = harmonization$harmonization_contract_version,
    age_semantics = harmonization$age_semantics,
    age_breaks = age_breaks,
    date_semantics = harmonization$date_semantics,
    date_granularity = harmonization$date_granularity,
    datetime_timezone = harmonization$datetime_timezone,
    week_start = harmonization$week_start
  )
}

#' Build an output-level staged bundle contract
#'
#' A composite plan output may be materialized as several files with deliberately
#' different component shapes (for example personRef vs temporalCovariates).
#' This output-level contract binds those siblings to one plan output and one
#' high-entropy staging token while retaining the shared semantic settings.
#'
#' @param plan Extraction plan carrying the output and population semantics.
#' @param output_name Logical plan output name, without a component suffix.
#' @param token High-entropy staging token.
#' @return Canonical staged bundle contract.
#' @keywords internal
.stagedBundleContract <- function(plan, output_name, token) {
  output_name <- .validateIdentifier(output_name, "staged bundle output")
  token <- .stagedScalarString(token, "bundle token")
  if (!grepl("^stg_[0-9a-f]{32}$", token)) {
    stop("Invalid staged bundle token.", call. = FALSE)
  }
  list(
    contract_version = "dsomop-staged-bundle-v1",
    output_id = output_name,
    staged_token = token,
    semantic_contract = .validateStagedSemanticContract(
      .stagedSemanticContract(plan, output_name, component = NULL)
    )
  )
}

#' Validate an output-level staged bundle contract
#'
#' @param contract Bundle contract to validate.
#' @param dataset_id Optional descriptor dataset id to bind to the output.
#' @param staged_token Optional descriptor token to bind to the bundle.
#' @param semantic_contract Optional component semantic contract. Shared fields
#'   must match, while component shape fields may deliberately differ.
#' @return Canonical staged bundle contract.
#' @keywords internal
.validateStagedBundleContract <- function(contract, dataset_id = NULL,
                                           staged_token = NULL,
                                           semantic_contract = NULL) {
  fail <- function(detail = "") {
    stop("Invalid staged bundle contract", detail, ".", call. = FALSE)
  }
  required <- c(
    "contract_version", "output_id", "staged_token", "semantic_contract"
  )
  if (!is.list(contract) || is.null(names(contract)) ||
      any(!nzchar(names(contract))) || anyDuplicated(names(contract)) ||
      !setequal(names(contract), required) || length(contract) != length(required)) {
    fail()
  }
  if (!identical(contract$contract_version, "dsomop-staged-bundle-v1")) {
    fail(" version")
  }
  output_id <- tryCatch(
    .validateIdentifier(contract$output_id, "staged bundle output"),
    error = function(e) NULL
  )
  if (is.null(output_id)) fail(" output_id")
  token <- tryCatch(
    .stagedScalarString(contract$staged_token, "bundle token"),
    error = function(e) NULL
  )
  if (is.null(token) || !grepl("^stg_[0-9a-f]{32}$", token)) {
    fail(" token")
  }
  semantics <- tryCatch(
    .validateStagedSemanticContract(contract$semantic_contract),
    error = function(e) NULL
  )
  if (is.null(semantics) || !is.null(semantics$component)) {
    fail(" semantics")
  }

  if (!is.null(dataset_id)) {
    dataset_id <- tryCatch(
      .stagedScalarString(dataset_id, "dataset_id"),
      error = function(e) NULL
    )
    expected_id <- paste0("omop.plan.", output_id)
    if (is.null(dataset_id) ||
        !(identical(dataset_id, expected_id) ||
          startsWith(dataset_id, paste0(expected_id, ".")))) {
      fail(" output_id")
    }
  }
  if (!is.null(staged_token) && !identical(token, staged_token)) {
    fail(" token")
  }
  if (!is.null(semantic_contract)) {
    component_semantics <- tryCatch(
      .validateStagedSemanticContract(semantic_contract),
      error = function(e) NULL
    )
    if (is.null(component_semantics)) fail(" semantics")
    shape_fields <- c("output_format", "component", "grain")
    shared_fields <- setdiff(names(semantics), shape_fields)
    if (!identical(semantics[shared_fields],
                   component_semantics[shared_fields])) {
      fail(" semantics")
    }
    if (is.null(component_semantics$component) &&
        !identical(semantics, component_semantics)) {
      fail(" semantics")
    }
  }

  list(
    contract_version = "dsomop-staged-bundle-v1",
    output_id = output_id,
    staged_token = token,
    semantic_contract = semantics
  )
}

#' Resolve a validated server-local staged dataset path
#'
#' Validates a dsOMOP staged descriptor before another server package opens its
#' Parquet or CSV file. The resolver ignores no descriptor identity or path
#' components: origin, dataset id, source kind, token, directory, absolute file
#' name, format, owner-only permissions, regular-file type, expiry and optional
#' pseudonym-key identity must all agree. Version-2 descriptors additionally
#' require canonical component and bundle contracts. A consumer can compare
#' exact component semantics, or compare the output-level bundle contract when
#' sibling files intentionally have different component shapes.
#' Person-bearing files require a resource-scoped pseudonymization provider even
#' when no expected key is supplied. It therefore must be used instead of opening
#' \code{descriptor$metadata$file} directly.
#'
#' This is a server-local interoperability utility, not a DataSHIELD download
#' method. The returned path remains on the data node and is not safe to expose
#' through an aggregate method.
#'
#' For a join between semantically equivalent staged datasets, take
#' \code{key_id}, \code{epoch}, and the semantic contract from the first
#' descriptor and pass them as the expected values for every other descriptor.
#' When joining sibling components of one logical output, compare
#' \code{key_id}, \code{epoch}, and the output-level bundle contract instead,
#' because sibling component shapes are intentionally different. A mismatch
#' fails closed.
#'
#' @param descriptor A \code{FlowerDatasetDescriptor} or
#'   \code{OMOPStagedDatasetDescriptor} produced by dsOMOP.
#' @param expected_key_id Optional non-secret pseudonymization key identifier.
#' @param expected_epoch Optional positive integer pseudonymization epoch.
#' @param expected_semantic_contract Optional canonical staged semantic contract
#'   for semantically equivalent datasets. When supplied, the descriptor must
#'   match it exactly after validation; do not use it to compare different
#'   sibling components of one output bundle.
#' @param expected_bundle_contract Optional canonical output-level bundle
#'   contract for sibling components. When supplied, the descriptor must belong
#'   to the same logical output and staging token; component-level semantic
#'   equality is deliberately not required.
#' @return Normalized server-local file path.
#' @export
omopStagedDatasetPath <- function(descriptor, expected_key_id = NULL,
                                  expected_epoch = NULL,
                                  expected_semantic_contract = NULL,
                                  expected_bundle_contract = NULL) {
  if (!is.list(descriptor) ||
      !inherits(descriptor, c("FlowerDatasetDescriptor",
                             "OMOPStagedDatasetDescriptor"))) {
    stop("A dsOMOP staged dataset descriptor is required.", call. = FALSE)
  }
  version <- descriptor$contract_version
  if (!is.numeric(version) || length(version) != 1L || is.na(version) ||
      !version %in% c(1L, 2L)) {
    stop("Unsupported staged descriptor contract version.", call. = FALSE)
  }
  origin <- .stagedScalarString(descriptor$origin, "origin")
  if (!identical(origin, "dsOMOP")) {
    stop("Invalid staged descriptor origin.", call. = FALSE)
  }
  dataset_id <- .stagedScalarString(descriptor$dataset_id, "dataset_id")
  if (!grepl("^omop\\.plan\\.[A-Za-z_][A-Za-z0-9_.]*$", dataset_id)) {
    stop("Invalid staged descriptor dataset_id.", call. = FALSE)
  }
  token <- .stagedScalarString(descriptor$staged_token, "token")
  if (!grepl("^stg_[0-9a-f]{32}$", token)) {
    stop("Invalid staged descriptor token.", call. = FALSE)
  }
  metadata <- descriptor$metadata
  if (!is.list(metadata)) {
    stop("Invalid staged descriptor metadata.", call. = FALSE)
  }
  format <- tolower(.stagedScalarString(metadata$format, "format"))
  if (!format %in% c("parquet", "csv")) {
    stop("Unsupported staged dataset format.", call. = FALSE)
  }
  source_kind <- .stagedScalarString(descriptor$source_kind, "source_kind")
  if (!identical(source_kind, paste0("staged_", format))) {
    stop("The staged descriptor source_kind does not match its format.",
         call. = FALSE)
  }
  columns <- metadata$columns
  if (!is.character(columns) || anyNA(columns) || any(!nzchar(columns)) ||
      anyDuplicated(columns)) {
    stop("Invalid staged descriptor column contract.", call. = FALSE)
  }
  expires <- .stagedExpiry(descriptor$expires_at)
  if (expires <= Sys.time()) {
    stop("The staged dataset descriptor has expired.", call. = FALSE)
  }
  semantic_contract <- metadata$semantic_contract
  if (version >= 2L && is.null(semantic_contract)) {
    stop("A v2 staged dataset lacks its semantic contract.", call. = FALSE)
  }
  if (!is.null(semantic_contract)) {
    semantic_contract <- .validateStagedSemanticContract(semantic_contract)
  }
  if (!is.null(expected_semantic_contract)) {
    expected_semantic_contract <- .validateStagedSemanticContract(
      expected_semantic_contract
    )
    if (is.null(semantic_contract) ||
        !identical(semantic_contract, expected_semantic_contract)) {
      stop("The staged dataset uses an incompatible semantic contract.",
           call. = FALSE)
    }
  }
  bundle_contract <- metadata$bundle_contract
  if (version >= 2L && is.null(bundle_contract)) {
    stop("A v2 staged dataset lacks its bundle contract.", call. = FALSE)
  }
  if (!is.null(bundle_contract)) {
    bundle_contract <- .validateStagedBundleContract(
      bundle_contract,
      dataset_id = dataset_id,
      staged_token = token,
      semantic_contract = semantic_contract
    )
  }
  if (!is.null(expected_bundle_contract)) {
    expected_bundle_contract <- .validateStagedBundleContract(
      expected_bundle_contract
    )
    if (is.null(bundle_contract) ||
        !identical(bundle_contract, expected_bundle_contract)) {
      stop("The staged dataset uses an incompatible bundle contract.",
           call. = FALSE)
    }
  }

  layout <- tolower(.stagedScalarString(metadata$layout %||% "file", "layout"))
  if (!identical(layout, "file")) {
    stop("Unsupported staged dataset layout.", call. = FALSE)
  }
  file <- .stagedScalarString(metadata$file, "file")
  if (!.stagedIsAbsolutePath(file)) {
    stop("The staged descriptor file must be one absolute server path.",
         call. = FALSE)
  }
  file_name <- basename(file)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_.]*\\.(parquet|csv)$", file_name) ||
      !identical(tolower(tools::file_ext(file_name)), format)) {
    stop("The staged descriptor file name does not match its format.",
         call. = FALSE)
  }
  base <- .stagingBaseDir()
  expected_dir <- file.path(base, token)
  if (!dir.exists(expected_dir) || .isSymbolicLink(expected_dir) ||
      !file.exists(file) || .isSymbolicLink(file)) {
    stop("The staged dataset file is unavailable or unsafe.", call. = FALSE)
  }
  actual_dir <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  expected_dir <- normalizePath(expected_dir, winslash = "/", mustWork = TRUE)
  actual_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  if (!identical(actual_dir, expected_dir) ||
      !identical(dirname(actual_file), expected_dir) ||
      !identical(basename(actual_file), file_name)) {
    stop("The staged descriptor path escapes its token directory.",
         call. = FALSE)
  }
  current_uid <- NULL
  if (.Platform$OS.type == "unix") {
    dir_info <- file.info(expected_dir)
    current_uid <- .dsomopEffectiveUid()
    expected_dir_mode <- as.integer(strtoi("700", base = 8L))
    if (nrow(dir_info) != 1L || !isTRUE(dir_info$isdir[[1L]]) ||
        is.na(dir_info$mode[[1L]]) || is.na(dir_info$uid[[1L]]) ||
        !identical(as.integer(dir_info$mode[[1L]]), expected_dir_mode) ||
        !identical(as.integer(dir_info$uid[[1L]]), current_uid)) {
      stop("The staged token directory must be owner-only (0700) and owned ",
           "by the server R user.", call. = FALSE)
    }
  }
  info <- file.info(actual_file)
  if (nrow(info) != 1L || isTRUE(info$isdir[[1L]]) ||
      is.na(info$size[[1L]])) {
    stop("The staged dataset is not a regular file.", call. = FALSE)
  }
  if (.Platform$OS.type == "unix") {
    if (!.stagedIsRegularFile(actual_file)) {
      stop("The staged dataset is not a regular file.", call. = FALSE)
    }
    expected_mode <- as.integer(strtoi("600", base = 8L))
    if (is.na(info$mode[[1L]]) || is.na(info$uid[[1L]]) ||
        !identical(as.integer(info$mode[[1L]]), expected_mode) ||
        !identical(as.integer(info$uid[[1L]]), current_uid) ||
        !identical(.dsomopLinkCount(actual_file), 1)) {
      stop("The staged dataset must be an owner-only file without hard links.",
           call. = FALSE)
    }
  }

  pseudonymization <- metadata$pseudonymization
  person_bearing <- any(tolower(columns) %in% .PERSON_KEY_COLS())
  if (person_bearing &&
      (version < 2L || !is.list(pseudonymization) ||
       !identical(pseudonymization$token_protocol,
                  "dsomop-person-token-v2"))) {
    stop("A person-bearing staged dataset lacks its pseudonymization contract.",
         call. = FALSE)
  }
  if (!is.null(pseudonymization)) {
    pseudonymization <- .validateStagedPseudonymization(pseudonymization)
  }
  if (person_bearing && !isTRUE(pseudonymization$resource_scoped)) {
    stop("A person-bearing staged dataset requires a resource-scoped ",
         "pseudonymization provider.", call. = FALSE)
  }
  if (!is.null(expected_key_id)) {
    expected_key_id <- .stagedScalarString(expected_key_id,
                                            "expected key identifier")
    if (!grepl("^dsomop-person-key-v1:[0-9a-f]{64}$", expected_key_id)) {
      stop("Invalid staged descriptor expected key identifier.",
           call. = FALSE)
    }
    if (!is.list(pseudonymization) ||
        !identical(pseudonymization$key_id, expected_key_id)) {
      stop("The staged dataset uses an incompatible pseudonymization key.",
           call. = FALSE)
    }
    if (!isTRUE(pseudonymization$resource_scoped)) {
      stop("The staged dataset uses a legacy global or unknown-scope key; ",
           "cross-descriptor compatibility cannot be established safely.",
           call. = FALSE)
    }
  }
  if (!is.null(expected_epoch)) {
    if (!is.numeric(expected_epoch) || length(expected_epoch) != 1L ||
        is.na(expected_epoch) || !is.finite(expected_epoch) ||
        expected_epoch < 1 || expected_epoch != floor(expected_epoch) ||
        !is.list(pseudonymization) ||
        !identical(as.integer(pseudonymization$epoch),
                   as.integer(expected_epoch))) {
      stop("The staged dataset uses an incompatible pseudonymization epoch.",
           call. = FALSE)
    }
  }
  actual_file
}
