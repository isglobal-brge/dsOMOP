# 2.10.6 deliberately changes concept storage, including untranslated outputs.
# Assert the new public factor contract, then project only that representation
# for immutable historical value/type oracles. All nonconcept values, row order,
# staged cell values and unrelated attributes retain the original checks. CSV
# quoting changes for new factor columns are checked against immutable bytes.
.legacy_concept_csv_bytes <- function(descriptor) {
  encoded <- descriptor$metadata$concept_presentation
  if(is.null(encoded) || !identical(descriptor$metadata$format,"csv"))return(NULL)
  capsule <- .ds_arg(encoded)
  contracts <- capsule$contracts %||% list()
  migrated <- names(contracts)[vapply(contracts,function(c)is.null(c$kind)&&isFALSE(c$translate),logical(1))]
  if(!length(migrated))return(NULL)
  positions <- match(unlist(capsule$names[migrated],use.names=FALSE),descriptor$metadata$columns)
  path <- descriptor$metadata$file
  bytes <- readChar(path,file.info(path)$size,useBytes=TRUE)
  lines <- strsplit(bytes,"\n",fixed=TRUE)[[1L]]
  for(i in seq_along(lines)[-1L]) {
    fields <- strsplit(lines[i],',(?=(?:[^"]*"[^"]*")*[^"]*$)',perl=TRUE)[[1L]]
    stopifnot(length(fields)==length(descriptor$metadata$columns))
    for(j in positions)if(grepl('^"[0-9]+"$',fields[j]))fields[j] <- substring(fields[j],2L,nchar(fields[j])-1L)
    lines[i] <- paste(fields,collapse=",")
  }
  projected <- paste0(paste(lines,collapse="\n"),if(endsWith(bytes,"\n"))"\n" else "")
  digest <- unclass(as.character(openssl::sha256(charToRaw(projected))))
  unclass(as.character(openssl::sha256(charToRaw(digest))))
}

.expect_legacy_concept_ids <- function(actual, expected, info = "2.10.6 concept migration") {
  check <- function(column, contract) {
    expect_true(inherits(column, "dsomop_concept_factor"), info = info)
    expect_false(is.ordered(column), info = info)
    expect_identical(as.character(column), as.character(contract$ids), info = info)
    ids <- as.character(unlist(contract$resolved$ids, use.names = FALSE))
    expect_identical(levels(column), ids[.conceptIdOrder(ids)], info = info)
  }
  if (is.data.frame(actual) && is.data.frame(expected)) {
    contracts <- attr(actual, "omop_concepts") %||% list()
    migrated <- names(contracts)[vapply(contracts, function(c) is.null(c$kind) && isFALSE(c$translate), logical(1))]
    for (col in intersect(migrated, names(expected))) {
      check(actual[[col]], contracts[[col]])
      ids <- contracts[[col]]$ids
      actual[[col]] <- switch(typeof(expected[[col]]), integer=as.integer(ids), double=as.numeric(ids), character=as.character(ids), stop("Unsupported historical concept storage"))
      contracts[[col]] <- NULL
    }
    attr(actual, "omop_concepts") <- if(length(contracts))contracts else NULL
    if (is.null(attr(expected,"omop_concept_cols"))) {
      remaining <- intersect(attr(actual,"omop_concept_cols"),names(contracts))
      attr(actual,"omop_concept_cols") <- if(length(remaining))remaining else NULL
    }
    if (is.null(attr(expected,"omop_concept_reference")) && !is.null(attr(actual,"omop_concept_reference"))) {
      expect_true(isTRUE(attr(actual,"omop_concept_reference")),info=info)
      attr(actual,"omop_concept_reference") <- NULL
    }
    if(is.null(attr(expected,"omop_dictionary_declared")) && !is.null(attr(actual,"omop_dictionary_declared"))) {
      expect_true(all(attr(actual,"omop_dictionary_declared") %in% as.character(actual$concept_id)),info=info)
      attr(actual,"omop_dictionary_declared") <- NULL
    }
    return(actual)
  }
  if (inherits(actual,"b10_staged") && inherits(expected,"b10_staged")) {
    encoded <- actual$descriptor$metadata$concept_presentation
    if (!is.null(encoded) && is.null(expected$descriptor$metadata$concept_presentation)) {
      capsule <- .ds_arg(encoded)
      expect_identical(as.integer(capsule$protocol),1L,info=info)
      contracts <- capsule$contracts %||% list()
      for(col in names(contracts)) {
        contract <- contracts[[col]]
        expect_true(is.null(contract$kind) && isFALSE(contract$translate),info=info)
        physical <- as.character(capsule$names[[col]])
        expect_identical(actual$descriptor$metadata$column_types[[physical]],"integer|dsomop_concept_factor/factor",info=info)
        ids <- as.character(unlist(contract$resolved$ids,use.names=FALSE))
        expect_identical(as.character(unlist(contract$resolved$levels,use.names=FALSE)),ids[.conceptIdOrder(ids)],info=info)
        expect_true(all(actual$staged_file$content[[physical]] %in% c(ids,"","NA")),info=info)
        actual$descriptor$metadata$column_types[[physical]] <- expected$descriptor$metadata$column_types[[physical]]
      }
      if(!is.null(actual$staged_file$legacy_concept_bytes_sha256)) {
        # Outputs that already wrote quoted ID text keep their exact historical
        # bytes. Otherwise remove only numeric-ID field quotes from the actual raw
        # CSV bytes; person tokens and every unrelated field remain byte-for-byte exact.
        if(!identical(actual$staged_file$bytes_sha256,expected$staged_file$bytes_sha256)) {
          expect_identical(actual$staged_file$legacy_concept_bytes_sha256,expected$staged_file$bytes_sha256,info=info)
          actual$staged_file$bytes_sha256 <- actual$staged_file$legacy_concept_bytes_sha256
        }
        actual$staged_file$legacy_concept_bytes_sha256 <- NULL
      }
      actual$descriptor$metadata$concept_presentation <- NULL
    }
    return(actual)
  }
  if(is.list(actual) && is.list(expected) && !is.data.frame(actual)) {
    for (i in seq_along(actual)) {
      j <- if(is.null(names(actual)))i else match(names(actual)[i],names(expected))
      if(!is.na(j) && j <= length(expected)) actual[i] <- list(.expect_legacy_concept_ids(actual[[i]],expected[[j]],info))
    }
  }
  actual
}
