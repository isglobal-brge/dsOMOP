# Module: the data generation of the fixed synopses (dsOMOP 2.10.3, R02 / §5)
#
# A fixed synopsis is keyed on a secret digest of the exact multiset of
# person identifiers of the CDM person table, recomputed inside each request
# that uses the fixed route, within that request's read snapshot
# (read_snapshot.R), and passed to its builders as a request-local value.
# Nothing is cached across requests, handles or sessions, and a failed read
# fails only the current request. Adding, removing or replacing a person
# changes the generation, so every fixed synopsis redraws at once; clinical
# rows of an unchanged person set do not change it. The per-component
# contribution fingerprints (tools_release.R) still redraw any component whose
# bounded contributions changed.
#
# The digest hashes canonical ordered (person_id, multiplicity) records in
# fixed public identifier buckets, then hashes the ordered bucket digests.
# This is a cryptographic hash tree, not XOR or sums of hashes. The four
# native adapters compute the leaves in SQL; other engines stream bounded
# pages. All engines produce the same digest. Only R receives the root key.

.DSOMOP_DP_GENERATION_SOURCE <- "person_membership_digest"
.DSOMOP_DP_GENERATION_BASIS <- paste0(
  "HMAC under the noise root of SHA-256 digests of canonically ordered ",
  "person_id and multiplicity records in fixed public identifier buckets, ",
  "recomputed inside each fixed-synopsis request's own read snapshot; ",
  "nothing is cached between requests and the digest is never released")
.DSOMOP_DP_GENERATION_RESIDUAL <- paste0(
  "The generation follows the person set only. Clinical-only changes that ",
  "leave the person set unchanged (in-place edits, added or removed records ",
  "of existing persons) keep the generation: the fixed-synopsis components ",
  "whose bounded contributions changed redraw one by one (their noise is ",
  "also keyed on their realised contributions). Comparing answers can reveal ",
  "component change/equality patterns; auxiliary information can reveal more. ",
  "This is not a joint guarantee over edits. Each new person set is another ",
  "data state whose fixed synopses ",
  "are another disclosure.")
.DSOMOP_DP_MEMBERSHIP_BUCKET <- 10000L
.DSOMOP_DP_MEMBERSHIP_PAGE <- 10000L
.DSOMOP_DP_GENERATION_ERROR <- paste0(
  "The protected release could not be prepared for this request (its data ",
  "generation could not be read); no value was released. Retry the request.")

# Exact non-negative integer text of an aggregate.
.dsomopDpMembershipInteger <- function(x) {
  x <- as.character(x)
  if (length(x) != 1L || is.na(x)) stop("A membership aggregate is malformed.")
  x <- sub("\\.0+$", "", trimws(x))
  if (!grepl("^[0-9]+$", x)) stop("A membership aggregate is malformed.")
  sub("^0+(?=[0-9])", "", x, perl = TRUE)
}

# Public buckets use exact decimal truncation toward zero, including 64-bit
# identifiers that cannot be represented by an R double. A bucket has at most
# 19,999 distinct signed integer IDs (the zero bucket); all other buckets
# have 10,000. Multiplicity is framed in each record, so duplicate rows cannot
# cancel. Decimal integers and colon/newline separators are unambiguous.
.dsomopDpMembershipBucket <- function(ids) {
  negative <- startsWith(ids, "-")
  digits <- sub("^-", "", ids)
  out <- ifelse(nchar(digits) <= 4L, "0",
                substr(digits, 1L, pmax(0L, nchar(digits) - 4L)))
  ifelse(negative & out != "0", paste0("-", out), out)
}

#' In-database SQL of the ordered person-membership hash leaves
#' @param handle CDM handle.
#' @return One SELECT of ordered bucket digests, or NULL for streaming engines.
#' @keywords internal
.dsomopDpMembershipSql <- function(handle) {
  dbms <- .normalizeDBMS(handle$dbms %||% "")
  person <- .qualifyTable(handle, "person")
  rows <- paste0("SELECT person_id AS p, COUNT(*) AS k FROM ", person,
                 " WHERE person_id IS NOT NULL GROUP BY person_id")
  if (identical(dbms, "postgresql")) {
    return(paste0(
      "SELECT CAST(p / 10000 AS TEXT) AS bucket, CAST(SUM(k) AS TEXT) AS n, ",
      "encode(sha256(convert_to(string_agg(CAST(p AS TEXT) || ':' || ",
      "CAST(k AS TEXT) || chr(10), '' ORDER BY p), 'UTF8')), 'hex') AS digest ",
      "FROM (", rows, ") dsomop_membership GROUP BY p / 10000 ORDER BY p / 10000"))
  }
  if (identical(dbms, "duckdb")) {
    return(paste0(
      "SELECT CAST(p // 10000 AS VARCHAR) AS bucket, CAST(SUM(k) AS VARCHAR) AS n, ",
      "sha256(string_agg(CAST(p AS VARCHAR) || ':' || CAST(k AS VARCHAR) || ",
      "chr(10), '' ORDER BY p)) AS digest FROM (", rows,
      ") dsomop_membership GROUP BY p // 10000 ORDER BY p // 10000"))
  }
  if (dbms %in% c("mysql", "mariadb")) {
    record <- "CONCAT(CAST(p AS CHAR), ':', CAST(k AS CHAR), CHAR(10))"
    payload <- paste0("GROUP_CONCAT(", record, " ORDER BY p SEPARATOR '')")
    return(paste0(
      "SELECT CAST(p DIV 10000 AS CHAR) AS bucket, CAST(SUM(k) AS CHAR) AS n, ",
      "SHA2(", payload, ", 256) AS digest, LENGTH(", payload,
      ") AS bytes, SUM(LENGTH(", record, ")) AS expected_bytes FROM (", rows,
      ") dsomop_membership GROUP BY CAST(p DIV 10000 AS CHAR) ORDER BY MIN(p DIV 10000)"))
  }
  NULL
}

# Consume leaves in numeric bucket order with constant memory. Each chain
# step binds the previous digest, bucket, count and complete SHA-256 leaf;
# the empty digest has its own domain. The final HMAC is applied only in R.
.dsomopDpMembershipAccumulator <- function() {
  total <- 0
  digest <- .dsomopDpSha256("dsomop-person-membership-ordered-v2:empty")
  add <- function(bucket, n, leaf) {
    n <- .dsomopDpMembershipInteger(n)
    if (length(bucket) != 1L || !grepl("^-?[0-9]+$", bucket) ||
        length(leaf) != 1L || is.na(leaf) || !grepl("^[0-9a-f]{64}$", leaf)) {
      stop("A membership digest leaf is malformed.")
    }
    total <<- total + as.numeric(n)
    if (!is.finite(total) || total >= 2^53) stop("Person count exceeds exact range.")
    digest <<- .dsomopDpSha256(.dsomopFrame(
      "dsomop-person-membership-ordered-v2:leaf", digest, bucket, n, leaf))
  }
  list(add = add, result = function() list(n = sprintf("%.0f", total),
                                          digest = digest))
}

#' Stream a canonical person-membership hash in bounded keyset pages
#' @param handle CDM handle.
#' @param page Identifiers fetched per query; this never changes the digest.
#' @return List of exact row count and ordered membership digest.
#' @keywords internal
.dsomopDpMembershipStream <- function(handle, page = .DSOMOP_DP_MEMBERSHIP_PAGE) {
  page <- as.integer(page)
  if (length(page) != 1L || is.na(page) || page < 1L) stop("Invalid stream page.")
  person <- .qualifyTable(handle, "person")
  acc <- .dsomopDpMembershipAccumulator()
  last <- NULL
  pending_ids <- pending_k <- character()
  flush <- function(ids, k) {
    acc$add(.dsomopDpMembershipBucket(ids[[1L]]),
            sprintf("%.0f", sum(as.numeric(k))),
            .dsomopDpSha256(paste0(ids, ":", k, "\n", collapse = "")))
  }
  repeat {
    sql <- paste0("SELECT TOP ", page,
      " person_id AS p, COUNT(*) AS k FROM ", person,
      " WHERE person_id IS NOT NULL",
      if (!is.null(last)) paste0(" AND person_id > ", last),
      " GROUP BY person_id ORDER BY person_id")
    rows <- .executeQuery(handle, .sql_translate(sql, handle$target_dialect))
    if (!nrow(rows)) break
    ids <- .dsomopReleasePersonIdText(rows$p)
    if (anyNA(ids) || any(!grepl("^-?[0-9]+$", ids))) stop("Person IDs must be integers.")
    k <- vapply(seq_len(nrow(rows)), function(i) {
      .dsomopDpMembershipInteger(rows$k[[i]])
    }, character(1L))
    last <- ids[[length(ids)]]
    ids <- c(pending_ids, ids)
    k <- c(pending_k, k)
    buckets <- .dsomopDpMembershipBucket(ids)
    ends <- c(which(buckets[-1L] != buckets[-length(buckets)]), length(buckets))
    begin <- 1L
    for (end in head(ends, -1L)) {
      flush(ids[begin:end], k[begin:end])
      begin <- end + 1L
    }
    pending_ids <- ids[begin:length(ids)]
    pending_k <- k[begin:length(k)]
    if (nrow(rows) < page) break
  }
  if (length(pending_ids)) flush(pending_ids, pending_k)
  acc$result()
}

#' Cryptographic person-membership digest
#' @param handle CDM handle.
#' @param method auto, database, or stream.
#' @param page Maximum identifiers per streamed page.
#' @return List: n, digest, method. Never released.
#' @keywords internal
.dsomopDpMembershipHash <- function(handle, method = c("auto", "database", "stream"),
                                    page = .DSOMOP_DP_MEMBERSHIP_PAGE) {
  method <- match.arg(method)
  sql <- if (method != "stream") .dsomopDpMembershipSql(handle) else NULL
  if (identical(method, "database") && is.null(sql)) stop("No database digest adapter.")
  if (is.null(sql)) return(c(.dsomopDpMembershipStream(handle, page), list(method = "stream")))
  acc <- .dsomopDpMembershipAccumulator()
  .withDbReconnect(handle, function(conn) {
    if (.normalizeDBMS(handle$dbms) %in% c("mysql", "mariadb")) {
      old <- DBI::dbGetQuery(conn, "SELECT @@session.group_concat_max_len AS n")$n[[1L]]
      # A leaf contains <=19,999 exact ID:count records, each <=42 bytes.
      # This is a connection-local execution setting, automatically restored.
      DBI::dbExecute(conn, "SET SESSION group_concat_max_len = 1048576")
      on.exit(try(DBI::dbExecute(conn, paste0(
        "SET SESSION group_concat_max_len = ", as.character(old))), silent = TRUE), add = TRUE)
    }
    result <- DBI::dbSendQuery(conn, sql)
    on.exit(DBI::dbClearResult(result), add = TRUE, after = FALSE)
    repeat {
      rows <- DBI::dbFetch(result, n = 1000L)
      if (!nrow(rows)) break
      names(rows) <- tolower(names(rows))
      if ("bytes" %in% names(rows) && any(rows$bytes != rows$expected_bytes)) {
        stop("Database truncated a membership digest leaf.")
      }
      for (i in seq_len(nrow(rows))) acc$add(rows$bucket[[i]], rows$n[[i]], rows$digest[[i]])
    }
  })
  c(acc$result(), list(method = "database"))
}

#' Data generation of a fixed-synopsis request
#'
#' \code{HMAC(K_generation, frame(protocol, n, ordered digest))} of
#' \code{.dsomopDpMembershipHash}, with a subkey of the noise root. Computed
#' once per request inside its read snapshot (\code{.dsomopSnapshotMemo}) and
#' never retained afterwards: no per-handle, per-session or negative cache.
#' A failed read ends only the current request with one stable error (the
#' reason goes to the server log); the next request computes it afresh.
#'
#' @param handle CDM handle of the request.
#' @param policy DP policy (noise root and corpus).
#' @return List: \code{available = TRUE}, \code{id} (64 hex; internal only).
#' @keywords internal
.dsomopDpDataGeneration <- function(handle, policy) {
  .dsomopSnapshotMemo(handle, paste0("generation|", policy$domain, "|",
                                     policy$noise_root$key_id %||% ""),
                      function() {
    tryCatch({
      if (!is.environment(handle)) stop("No connected OMOP resource.")
      root <- policy$noise_root$key
      if (!is.raw(root) || length(root) != 32L) stop("No noise root.")
      hash <- .dsomopDpMembershipHash(handle)
      key <- .dsomopDpSubkey(root, policy$domain,
                             "person-membership-generation/v1")
      list(available = TRUE,
           id = .dsomopDpHmac(key, .dsomopFrame(
             "dsomop-person-membership-generation-v2", hash$n,
             hash$digest)))
    }, error = function(e) {
      message("[dsomop-dp-generation] The data generation of this request ",
              "could not be read (database or digest failure).")
      stop(structure(
        list(message = .DSOMOP_DP_GENERATION_ERROR, call = NULL),
        class = c("dsomop_generation_error", "error", "condition")))
    })
  })
}
