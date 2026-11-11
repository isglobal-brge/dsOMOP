# Semantic digest of a normalised plan IR (design-2.10.3 section 4.4).
#
# WS-0 frozen artefact. The digest is SHA-256 of the RFC 8785 (JSON
# Canonicalization Scheme) serialisation of the normalised IR with
#   (i)   map keys sorted (by UTF-16 code units; plan keys are ASCII),
#   (ii)  arrays kept in order,
#   (iii) defaults expanded exactly once by the normaliser (the caller passes
#         the normalised IR; this file never expands defaults),
#   (iv)  null and absent kept distinct (an explicit null is serialised),
#   (v)   advisory fields excluded individually: meta.generated_by,
#         meta.client_version, meta.recipe_hash, and every `label`,
#         `description` and `result_symbol`; `version`,
#         meta.language_version and meta.semantic_version stay in the
#         payload,
#   (vi)  everything else included.
# The federation harmonisation signature and the DP provenance capsule bind
# this same payload. inst/schema/digest-vectors.json holds the three test
# vectors (base, semantic-version change, result-symbol change). The
# identical file is vendored in dsOMOPClient (tests pin the byte identity).

.DIGEST_ADVISORY_META <- c("generated_by", "client_version", "recipe_hash")
.DIGEST_ADVISORY_KEYS <- c("label", "description", "result_symbol")

#' Canonical JSON (RFC 8785) of an R value parsed with simplifyVector = FALSE
#'
#' Objects are named lists, arrays unnamed lists, scalars length-one vectors,
#' null is NULL. Atomic vectors of length other than one are serialised as
#' arrays of scalars.
#' @keywords internal
.jcsCanonical <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) {
      return(paste0("[", paste(vapply(x, .jcsCanonical, ""), collapse = ","), "]"))
    }
    if (length(x) == 0L) return("{}")
    if (any(!nzchar(nm)) || anyDuplicated(nm)) {
      stop("JSON object keys must be non-empty and unique.", call. = FALSE)
    }
    ord <- .jcsKeyOrder(nm)
    parts <- vapply(ord, function(i) {
      paste0(.jcsString(nm[[i]]), ":", .jcsCanonical(x[[i]]))
    }, "")
    return(paste0("{", paste(parts, collapse = ","), "}"))
  }
  if (length(x) != 1L) {
    return(paste0("[", paste(vapply(seq_along(x), function(i) .jcsCanonical(x[[i]]), ""),
                             collapse = ","), "]"))
  }
  if (is.logical(x)) {
    if (is.na(x)) stop("NA is not a JSON value.", call. = FALSE)
    return(if (x) "true" else "false")
  }
  if (is.numeric(x)) return(.jcsNumber(x))
  if (is.character(x)) {
    if (is.na(x)) stop("NA is not a JSON value.", call. = FALSE)
    return(.jcsString(x))
  }
  if (inherits(x, "Date")) return(.jcsString(format(x, "%Y-%m-%d")))
  stop("Unsupported value of class ", class(x)[1L], " in canonical JSON.",
       call. = FALSE)
}

# Sort keys by UTF-16 code units (RFC 8785 section 3.2.3). Keys are compared
# as sequences of code units; for BMP characters this equals code-point
# order, which C-locale ordering of the UTF-8 bytes also gives.
.jcsKeyOrder <- function(keys) {
  units <- lapply(keys, function(k) {
    cp <- utf8ToInt(enc2utf8(k))
    out <- integer(0)
    for (c in cp) {
      if (c >= 0x10000) {
        c2 <- c - 0x10000
        out <- c(out, 0xD800 + bitwShiftR(c2, 10L), 0xDC00 + bitwAnd(c2, 0x3FF))
      } else {
        out <- c(out, c)
      }
    }
    out
  })
  cmp <- function(a, b) {
    n <- min(length(a), length(b))
    for (i in seq_len(n)) {
      if (a[i] != b[i]) return(sign(a[i] - b[i]))
    }
    sign(length(a) - length(b))
  }
  ord <- seq_along(keys)
  # insertion sort on code-unit vectors (key sets are small)
  for (i in seq_along(ord)[-1L]) {
    j <- i
    while (j > 1L && cmp(units[[ord[j - 1L]]], units[[ord[j]]]) > 0) {
      tmp <- ord[j - 1L]; ord[j - 1L] <- ord[j]; ord[j] <- tmp
      j <- j - 1L
    }
  }
  ord
}

# RFC 8785 section 3.2.2.2 string serialisation.
.jcsString <- function(s) {
  s <- enc2utf8(s)
  cp <- utf8ToInt(s)
  out <- character(length(cp))
  for (i in seq_along(cp)) {
    c <- cp[i]
    out[i] <- if (c == 0x22) "\\\"" else if (c == 0x5C) "\\\\" else
      if (c == 0x08) "\\b" else if (c == 0x0C) "\\f" else
      if (c == 0x0A) "\\n" else if (c == 0x0D) "\\r" else
      if (c == 0x09) "\\t" else if (c < 0x20) sprintf("\\u%04x", c) else
      intToUtf8(c)
  }
  paste0("\"", paste(out, collapse = ""), "\"")
}

# RFC 8785 section 3.2.2.3: ECMAScript Number::toString of the shortest
# round-tripping decimal.
.jcsNumber <- function(x) {
  if (is.na(x) || !is.finite(x)) stop("Non-finite numbers are not JSON values.", call. = FALSE)
  if (x == 0) return("0")
  if (x == round(x) && abs(x) < 1e21) {
    return(format(x, scientific = FALSE, trim = TRUE, nsmall = 0L))
  }
  # shortest %.<d>g representation that round-trips
  s <- NULL
  for (d in 1:17) {
    cand <- sprintf(paste0("%.", d, "g"), x)
    if (as.numeric(cand) == x) { s <- cand; break }
  }
  if (is.null(s)) s <- sprintf("%.17g", x)
  neg <- startsWith(s, "-")
  s <- sub("^-", "", s)
  if (grepl("e", s, fixed = TRUE)) {
    parts <- strsplit(s, "e", fixed = TRUE)[[1L]]
    mant <- parts[1L]; e <- as.integer(parts[2L])
  } else {
    mant <- s; e <- 0L
  }
  int_part <- sub("\\..*$", "", mant)
  frac_part <- if (grepl(".", mant, fixed = TRUE)) sub("^.*\\.", "", mant) else ""
  digits <- paste0(int_part, frac_part)
  lead <- regmatches(digits, regexpr("^0*", digits))
  digits <- substring(digits, nchar(lead) + 1L)
  digits <- sub("0+$", "", digits)
  # n = position of the decimal point relative to the start of `digits`
  n <- nchar(int_part) + e - nchar(lead)
  if (nchar(digits) == 0L) return("0")
  k <- nchar(digits)
  ax <- abs(x)
  body <- if (ax >= 1e-7 && ax < 1e21) {
    if (n <= 0L) {
      paste0("0.", strrep("0", -n), digits)
    } else if (n >= k) {
      paste0(digits, strrep("0", n - k))
    } else {
      paste0(substring(digits, 1L, n), ".", substring(digits, n + 1L, k))
    }
  } else {
    m <- if (k > 1L) paste0(substring(digits, 1L, 1L), ".", substring(digits, 2L, k)) else digits
    paste0(m, "e", if (n - 1L > 0L) "+" else "-", abs(n - 1L))
  }
  paste0(if (neg) "-" else "", body)
}

#' Digest payload: the normalised IR without its advisory fields
#' @keywords internal
.planDigestPayload <- function(ir) {
  if (!is.list(ir) || is.null(names(ir))) {
    stop("A plan IR must be a named list.", call. = FALSE)
  }
  if (!is.null(ir$meta)) {
    keep <- setdiff(names(ir$meta), .DIGEST_ADVISORY_META)
    ir$meta <- ir$meta[keep]
    if (length(ir$meta) == 0L) ir$meta <- structure(list(), names = character(0))
  }
  strip <- function(o) {
    if (is.list(o)) {
      nm <- names(o)
      if (!is.null(nm)) {
        o <- o[!(nm %in% .DIGEST_ADVISORY_KEYS)]
        if (length(o) == 0L) return(structure(list(), names = character(0)))
      }
      return(lapply(o, strip))
    }
    o
  }
  strip(ir)
}

#' Semantic digest of a normalised plan IR
#'
#' @param ir The normalised IR (named list as parsed with simplifyVector =
#'   FALSE, or the identical structure built in R).
#' @return A list with `payload` (canonical JSON) and `sha256` (hex).
#' @keywords internal
.planSemanticDigest <- function(ir) {
  payload <- .jcsCanonical(.planDigestPayload(ir))
  hash <- paste(as.character(openssl::sha256(charToRaw(enc2utf8(payload)))))
  list(payload = payload, sha256 = hash)
}
