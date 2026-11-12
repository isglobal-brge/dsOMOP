# Semantic digest of a normalised plan IR (design-2.10.3 section 4.4;
# amendment list: B1 of the WS-0 review).
#
# WS-0 frozen artefact. The digest is SHA-256 of the RFC 8785 (JSON
# Canonicalization Scheme) serialisation of the normalised IR with
#   (i)   map keys sorted by UTF-16 code units,
#   (ii)  arrays kept in order,
#   (iii) defaults expanded exactly once by the normaliser (the caller passes
#         the normalised IR; this file never expands defaults),
#   (iv)  null and absent kept distinct (an explicit null is serialised),
#   (v)   advisory properties removed AT THEIR OWNING SCHEMA PATHS ONLY:
#         meta.generated_by, meta.client_version, meta.recipe_hash,
#         populations.<id>.label, outputs.<name>.result_symbol (and
#         outputs.<name>.description / populations.<id>.description if a future
#         schema adds them). A user-chosen definition NAME such as an event
#         called `description` or `label` is clinical content and is never
#         stripped; `version`, meta.language_version and meta.semantic_version
#         stay in the payload,
#   (vi)  everything else included.
# Numbers follow ECMAScript Number::toString (RFC 8785 section 3.2.2.3):
# the shortest round-tripping decimal digits, plain notation for exponents in
# (-6, 21], exponent notation otherwise. The round-trip check parses with
# jsonlite (a correctly rounded strtod); base R's as.numeric() is not
# correctly rounded for every 16-17 digit input and is never used here.
# The federation harmonisation signature and the DP provenance capsule bind
# this same payload. inst/schema/digest-vectors.json holds the test vectors.
# The identical file is vendored in dsOMOPClient (tests pin the byte identity).

.DIGEST_ADVISORY_PATHS <- list(
  list(c("meta"), c("generated_by", "client_version", "recipe_hash")),
  list(c("populations", "*"), c("label", "description")),
  list(c("outputs", "*"), c("result_symbol", "description"))
)

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

# Sort keys by UTF-16 code units (RFC 8785 section 3.2.3).
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

# Shortest decimal digits that parse back to exactly `x` (correctly rounded
# parse through jsonlite). Returns list(digits = "<no leading/trailing zeros>",
# n = <decimal exponent: value = 0.digits * 10^n>) for a finite x > 0.
.jcsShortestDigits <- function(x) {
  s <- NULL
  for (d in 1:17) {
    cand <- sprintf(paste0("%.", d - 1L, "e"), x)
    parsed <- jsonlite::fromJSON(cand)
    if (identical(as.numeric(parsed), x)) { s <- cand; break }
  }
  if (is.null(s)) s <- sprintf("%.16e", x)
  parts <- strsplit(s, "e", fixed = TRUE)[[1L]]
  mant <- parts[1L]; e <- as.integer(parts[2L])
  digits <- gsub(".", "", mant, fixed = TRUE)
  lead <- nchar(regmatches(digits, regexpr("^0*", digits)))
  digits <- substring(digits, lead + 1L)
  digits <- sub("0+$", "", digits)
  # mantissa d.ddd means value = d.ddd * 10^e = 0.dddd * 10^(e + 1)
  list(digits = digits, n = e + 1L - lead)
}

# RFC 8785 section 3.2.2.3 / ECMAScript Number::toString.
.jcsNumber <- function(x) {
  if (is.na(x) || !is.finite(x)) stop("Non-finite numbers are not JSON values.", call. = FALSE)
  x <- as.numeric(x)
  if (x == 0) return("0")
  neg <- x < 0
  sd <- .jcsShortestDigits(abs(x))
  digits <- sd$digits; n <- sd$n; k <- nchar(digits)
  body <- if (k <= n && n <= 21L) {
    paste0(digits, strrep("0", n - k))
  } else if (0L < n && n <= 21L) {
    paste0(substring(digits, 1L, n), ".", substring(digits, n + 1L, k))
  } else if (-6L < n && n <= 0L) {
    paste0("0.", strrep("0", -n), digits)
  } else {
    e10 <- n - 1L
    m <- if (k > 1L) paste0(substring(digits, 1L, 1L), ".", substring(digits, 2L, k)) else digits
    paste0(m, "e", if (e10 > 0L) "+" else "-", abs(e10))
  }
  paste0(if (neg) "-" else "", body)
}

#' Digest payload: the normalised IR without its advisory properties
#'
#' Advisory properties are removed only at their owning schema paths
#' (.DIGEST_ADVISORY_PATHS); definition names are never touched.
#' @keywords internal
.planDigestPayload <- function(ir) {
  if (!is.list(ir) || is.null(names(ir))) {
    stop("A plan IR must be a named list.", call. = FALSE)
  }
  drop_keys <- function(obj, keys) {
    if (!is.list(obj) || is.null(names(obj))) return(obj)
    keep <- setdiff(names(obj), keys)
    out <- obj[keep]
    if (length(out) == 0L) structure(list(), names = character(0)) else out
  }
  for (rule in .DIGEST_ADVISORY_PATHS) {
    path <- rule[[1L]]; keys <- rule[[2L]]
    if (length(path) == 1L) {
      if (!is.null(ir[[path]])) ir[[path]] <- drop_keys(ir[[path]], keys)
    } else {
      container <- ir[[path[[1L]]]]
      if (is.list(container) && !is.null(names(container))) {
        for (nm in names(container)) container[[nm]] <- drop_keys(container[[nm]], keys)
        ir[[path[[1L]]]] <- container
      }
    }
  }
  ir
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
