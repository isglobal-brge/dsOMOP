# Unmodified helpers from server b9cb9f8, for byte-for-byte comparisons.
.dsomopReleaseCanonicalPersonTokens <- function(keys, policy, text) {
  if (anyNA(text) || any(!nzchar(text))) {
    stop("Protected releases need a person identifier for every row.",
         call. = FALSE)
  }
  # Byte-length framed (B04): no identifier or domain text can shift a field
  # boundary.
  domain <- enc2utf8(as.character(policy$domain %||% ""))
  text <- enc2utf8(text)
  framed <- paste0("dsomop-release-person-v2|", nchar(domain, type = "bytes"),
                   ":", domain, "|", nchar(text, type = "bytes"), ":", text)
  as.character(openssl::sha256(framed, key = keys$person))
}

.dsomopReleaseFingerprintFramed <- function(keys, owner, family, coordinate, records) {
  digests <- character(0)
  if (length(records)) {
    starts <- seq.int(1L, length(records), by = 10000L)
    digests <- vapply(starts, function(start) {
      chunk <- records[seq.int(start, min(start + 9999L, length(records)))]
      .dsomopDpHmac(keys$fingerprint, paste(
        c("dsomop-contribution-chunk-v2", length(chunk), chunk),
        collapse = "\n"))
    }, character(1L), USE.NAMES = FALSE)
  }
  .dsomopDpHmac(keys$fingerprint, .dsomopDpCanonicalJson(list(
    protocol = "dsomop-contributions-v1", owner = owner, family = family,
    coordinate = coordinate, n = length(records),
    chunks = paste(digests, collapse = ",")
  )))
}
