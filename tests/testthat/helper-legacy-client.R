# The execution call of dsOMOPClient 2.10.2 (tag 2.10.2), for the WS-8 legacy
# route (Astra's WS-8 review, B1): a federation with a site that predates the
# publication protocol sends this call, unchanged, to every site, including
# 2.10.4 servers. The encoding is 2.10.2's R/session.R .ds_coerce_names() and
# .ds_encode(), verbatim; the call is its exec_args (R/plan.R): positional, the
# resource symbol, the encoded plan and out mapping and the output mode, and
# no commit_policy.

.legacy_coerce_names <- function(x) {
  if (is.data.frame(x)) return(x)
  if (is.list(x)) return(lapply(x, .legacy_coerce_names))
  if (is.atomic(x) && !is.null(names(x))) return(as.list(x))
  x
}

.legacy_ds_encode <- function(x) {
  if (is.list(x) || (is.vector(x) && length(x) > 1)) {
    x <- .legacy_coerce_names(x)
    json <- as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))
    b64 <- gsub("[\r\n]", "", jsonlite::base64_enc(charToRaw(json)))
    b64 <- gsub("\\+", "-", b64)
    b64 <- gsub("/", "_", b64)
    b64 <- gsub("=+$", "", b64)
    paste0("B64:", b64)
  } else {
    x
  }
}

.legacy_execution_call <- function(method, res_symbol, plan, out, output_mode = "memory") {
  as.call(list(as.name(method), res_symbol, .legacy_ds_encode(plan),
               .legacy_ds_encode(out), output_mode))
}
