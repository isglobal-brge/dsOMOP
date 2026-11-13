# CSV keeps the conventional unquoted NA token for missing cells. Escape the
# literal string NA and every leading backslash, so no valid text collides
# with missingness or with the escape itself. The manifest binds the codec;
# descriptors without it retain the legacy reader behaviour.
.stagedWriteCsv <- function(frame, path, col.names = TRUE, append = FALSE) {
  for (col in names(frame)) {
    x <- frame[[col]]
    if (!is.character(x) && !is.factor(x)) next
    x <- as.character(x)
    escaped <- !is.na(x) & (x == "NA" | startsWith(x, "\\"))
    x[escaped] <- paste0("\\", x[escaped])
    frame[[col]] <- x
  }
  utils::write.table(frame, path, sep = ",", row.names = FALSE,
                    col.names = col.names, append = append, na = "NA",
                    qmethod = "double")
}

.stagedDecodeCsv <- function(frame, encoding) {
  if (is.null(encoding)) return(frame)
  if (!identical(encoding, "backslash-v1")) {
    stop("Unsupported staged CSV encoding.", call. = FALSE)
  }
  for (col in names(frame)) {
    x <- frame[[col]]
    escaped <- !is.na(x) & startsWith(x, "\\")
    x[escaped] <- substring(x[escaped], 2L)
    frame[[col]] <- x
  }
  frame
}
