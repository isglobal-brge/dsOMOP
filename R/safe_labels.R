# Labels that every dsBase function carries on Opal (dsOMOP 2.10.9). Opal's
# DataSHIELD expression parser rejects spaces, accented letters and most
# punctuation inside strings, and dsBase's ds.table() splits its level list at
# commas. Published factor labels therefore keep ASCII letters, digits, ".",
# "_", "-" and "+" (measured on a real Opal): common Latin letters are
# transliterated with a fixed table (not iconv, whose result depends on each
# server's locale), any other run of characters becomes "_", a label left
# empty (another script) takes its fallback (an identity), and a collision gets
# the level's position as a suffix. The transformation is a pure function of
# the shared levels, so every site gets the same labels. The client keeps an
# identical copy of .safeLabels().
.SAFE_LABEL_FROM <- paste0(
  "ÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑ",
  "ÒÓÔÕÖÙÚÛÜÝàáâãäå",
  "çèéêëìíîïñòóôõöù",
  "úûüýÿ")
.SAFE_LABEL_TO <- "AAAAAACEEEEIIIINOOOOOUUUUYaaaaaaceeeeiiiinooooouuuuyy"
.SAFE_LABEL_PAIRS <- c("ß" = "ss", "Æ" = "AE", "æ" = "ae", "Œ" = "OE", "œ" = "oe",
                       "Ø" = "O", "ø" = "o", "Đ" = "D", "đ" = "d", "Ł" = "L",
                       "ł" = "l", "Þ" = "TH", "þ" = "th", "Ð" = "D", "ð" = "d")

.safeLabels <- function(x, fallback = NULL) {
  x <- enc2utf8(as.character(x))
  clean <- function(v) {
    v <- chartr(.SAFE_LABEL_FROM, .SAFE_LABEL_TO, v)
    for (from in names(.SAFE_LABEL_PAIRS)) v <- gsub(from, .SAFE_LABEL_PAIRS[[from]], v, fixed = TRUE)
    v <- gsub("[^A-Za-z0-9._+-]+", "_", v, perl = TRUE)
    gsub("^_+|_+$", "", v, perl = TRUE)
  }
  out <- clean(x)
  out[is.na(x)] <- NA_character_
  empty <- !is.na(out) & !nzchar(out)
  if (any(empty) && length(fallback) == length(x)) out[empty] <- clean(as.character(fallback[empty]))
  out[!is.na(out) & !nzchar(out)] <- "level"
  # A collision keeps the first label and numbers the others by position.
  repeat {
    clash <- !is.na(out) & duplicated(out)
    if (!any(clash)) break
    out[clash] <- paste0(out[clash], "_", which(clash))
  }
  out
}

# The shared resolution with safe labels and levels (after its validation, so
# declared user labels are checked as declared).
.conceptSafeShared <- function(shared) {
  levels <- as.character(unlist(shared$levels, use.names = FALSE))
  # A label in another script falls back to the first identity it holds.
  ids <- as.character(unlist(shared$ids %||% shared$keys, use.names = FALSE))
  labels <- unname(.categoryNullableStrings(shared$labels))
  first <- if (length(ids) == length(labels)) ids[match(levels, labels)] else NULL
  safe <- .safeLabels(levels, first)
  shared$safe_labels <- TRUE
  if (identical(safe, levels)) return(shared)
  map <- stats::setNames(safe, levels)
  relabel <- function(x) {
    values <- unname(.categoryNullableStrings(x))
    out <- unname(map[values])
    out[is.na(values)] <- NA_character_
    out
  }
  shared$labels <- relabel(shared$labels)
  shared$levels <- safe
  shared
}
