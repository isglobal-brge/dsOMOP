# A-30 pure category declarations. Keep byte-identical in dsOMOPClient.
.categoryReject <- function(message, reason = "category_spec_invalid") {
  stop(structure(list(message = paste0(reason, ": ", message), call = NULL,
    reason = reason, code = "validation_error"), class = c("dsomop_category_error", "dsomop_plan_rejection", "error", "condition")))
}
.categoryStrings <- function(x, unique = FALSE, empty = TRUE) {
  if (is.list(x) && (is.null(names(x)) || !length(x))) x <- unlist(x, use.names = FALSE)
  if (!length(x) && empty) return(character())
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) ||
      anyNA(iconv(x, to = "UTF-8")) || (unique && anyDuplicated(x)))
    .categoryReject("Use nonempty UTF-8 strings and unique declared keys.")
  enc2utf8(x)
}
.categoryKeys <- function(x, empty = TRUE, concept = FALSE) {
  x <- .categoryStrings(x, TRUE, empty)
  valid <- grepl("^concept:(0|[1-9][0-9]*)$", x)
  ids <- sub("^concept:", "", x[valid])
  if (any(nchar(ids) > 10L | (nchar(ids) == 10L & ids > "2147483647")))
    .categoryReject("Concept keys must use the A-27 exact ID range.")
  if (!concept) valid <- valid | grepl("^(bin:[1-9][0-9]*|category:([1-9][0-9]*|[A-Za-z][A-Za-z0-9_]{0,63})|period:[^[:space:]]+|flag:[01]|special:(missing|unknown))$", x)
  if (any(!valid)) .categoryReject("Use canonical typed category keys.")
  unname(x)
}
.categoryObject <- function(x, allowed, required = character()) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyNA(names(x)) ||
      anyDuplicated(names(x)))) || length(setdiff(names(x), allowed)) ||
      length(setdiff(required, names(x)))) .categoryReject("Invalid closed category declaration.")
  if (any(vapply(x, is.null, logical(1)))) .categoryReject("Omit absent fields.")
  invisible(x)
}
.categoryChoice <- function(x, choices) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% choices)
    .categoryReject(paste0("Expected one of: ", paste(choices, collapse = ", "), "."))
  x
}
.categoryBool <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) .categoryReject("Expected one Boolean.")
  x
}
.categoryOrderCheck <- function(x) {
  .categoryObject(x, c("kind", "keys", "metadata"), "kind")
  if (!identical(x$kind, "category_order") || is.null(x$keys) == is.null(x$metadata))
    .categoryReject("Order requires exactly keys or public metadata.")
  if (!is.null(x$keys)) return(.categoryKeys(x$keys))
  if (is.character(x$metadata)) return(.categoryChoice(x$metadata, c("identity", "producer")))
  m <- x$metadata
  .categoryObject(m, c("id", "version", "sha256", "keys"), c("id", "version", "sha256", "keys"))
  if (length(m$id) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$", m$id) ||
      length(.categoryStrings(m$version, empty = FALSE)) != 1L ||
      length(m$sha256) != 1L || !grepl("^[0-9a-f]{64}$", m$sha256))
    .categoryReject("Invalid ordinal metadata identity/version/hash.")
  keys <- .categoryKeys(m$keys)
  digest <- paste0(as.character(openssl::sha256(charToRaw(.jcsCanonical(unname(as.list(keys)))))))
  if (!identical(digest, paste0(m$sha256))) .categoryReject("Ordinal metadata key hash does not match.")
  keys
}
.categoryRecodeCheck <- function(x) {
  .categoryObject(x, c("kind", "map", "levels", "labels", "ordered", "unmatched"),
    c("kind", "map", "levels", "ordered", "unmatched"))
  if (!identical(x$kind, "category_recode")) .categoryReject("Use category_recode.")
  map <- unlist(x$map, use.names = TRUE)
  if (length(map)) {
    if (is.null(names(map)) || anyDuplicated(names(map))) .categoryReject("Recode sources must be unique.")
    .categoryKeys(names(map)); .categoryStrings(unname(map))
    if (any(!grepl("^category:[A-Za-z][A-Za-z0-9_]{0,63}$", map))) .categoryReject("Recode targets must be nominal category keys, never sentinels.")
  }
  lev <- .categoryKeys(x$levels)
  if (!all(map %in% lev)) .categoryReject("Declare every recode target level.")
  if (!is.null(x$labels) && length(.categoryStrings(x$labels, TRUE)) != length(lev))
    .categoryReject("Supply one unique label per recode level.")
  .categoryBool(x$ordered); .categoryChoice(x$unmatched, c("keep", "missing"))
  invisible(x)
}
.categoryDomainCheck <- function(x,depth=0L) {
  if(inherits(x,"omop_concept_set")) {
    x<-list(concepts=x$concepts,include_descendants=x$descendants,include_mapped=x$mapped,exclude=x$exclude,validity_policy=x$validity_policy)
    x<-x[!vapply(x,is.null,logical(1))]
  }
  if(!is.list(x)||is.null(names(x)))return(.categoryKeys(x,FALSE,TRUE))
  if(depth>16L) .categoryReject("Concept domain exclusions exceed the nesting bound.")
  x<-x[!vapply(x,is.null,logical(1))]
  .categoryObject(x,c("concepts","include_descendants","include_mapped","exclude","validity_policy"),"concepts")
  if (!is.null(x$validity_policy)) .ohdsiValidityPolicy(x$validity_policy)
  ids<-unlist(x$concepts,use.names=FALSE)
  if(!is.numeric(ids)||!length(ids)||anyNA(ids)||any(ids<0|ids>2147483647|ids!=trunc(ids))||anyDuplicated(ids)) .categoryReject("Use a nonempty public concept-set domain.")
  for(n in intersect(names(x),c("include_descendants","include_mapped"))) .categoryBool(x[[n]])
  if(!is.null(x$exclude)) {
    if(is.list(x$exclude)&&!is.null(names(x$exclude))||inherits(x$exclude,"omop_concept_set")) .categoryDomainCheck(x$exclude,depth+1L)
    else {ids<-unlist(x$exclude,use.names=FALSE);if(!is.numeric(ids)||anyNA(ids)||any(ids<0|ids>2147483647|ids!=trunc(ids))) .categoryReject("Invalid domain exclusion IDs.")}
  }
  invisible(x)
}
.categoryRollupCheck <- function(x) {
  .categoryObject(x, c("kind", "level", "ancestors", "separation", "distance", "vocabulary", "domain", "multiple", "priority", "unmatched"), "kind")
  kind <- .categoryChoice(x$kind, c("none", "atc", "icd10", "snomed", "ancestors"))
  if (kind == "none") {
    if (length(x) != 1L) .categoryReject("rollup none has no other fields.")
    return(invisible(x))
  }
  .categoryDomainCheck(x$domain)
  .categoryChoice(x$multiple, c("error", "priority", "all"))
  .categoryChoice(x$unmatched, c("keep", "missing"))
  if (kind == "atc") {
    if (!is.numeric(x$level) || length(x$level) != 1L || is.na(x$level) || !x$level %in% 1:5 ||
        any(c("ancestors", "separation", "distance", "vocabulary") %in% names(x))) .categoryReject("ATC requires a class level 1..5 only.")
  } else if (kind == "icd10") {
    .categoryChoice(x$level, c("chapter", "block"))
    if (!identical(x$vocabulary, "ICD10") || any(c("ancestors", "separation", "distance") %in% names(x)))
      .categoryReject("ICD classification requires vocabulary ICD10 and chapter/block.")
  } else {
    if (!is.null(x$level)) .categoryReject("Named ancestors/separation do not accept level.")
    if (kind == "ancestors" && (!is.null(x$separation) || is.null(x$vocabulary))) .categoryReject("Named ancestors require vocabulary.")
    if (kind == "snomed" && !is.null(x$vocabulary) && !identical(x$vocabulary, "SNOMED")) .categoryReject("SNOMED requires SNOMED vocabulary.")
    if (is.null(x$ancestors) == is.null(x$separation)) .categoryReject("Declare exactly ancestors or separation.")
    if (!is.null(x$ancestors)) {
      .categoryKeys(x$ancestors, FALSE, TRUE)
      if (!is.null(x$distance)) .categoryReject("distance requires separation.")
    } else {
      if (!is.numeric(x$separation) || length(x$separation) != 1L || is.na(x$separation) ||
          !is.finite(x$separation) || x$separation < 0 || x$separation != trunc(x$separation)) .categoryReject("Separation must be a nonnegative integer.")
      .categoryChoice(x$distance, c("min", "max"))
    }
  }
  if (x$multiple == "priority") .categoryKeys(x$priority, FALSE, TRUE)
  else if (!is.null(x$priority)) .categoryReject("priority requires multiple=priority.")
  invisible(x)
}
.categorySpecCheck <- function(s) {
  if (is.null(s)) return(invisible(NULL))
  .categoryObject(s, c("levels", "order", "ordered", "reference", "recode", "rollup", "label_style", "language", "missing", "binary"))
  if (!is.null(s$levels)) .categoryKeys(s$levels)
  if (!is.null(s[["order"]])) .categoryOrderCheck(s[["order"]])
  if (!is.null(s$ordered)) .categoryBool(s$ordered)
  if (!is.null(s$reference) && length(.categoryKeys(s$reference, FALSE)) != 1L) .categoryReject("reference requires one key.")
  if (!is.null(s$recode)) .categoryRecodeCheck(s$recode)
  if (!is.null(s$rollup)) .categoryRollupCheck(s$rollup)
  if (!is.null(s$label_style)) .categoryChoice(s$label_style, c("name", "code", "code_name", "vocabulary_code", "source_code"))
  if (!is.null(s$language) && !identical(s$language, "en")) {
    .categoryObject(s$language, c("kind", "language_concept_id"), c("kind", "language_concept_id"))
    id <- s$language$language_concept_id
    if (!identical(s$language$kind, "category_language") || length(id) != 1L ||
        !is.numeric(id) || is.na(id) || id <= 0 || id > 2147483647 || id != trunc(id)) .categoryReject("Language needs one positive language concept ID.")
  }
  if (!is.null(s$missing)) {
    m <- s$missing
    .categoryObject(m, c("kind", "na", "unknown", "labels"), c("kind", "na", "unknown", "labels"))
    if (!identical(m$kind, "category_missing")) .categoryReject("Use category_missing.")
    .categoryChoice(m$na, c("keep", "level")); .categoryChoice(m$unknown, c("keep", "level", "missing"))
    if (!setequal(names(m$labels), c("missing", "unknown")) ||
        length(.categoryStrings(unlist(m$labels), TRUE, FALSE)) != 2L) .categoryReject("Missing labels require distinct missing and unknown labels.")
  }
  if (!is.null(s$binary)) {
    b <- s$binary
    .categoryObject(b, c("kind", "as", "labels"), c("kind", "as", "labels"))
    if (!identical(b$kind, "category_binary")) .categoryReject("Use category_binary.")
    .categoryChoice(b$as, c("integer", "logical", "factor"))
    if (length(.categoryStrings(b$labels, TRUE, FALSE)) != 2L) .categoryReject("Binary labels require false then true.")
  }
  invisible(s)
}
.categoryEffective <- function(variable = NULL, recipe = NULL) {
  .categorySpecCheck(variable); .categorySpecCheck(recipe)
  c(unclass(variable), unclass(recipe)[setdiff(names(recipe), names(variable))])
}
.categoryKeyOrder <- function(keys) {
  prefix <- sub(":.*", "", keys); tail <- sub("^[^:]+:", "", keys)
  group <- match(prefix, c("concept", "bin", "category", "period", "flag", "special"))
  numeric_key <- prefix %in% c("concept", "bin", "flag") | prefix == "category" & grepl("^[0-9]+$", tail)
  width <- ifelse(numeric_key, nchar(tail), 0L)
  subtype <- ifelse(prefix == "category" & !numeric_key, 1L, 0L)
  order(group, subtype, width, tail, method = "radix")
}
.categoryPlanCheck <- function(plan) {
  .categorySpecCheck(plan$options$category_spec)
  walk <- function(x) {
    if (!is.list(x)) return(invisible(NULL))
    if (!is.null(x$category_spec)) .categorySpecCheck(x$category_spec)
    d<-x[["derived"]]
    if(!is.list(d))d<-NULL
    if(identical(d$kind,"repeated")&&!inherits(plan,"omop_recipe")) {
      if(is.null(d$timepoints)==is.null(d$visits)) .categoryReject("Choose exactly timepoints or visits.","repeated_spec_invalid")
      if(!length(d$values)) .categoryReject("Repeated values cannot be empty.","repeated_spec_invalid")
      names<-vapply(d$values,function(e)e$name,"");.categoryStrings(names,TRUE,FALSE)
      if(!d$value %in% names) .categoryReject("Unknown repeated value.","repeated_spec_invalid")
      .categoryChoice(d$missing,c("keep","skip_all","skip_any"))
      if(!is.null(d$timepoints)) {
        .categoryBool(d$timepoints$reuse)
        points<-d$timepoints$points;names<-vapply(points,function(e)e$name,"");.categoryStrings(names,TRUE,FALSE)
        if(!d$point %in% names) .categoryReject("Unknown repeated point.","repeated_spec_invalid")
        for(entry in points) {
          p<-entry$point;.categoryChoice(p$unit,c("days","weeks","months","years"));.categoryChoice(p$select,c("closest","first","last"))
          if(!is.numeric(p$offset)||length(p$offset)!=1L||!is.finite(p$offset)||p$offset!=trunc(p$offset)||abs(p$offset)>.Machine$integer.max) .categoryReject("Invalid repeated offset.","repeated_spec_invalid")
          tolerance<-unlist(p$tolerance,use.names=FALSE)
          if(!is.numeric(tolerance)||length(tolerance)!=2L||any(!is.finite(tolerance))||any(tolerance!=trunc(tolerance))||tolerance[1]>0||tolerance[2]<0) .categoryReject("Invalid inclusive target tolerance.","repeated_spec_invalid")
        }
      } else {
        numbers<-unlist(d$visits$numbers,use.names=FALSE)
        if(!is.numeric(numbers)||!length(numbers)||anyNA(numbers)||any(!is.finite(numbers))||any(numbers<1|numbers!=trunc(numbers))||anyDuplicated(numbers)||!d$point %in% paste0("visit_",numbers)) .categoryReject("Invalid visit ordinals.","repeated_spec_invalid")
        .categoryChoice(d$visits$select,c("first","last","closest"))
      }
    }
    for (v in x) if (is.list(v)) walk(v)
  }
  walk(plan$variables); walk(plan$outputs)
  invisible(plan)
}

.categoryCapabilities <- function(plan) {
  ops <- character()
  walk <- function(x) {
    if(!is.list(x))return(invisible(NULL))
    if(identical(x$kind,"repeated"))ops <<- union(ops,"layout.repeated")
    if(identical(x$kind,"category_reduce"))ops <<- union(ops,"category.reduce")
    if(!is.null(x$category_columns))ops <<- union(ops,"category.presentation")
    if(identical(x$kind,"category_rows"))ops <<- union(ops,"category.presentation")
    if(identical(x$kind,"category_indicators"))ops <<- union(ops,"category.indicators")
    if(!is.null(x$category_spec)) {
      ops <<- union(ops,"category.presentation")
      if(!is.null(x$category_spec$rollup) && x$category_spec$rollup$kind!="none")ops <<- union(ops,"category.rollup")
    }
    for(v in x)if(is.list(v))walk(v)
  }
  walk(plan);if(length(ops))ops<-union(ops,"output.codebook_metadata");ops
}

# JSON null array members must retain their identity positions.
.categoryNullableStrings <- function(x) {
  if(is.list(x))return(vapply(x,function(v)if(is.null(v))NA_character_ else as.character(v),""))
  setNames(as.character(x),names(x))
}

# Release metadata has nullable fields and arrays; plans still reject R NA.
.categoryMetadataPlain <- function(x) {
  if(is.null(x))return(NULL)
  if(is.list(x))return(lapply(x,.categoryMetadataPlain))
  if(is.atomic(x)&&!is.null(names(x)))return(lapply(as.list(x),.categoryMetadataPlain))
  if(is.atomic(x)&&length(x)!=1L)return(lapply(seq_along(x),function(i).categoryMetadataPlain(x[[i]])))
  if(is.atomic(x)&&length(x)==1L&&is.na(x))return(NULL)
  x
}

.categoryDeclaredUnit <- function(plan,node) {
  d<-node$derived
  if(identical(d$kind,"category_indicators"))return("1")
  if(identical(d$kind,"category_reduce"))return(if(d$reducer$fn %in% c("count","n_distinct","sum"))"count" else if(d$reducer$fn %in% c("any","all"))"1" else .categoryDeclaredUnit(plan,d$variable))
  if(identical(d$kind,"repeated")) {
    at<-match(d$value,vapply(d$values,function(x)x$name,""))
    return(.categoryDeclaredUnit(plan,d$values[[at]]$variable))
  }
  .numberDeclaredUnit(plan,node)
}

# Native normal forms can be inspected before .irNormalize adds provenance.
# An explicitly lowered v1 origin still wins over its version-2 IR envelope.
.categoryNative <- function(ir) {
  version<-attr(ir,"source_version") %||% ir[["version"]]
  identical(version,2L)||identical(version,2)
}

# Public producer identity, independent of observed values and factor storage.
.categoryDeclaredConcept <- function(v) {
  d <- v$derived; fn <- v$reducer$fn %||% d$reducer$fn %||% ""
  if (fn %in% c("binary","count","n_distinct","sum","any","all","abnormal","abnormal_low","abnormal_high")) return(FALSE)
  if (identical(d$kind,"category_reduce")) return(.categoryDeclaredConcept(d$variable))
  if (identical(d$kind,"repeated")) {
    at <- match(d$value, vapply(d$values,function(x)x$name,""))
    return(.categoryDeclaredConcept(d$values[[at]]$variable))
  }
  if ((d$kind %||% "") %in% c("race","ethnicity","category_rows") || grepl("(^|_)concept_id$",v$reducer$value %||% "") || grepl("(^|_)concept_id$",v$raw$column %||% "")) return(TRUE)
  if ((d$kind %||% "") %in% c(.DATE_KINDS,.NUMBER_KINDS,"category_indicators","died") || !is.null(v$date_spec) || nzchar(fn)) return(FALSE)
  NA
}
.categoryApplicableVocabulary <- function(spec, explicit, concept) {
  if (isFALSE(concept)) {
    irrelevant <- intersect(names(spec),c("label_style","language","rollup"))
    if (length(intersect(irrelevant,names(explicit)))) .categoryReject("Vocabulary presentation requires concept identities.")
    spec[irrelevant] <- NULL
  }
  spec
}

# Final native projection expands variable declarations in place, then retains
# structural columns before the requested analytic keys and output-local copies.
.categorySelectedColumns <- function(selected, variables, structural, available) {
  selected <- unique(unlist(lapply(unlist(selected, use.names=FALSE), function(key) {
    if (key %in% names(variables)) .pnv2VariableColumns(key, variables[[key]]) else key
  }), use.names=FALSE))
  intersect(unique(c(structural, selected)), available)
}
