# JSON Schema validation for the 2.10.3 plan contracts: a packaged,
# explicitly limited subset of JSON Schema 2020-12 (design-2.10.3 section 4;
# B2 of the WS-0 review).
#
# WS-0 frozen artefact. The wire schema inst/schema/plan-v2.json, the
# compatibility IR schema ir-v2.json and the mutation schema mutate-v1.json are
# validated with this self-contained validator, so that validation never
# depends on a JavaScript engine. It is NOT a general draft-2020-12 validator:
# it implements exactly the keywords those schemas use -- $ref (local JSON
# pointers), type, const, enum, properties, required, additionalProperties,
# propertyNames, minProperties, maxProperties, items, minItems, maxItems,
# uniqueItems, oneOf, anyOf, allOf, not, if/then/else, pattern, minLength,
# maxLength, minimum, maximum, exclusiveMinimum, exclusiveMaximum, and boolean
# schemas. Annotation keywords ($schema, $id, $comment, title, description,
# default, examples, $defs) are ignored. Any other keyword is an error, both
# when a packaged schema is loaded (.jsonSchemaLint walks every branch) and
# when a branch is visited. The independent Python jsonschema check of
# tools/ws0/check_contracts.py is the mandatory second oracle for the
# packaged schemas, examples and probes (run in CI).
#
# Instances are R values as produced by jsonlite::fromJSON(simplifyVector =
# FALSE): JSON objects are named lists, arrays unnamed lists, strings,
# numbers and booleans length-one vectors, null is NULL. A non-finite number
# (jsonlite parses "1e400" as Inf) and an object with duplicate member names
# are rejected as input errors before any keyword runs. The identical file is
# vendored in dsOMOPClient (tests pin the byte identity).

.JSON_SCHEMA_ANNOTATIONS <- c("$schema", "$id", "$comment", "title",
                              "description", "default", "examples", "$defs",
                              "deprecated", "readOnly", "writeOnly")
.JSON_SCHEMA_KEYWORDS <- c("$ref", "type", "const", "enum", "properties",
                           "required", "additionalProperties", "propertyNames",
                           "minProperties", "maxProperties", "items",
                           "minItems", "maxItems", "uniqueItems", "oneOf",
                           "anyOf", "allOf", "not", "if", "then", "else",
                           "pattern", "minLength", "maxLength", "minimum",
                           "maximum", "exclusiveMinimum", "exclusiveMaximum")
.JSON_SCHEMA_SUBSCHEMA_KEYS <- c("properties", "propertyNames", "additionalProperties",
                                 "items", "oneOf", "anyOf", "allOf", "not",
                                 "if", "then", "else", "$defs")

#' Load a JSON Schema or a JSON instance without simplification
#' @param path File path.
#' @return The parsed value (objects as named lists, arrays as lists).
#' @keywords internal
.jsonSchemaLoad <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Lint a schema: every branch may use only the supported keywords
#'
#' @param schema Parsed schema.
#' @param path Pointer used in messages.
#' @return Invisibly TRUE; stops on the first unsupported keyword.
#' @keywords internal
.jsonSchemaLint <- function(schema, path = "#") {
  if (is.logical(schema) && length(schema) == 1L) return(invisible(TRUE))
  if (!is.list(schema) || is.null(names(schema))) {
    if (is.list(schema) && length(schema) == 0L) return(invisible(TRUE))
    stop("Schema node at ", path, " must be an object or a boolean.", call. = FALSE)
  }
  keys <- names(schema)
  unknown <- setdiff(keys, c(.JSON_SCHEMA_KEYWORDS, .JSON_SCHEMA_ANNOTATIONS))
  if (length(unknown) > 0L) {
    stop("Unsupported JSON Schema keyword(s) at ", path, ": ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  for (key in intersect(keys, .JSON_SCHEMA_SUBSCHEMA_KEYS)) {
    node <- schema[[key]]
    if (key %in% c("properties", "$defs")) {
      for (nm in names(node)) .jsonSchemaLint(node[[nm]], paste0(path, "/", key, "/", nm))
    } else if (key %in% c("oneOf", "anyOf", "allOf")) {
      for (i in seq_along(node)) .jsonSchemaLint(node[[i]], paste0(path, "/", key, "/", i - 1L))
    } else {
      .jsonSchemaLint(node, paste0(path, "/", key))
    }
  }
  invisible(TRUE)
}

#' Read one of the packaged 2.10.3 schemas (linted on load)
#' @param name "plan-v2", "ir-v2" or "mutate-v1".
#' @keywords internal
.jsonSchemaPackaged <- function(name, package = "dsOMOP") {
  name <- match.arg(name, c("plan-v2", "ir-v2", "mutate-v1"))
  path <- system.file("schema", paste0(name, ".json"), package = package)
  if (!nzchar(path)) {
    stop("Packaged schema '", name, "' not found.", call. = FALSE)
  }
  schema <- .jsonSchemaLoad(path)
  .jsonSchemaLint(schema)
  schema
}

#' JSON type of an instance value
#'
#' Non-finite numbers and objects with duplicate member names are input
#' errors reported by the validator, not types.
#' @keywords internal
.jsonInstanceType <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) {
    nm <- names(x)
    if (is.null(nm)) return("array")
    if (length(x) == 0L) return("object")
    if (all(nzchar(nm))) return(if (anyDuplicated(nm)) "duplicate_keys" else "object")
    stop("JSON instance list mixes named and unnamed elements.", call. = FALSE)
  }
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return("boolean")
  if (is.numeric(x) && length(x) == 1L) {
    if (is.na(x) || !is.finite(x)) return("nonfinite")
    return(if (x == round(x)) "integer" else "number")
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) return("string")
  if (is.atomic(x) && length(x) != 1L) return("array")
  stop("Unsupported JSON instance value of class ", class(x)[1L], ".",
       call. = FALSE)
}

#' Elements of an array instance as a list
#' @keywords internal
.jsonArrayItems <- function(x) {
  if (is.list(x)) return(unname(x))
  lapply(seq_along(x), function(i) x[[i]])
}

#' Resolve a local JSON pointer reference ("#", "#/$defs/name", ...)
#' @keywords internal
.jsonSchemaResolveRef <- function(ref, root) {
  if (!is.character(ref) || length(ref) != 1L || !startsWith(ref, "#")) {
    stop("Only local $ref pointers are supported: '", ref, "'.", call. = FALSE)
  }
  pointer <- substring(ref, 2L)
  node <- root
  if (!nzchar(pointer)) return(node)
  if (!startsWith(pointer, "/")) {
    stop("Malformed JSON pointer in $ref: '", ref, "'.", call. = FALSE)
  }
  parts <- strsplit(substring(pointer, 2L), "/", fixed = TRUE)[[1L]]
  for (p in parts) {
    # RFC 6901: unescape ~1 before ~0
    p <- gsub("~0", "~", gsub("~1", "/", p, fixed = TRUE), fixed = TRUE)
    if (is.list(node) && !is.null(names(node)) && p %in% names(node)) {
      node <- node[[p]]
    } else if (is.list(node) && is.null(names(node)) &&
               grepl("^[0-9]+$", p) && as.integer(p) < length(node)) {
      node <- node[[as.integer(p) + 1L]]
    } else {
      stop("Unresolved $ref '", ref, "'.", call. = FALSE)
    }
  }
  node
}

#' Validate an instance against a schema (2020-12 subset)
#'
#' @param schema Parsed schema (list) or a boolean schema.
#' @param instance Parsed instance.
#' @param root Root schema used to resolve local references.
#' @param path JSON pointer of the instance being validated (for messages).
#' @return list(valid = logical(1), errors = character()) where each error is
#'   "<instance pointer>: <message>".
#' @keywords internal
.jsonSchemaValidate <- function(schema, instance, root = schema, path = "") {
  errors <- character(0)
  input <- .jsonInputErrors(instance, path)
  if (length(input) > 0L) return(list(valid = FALSE, errors = input))
  ok <- .jsv_node(schema, instance, root, path, function(msg) {
    errors <<- c(errors, msg)
  })
  list(valid = isTRUE(ok) && length(errors) == 0L, errors = errors)
}

# Input-level errors anywhere in the instance: non-finite numbers and
# duplicate member names (checked before any schema keyword runs).
.jsonInputErrors <- function(x, path = "") {
  here <- if (nzchar(path)) path else "/"
  t <- tryCatch(.jsonInstanceType(x), error = function(e) "unsupported")
  if (t == "nonfinite") return(paste0(here, ": number is not finite"))
  if (t == "duplicate_keys") {
    return(paste0(here, ": duplicate object member(s) ",
                  paste(unique(names(x)[duplicated(names(x))]), collapse = ", ")))
  }
  if (t == "unsupported") return(paste0(here, ": unsupported value"))
  if (t == "object") {
    return(unlist(lapply(names(x), function(nm) .jsonInputErrors(x[[nm]], paste0(path, "/", nm)))))
  }
  if (t == "array") {
    items <- .jsonArrayItems(x)
    return(unlist(lapply(seq_along(items), function(i) .jsonInputErrors(items[[i]], paste0(path, "/", i - 1L)))))
  }
  character(0)
}

.jsv_quiet <- function(schema, instance, root, path) {
  isTRUE(.jsv_node(schema, instance, root, path, function(msg) NULL))
}

.jsv_node <- function(schema, x, root, path, report) {
  if (is.logical(schema) && length(schema) == 1L) {
    if (!isTRUE(schema)) report("schema is false")
    return(isTRUE(schema))
  }
  if (!is.list(schema)) {
    stop("A schema must be an object or a boolean.", call. = FALSE)
  }
  keys <- names(schema) %||% character(0)
  unknown <- setdiff(keys, c(.JSON_SCHEMA_KEYWORDS, .JSON_SCHEMA_ANNOTATIONS))
  if (length(unknown) > 0L) {
    stop("Unsupported JSON Schema keyword(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  ok <- TRUE
  fail <- function(msg) {
    ok <<- FALSE
    report(paste0(if (nzchar(path)) path else "/", ": ", msg))
  }
  itype <- .jsonInstanceType(x)
  if (itype %in% c("nonfinite", "duplicate_keys")) {
    fail(if (itype == "nonfinite") "number is not finite" else "duplicate object member names")
    return(FALSE)
  }

  if ("$ref" %in% keys) {
    target <- .jsonSchemaResolveRef(schema[["$ref"]], root)
    if (!.jsv_node(target, x, root, path, report)) ok <- FALSE
  }
  if ("type" %in% keys) {
    want <- unlist(schema[["type"]])
    matched <- itype %in% want || (itype == "integer" && "number" %in% want)
    if (!matched) fail(paste0("expected type ", paste(want, collapse = "|"),
                              ", got ", itype))
  }
  if ("const" %in% keys) {
    if (!identical(.jcsCanonical(x), .jcsCanonical(schema[["const"]]))) {
      fail(paste0("expected const ", .jcsCanonical(schema[["const"]])))
    }
  }
  if ("enum" %in% keys) {
    canon <- .jcsCanonical(x)
    allowed <- vapply(.jsonArrayItems(schema[["enum"]]), .jcsCanonical, "")
    if (!canon %in% allowed) {
      fail(paste0("value ", canon, " is not one of ",
                  paste(allowed, collapse = ", ")))
    }
  }
  if ("pattern" %in% keys && itype == "string") {
    if (!grepl(schema[["pattern"]], x, perl = TRUE)) {
      fail(paste0("string does not match pattern ", schema[["pattern"]]))
    }
  }
  if ("minLength" %in% keys && itype == "string" && nchar(x) < schema[["minLength"]]) {
    fail(paste0("string shorter than ", schema[["minLength"]]))
  }
  if ("maxLength" %in% keys && itype == "string" && nchar(x) > schema[["maxLength"]]) {
    fail(paste0("string longer than ", schema[["maxLength"]]))
  }
  if (itype %in% c("number", "integer")) {
    if ("minimum" %in% keys && x < schema[["minimum"]]) fail(paste0("below minimum ", schema[["minimum"]]))
    if ("maximum" %in% keys && x > schema[["maximum"]]) fail(paste0("above maximum ", schema[["maximum"]]))
    if ("exclusiveMinimum" %in% keys && x <= schema[["exclusiveMinimum"]]) {
      fail(paste0("not above exclusiveMinimum ", schema[["exclusiveMinimum"]]))
    }
    if ("exclusiveMaximum" %in% keys && x >= schema[["exclusiveMaximum"]]) {
      fail(paste0("not below exclusiveMaximum ", schema[["exclusiveMaximum"]]))
    }
  }
  if (itype == "object") {
    props <- schema[["properties"]]
    prop_names <- names(props) %||% character(0)
    inst_names <- names(x)
    if ("required" %in% keys) {
      missing <- setdiff(unlist(schema[["required"]]), inst_names)
      if (length(missing) > 0L) {
        fail(paste0("missing required propert", if (length(missing) > 1L) "ies " else "y ",
                    paste(missing, collapse = ", ")))
      }
    }
    if ("minProperties" %in% keys && length(x) < schema[["minProperties"]]) {
      fail(paste0("fewer than ", schema[["minProperties"]], " properties"))
    }
    if ("maxProperties" %in% keys && length(x) > schema[["maxProperties"]]) {
      fail(paste0("more than ", schema[["maxProperties"]], " properties"))
    }
    if ("properties" %in% keys) {
      for (nm in intersect(prop_names, inst_names)) {
        if (!.jsv_node(props[[nm]], x[[nm]], root, paste0(path, "/", nm), report)) ok <- FALSE
      }
    }
    if ("propertyNames" %in% keys) {
      for (nm in inst_names) {
        if (!.jsv_node(schema[["propertyNames"]], nm, root, paste0(path, "/", nm), report)) ok <- FALSE
      }
    }
    if ("additionalProperties" %in% keys) {
      extra <- setdiff(inst_names, prop_names)
      ap <- schema[["additionalProperties"]]
      if (is.logical(ap) && length(ap) == 1L) {
        if (!isTRUE(ap) && length(extra) > 0L) {
          fail(paste0("unknown propert", if (length(extra) > 1L) "ies " else "y ",
                      paste(extra, collapse = ", ")))
        }
      } else {
        for (nm in extra) {
          if (!.jsv_node(ap, x[[nm]], root, paste0(path, "/", nm), report)) ok <- FALSE
        }
      }
    }
  }
  if (itype == "array") {
    items <- .jsonArrayItems(x)
    if ("minItems" %in% keys && length(items) < schema[["minItems"]]) {
      fail(paste0("fewer than ", schema[["minItems"]], " items"))
    }
    if ("maxItems" %in% keys && length(items) > schema[["maxItems"]]) {
      fail(paste0("more than ", schema[["maxItems"]], " items"))
    }
    if ("uniqueItems" %in% keys && isTRUE(schema[["uniqueItems"]])) {
      canon <- vapply(items, .jcsCanonical, "")
      if (anyDuplicated(canon)) fail("items are not unique")
    }
    if ("items" %in% keys) {
      for (i in seq_along(items)) {
        if (!.jsv_node(schema[["items"]], items[[i]], root, paste0(path, "/", i - 1L), report)) ok <- FALSE
      }
    }
  }
  if ("allOf" %in% keys) {
    for (s in .jsonArrayItems(schema[["allOf"]])) {
      if (!.jsv_node(s, x, root, path, report)) ok <- FALSE
    }
  }
  if ("anyOf" %in% keys) {
    hits <- vapply(.jsonArrayItems(schema[["anyOf"]]), function(s) .jsv_quiet(s, x, root, path), logical(1))
    if (!any(hits)) fail("matches none of the anyOf alternatives")
  }
  if ("oneOf" %in% keys) {
    hits <- vapply(.jsonArrayItems(schema[["oneOf"]]), function(s) .jsv_quiet(s, x, root, path), logical(1))
    if (sum(hits) != 1L) {
      fail(paste0("matches ", sum(hits), " of the oneOf alternatives (exactly one required)"))
    }
  }
  if ("not" %in% keys) {
    if (.jsv_quiet(schema[["not"]], x, root, path)) fail("matches the forbidden `not` schema")
  }
  if ("if" %in% keys) {
    branch <- if (.jsv_quiet(schema[["if"]], x, root, path)) "then" else "else"
    if (branch %in% keys) {
      if (!.jsv_node(schema[[branch]], x, root, path, report)) ok <- FALSE
    }
  }
  ok
}

#' Validate a plan against a packaged schema and stop on failure
#' @keywords internal
.jsonSchemaAssert <- function(instance, name = "plan-v2", package = "dsOMOP",
                              what = "plan") {
  res <- .jsonSchemaValidate(.jsonSchemaPackaged(name, package), instance)
  if (!res$valid) {
    stop("Invalid ", what, " (", name, "): ",
         paste(utils::head(res$errors, 8L), collapse = "; "),
         if (length(res$errors) > 8L) "; ..." else "", call. = FALSE)
  }
  invisible(TRUE)
}
