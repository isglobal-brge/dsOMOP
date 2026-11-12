# Private population snapshots (design section 6.9 and amendment A-20).
# A deterministic ORDER BY does not make a CTE immutable when the underlying
# CDM changes between statements. Freeze the selected relation inside the
# database once, retaining its fragment contract and consumer metadata. This
# path neither counts nor fetches patient rows into R and has no memory-row cap.

# Takes an UNREGISTERED fragment whose dependencies are registered in ctx.
# Returns the same fragment backed by a context-owned session table; the caller
# registers it. The contract and attributes survive, including candidate
# population identity and index-source metadata.
.populationFreezeFragment <- function(ctx, f) {
  .assertFragment(f)
  if (!is.null(.fragmentGet(ctx, f$name))) {
    stop("A population fragment must be frozen before registration.", call. = FALSE)
  }
  h <- .compileDialect(ctx)
  handle <- ctx[["handle"]]
  .sqlRequire(h, "session_table")
  columns <- names(f$columns)
  sql <- .buildStatement(ctx, f, select = paste0(
    "SELECT ", paste(paste0("p.", columns), collapse = ", "), " FROM ", f$name, " p"), dialect = h)
  types <- vapply(f$columns, function(column) column$type, "")
  nulls <- vapply(columns, function(column) {
    if (identical(types[[column]], "text") && .sqlFamily(h) %in% c("postgresql", "duckdb")) {
      return("CAST(NULL AS TEXT)")
    }
    .compileTypedNull(h, types[[column]])
  }, "")
  empty <- paste0("SELECT ", paste(paste0(nulls, " AS ", columns), collapse = ", "),
                  .compileNoTableFrom(h), " WHERE 1 = 0")
  base <- substr(gsub("[^a-z0-9_]", "_", tolower(paste0("dsomop_pop_", f$name))), 1L, 48L)
  table <- .reserveTempTableName(handle, base)
  # Own the name before CREATE. A failed DROP stays owned so context closure
  # can retry it, matching the other private compiler materialisations.
  assign("compile_temp_tables", unique(c(ctx[["compile_temp_tables"]] %||% character(0), table)),
         envir = ctx)
  tryCatch({
    .createTempTable(handle, table, empty)
    if (identical(.sqlFamily(h), "mysql")) {
      # CTAS over typed NULL otherwise gives CHAR(255) and DECIMAL(38,10).
      # Origin keys can be longer, and sampling weights need double precision.
      modify <- vapply(columns[types %in% c("text", "numeric")], function(column) {
        paste0("MODIFY COLUMN ", column, " ",
               if (identical(types[[column]], "text")) "LONGTEXT CHARACTER SET utf8mb4" else "DOUBLE")
      }, "")
      if (length(modify)) .executeStatement(handle, paste0("ALTER TABLE ", table, " ",
                                                          paste(modify, collapse = ", ")))
    }
    .executeStatement(handle, paste0("INSERT INTO ", table, " (", paste(columns, collapse = ", "),
                                     ") ", sql))
    f$select_body <- paste0("SELECT ", paste(paste0("t.", columns, " AS ", columns), collapse = ", "),
                            " FROM ", table, " t")
    f$dependencies <- character(0)
    f <- .mysqlStockpileSource(f, table, h)
    .assertFragment(f)
    f
  }, error = function(e) {
    cleanup <- tryCatch({
      .compileReleaseTempTables(ctx, table)
      NULL
    }, error = identity)
    if (!is.null(cleanup)) {
      e$message <- paste(conditionMessage(e), conditionMessage(cleanup), sep = "\n")
      e$cleanup_error <- cleanup
    }
    stop(e)
  })
}
