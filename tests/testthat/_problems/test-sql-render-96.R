# Extracted from test-sql-render.R:96

# test -------------------------------------------------------------------------
sql <- .sql_translate("DATEADD(day, 7, start_date)", "sqlite")
expect_true(grepl("STRFTIME", sql))
expect_true(grepl("86400", sql))
