# N05: select decimal text before DBI/bit64 can map INT64_MIN to NA.
# This certifies the membership digest only, not the separate person-token path.
for (engine in c("postgresql", "mariadb", "mysql")) local({
  dbms <- engine
  test_that(paste(dbms, "streams minimum signed64 membership without loss"), {
    config <- .dsomopVendorTestConfig(dbms)
    skip_if(is.null(config), "Vendor host not configured")
    fixture <- .dsomopVendorCreateFixture(config)
    on.exit(fixture$cleanup(), add = TRUE)
    h <- fixture$handle
    person <- .qualifyTable(h, "person")
    ids <- c("-9223372036854775808", "-9223372036854775807",
             "-10001", "-10000", "-9999", "0", "9999", "10000", "10001",
             "9223372036854775807")
    DBI::dbExecute(fixture$admin, paste0("DELETE FROM ", person))
    DBI::dbExecute(fixture$admin, paste0("INSERT INTO ", person,
      " (person_id) VALUES ", paste0("(", c(ids, ids[1L]), ")", collapse = ",")))
    .dsomopWithReadSnapshot(h, {
      native <- .dsomopDpMembershipHash(h, "database")
      expect_identical(native$n, as.character(length(ids)))
      for (page in c(1L, 2L, 5L, 100L)) {
        streamed <- .dsomopDpMembershipHash(h, "stream", page = page)
        expect_identical(streamed[c("n", "digest")],
                         native[c("n", "digest")], info = paste(dbms, page))
      }
      # An independent leaf construction pins inclusion of the minimum value,
      # rather than permitting both SQL and stream implementations to omit it.
      acc <- .dsomopDpMembershipAccumulator()
      buckets <- .dsomopDpMembershipBucket(ids)
      for (bucket in unique(buckets)) {
        members <- ids[buckets == bucket]
        acc$add(bucket, as.character(length(members)),
                .dsomopDpSha256(paste0(members, ":1\n", collapse = "")))
      }
      expect_identical(native[c("n", "digest")], acc$result())
    })
    expect_null(h$read_snapshot)
  })
})
