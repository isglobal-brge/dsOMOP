test_that("MariaDB and MySQL versions are read from the server's own number", {
  # Ubuntu 22.04's MariaDB 10.6 package: the distribution suffix carries
  # dotted numbers ("0.22.04") that were taken for the server version, so the
  # connection floor refused a MariaDB 10.6 as older than 10.2.
  ubuntu <- "10.6.23-MariaDB-0ubuntu0.22.04.1"
  expect_true(.databaseVersionAtLeast(ubuntu, "10.2.0", "mariadb"))
  expect_false(.databaseVersionAtLeast(ubuntu, "10.7.0", "mariadb"))
  # Replication-protocol prefix, other distribution suffixes, untagged forms.
  expect_true(.databaseVersionAtLeast("5.5.5-10.6.12-MariaDB-log", "10.6.12",
                                      "mariadb"))
  expect_false(.databaseVersionAtLeast("5.5.5-10.1.48-MariaDB", "10.2.0",
                                       "mariadb"))
  expect_true(.databaseVersionAtLeast("11.4.2-MariaDB-ubu2404", "11.4.0",
                                      "mariadb"))
  expect_true(.databaseVersionAtLeast("10.11.6-MariaDB-0+deb12u1", "10.11.6",
                                      "mariadb"))
  expect_false(.databaseVersionAtLeast("10.11.6-MariaDB-0+deb12u1", "10.11.7",
                                       "mariadb"))
  expect_true(.databaseVersionAtLeast("10.6.23", "10.2.0", "mariadb"))
  # MySQL reports its own number first, before any distribution suffix.
  expect_true(.databaseVersionAtLeast("8.0.36-0ubuntu0.22.04.1", "8.0.0",
                                      "mysql"))
  expect_true(.databaseVersionAtLeast("8.4.3", "8.0.0", "mysql"))
  expect_false(.databaseVersionAtLeast("5.7.44-log", "8.0.0", "mysql"))
  expect_false(.databaseVersionAtLeast("", "8.0.0", "mysql"))
  expect_false(.databaseVersionAtLeast(NA_character_, "8.0.0", "mysql"))
})

test_that("the analytic SQL floor accepts a distribution-suffixed MariaDB", {
  handle <- list(dbms = "mariadb", target_dialect = "mysql",
                 dbms_version = "10.6.23-MariaDB-0ubuntu0.22.04.1")
  local_mocked_bindings(.databaseServerVersion = function(handle) {
    handle$dbms_version
  })
  expect_true(.assertAnalyticDbmsSupport(handle))
  handle$dbms_version <- "5.5.5-10.1.48-MariaDB"
  expect_error(.assertAnalyticDbmsSupport(handle), "MariaDB >= 10.2")
})
