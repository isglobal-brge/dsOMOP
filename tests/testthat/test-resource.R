# --- Readable Resource URL Parsing Tests (.parseOmopUrl) ---

test_that("resource.js exposes and serializes every supported persistent namespace", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is required to execute resource.js")

  resource_js <- system.file("resources", "resource.js", package = "dsOMOP")
  if (!nzchar(resource_js)) {
    resource_js <- testthat::test_path("..", "..", "inst", "resources",
                                      "resource.js")
  }
  expect_true(file.exists(resource_js))

  harness <- tempfile(fileext = ".js")
  on.exit(unlink(harness), add = TRUE)
  writeLines(c(
    '"use strict";',
    'const assert = require("assert/strict");',
    'const fs = require("fs");',
    'const vm = require("vm");',
    'const sandbox = {};',
    'vm.runInNewContext(fs.readFileSync(process.argv[2], "utf8"), sandbox);',
    'const provider = sandbox.dsOMOP;',
    'assert(provider && provider.settings && provider.asResource);',
    'const expected = ["cdm_schema", "vocabulary_schema", "results_schema"];',
    'for (const type of provider.settings.types) {',
    '  const keys = type.parameters.items.map((item) => item.key);',
    '  if (type.name === "sqlite") {',
    '    for (const key of expected) assert(!keys.includes(key), `sqlite: unsupported ${key}`);',
    '  } else {',
    '    for (const key of expected) assert(keys.includes(key), `${type.name}: ${key}`);',
    '  }',
    '  assert(!keys.includes("temp_schema"), `${type.name}: unsupported temp_schema`);',
    '  if (type.name === "postgresql") for (const key of ["sslmode", "sslrootcert", "sslcert", "sslkey"]) assert(keys.includes(key), key);',
    '  if (type.name === "mysql" || type.name === "mariadb") for (const key of ["ssl_required", "ssl_ca", "ssl_cert", "ssl_key"]) assert(keys.includes(key), key);',
    '  const resource = provider.asResource(type.name, "omop", {',
    '    host: "db.example.org", port: 5432, database: "/omop",',
    '    cdm_schema: "cdm space", vocabulary_schema: "vocab/catalog",',
    '    results_schema: "ohdsi-results", temp_schema: "must_not_leak"',
    '  }, { username: "user", password: "secret" });',
    '  assert(resource && resource.format === "omop.dbi.db", type.name);',
    '  if (type.name === "sqlite") {',
    '    for (const key of expected) assert(!resource.url.includes(key), `sqlite: ${key}`);',
    '  } else {',
    '    assert(resource.url.includes("cdm_schema=cdm%20space"), type.name);',
    '    assert(resource.url.includes("vocabulary_schema=vocab%2Fcatalog"), type.name);',
    '    assert(resource.url.includes("results_schema=ohdsi-results"), type.name);',
    '  }',
    '  assert(!resource.url.includes("temp_schema"), type.name);',
    '}',
    'const pg = provider.asResource("postgresql", "omop", {',
    '  host: "postgres", port: 5432, database: "omop",',
    '  cdm_schema: "cdm", vocabulary_schema: "vocab", results_schema: "results"',
    '}, { username: "user", password: "secret" });',
    'assert.equal(pg.url, "omop+dbi:postgresql://postgres:5432/omop?cdm_schema=cdm&vocabulary_schema=vocab&results_schema=results");',
    'assert.equal(pg.identity, "user");',
    'assert.equal(pg.secret, "secret");',
    'const pgTls = provider.asResource("postgresql", "secure-pg", {',
    '  host: "postgres", port: 5432, database: "omop", sslmode: "verify-full",',
    '  sslrootcert: "/srv/ca.pem", sslcert: "/srv/client.pem", sslkey: "/srv/client.key"',
    '}, { username: "user", password: "secret" });',
    'assert(pgTls.url.includes("sslmode=verify-full"));',
    'assert(pgTls.url.includes("sslrootcert=%2Fsrv%2Fca.pem"));',
    'const mariaTls = provider.asResource("mariadb", "secure-maria", {',
    '  host: "maria", port: 3306, database: "omop", ssl_required: true,',
    '  ssl_ca: "/srv/ca.pem", ssl_cert: "/srv/client.pem", ssl_key: "/srv/client.key"',
    '}, { username: "user", password: "secret" });',
    'assert(mariaTls.url.includes("ssl_required=true"));',
    'assert(mariaTls.url.includes("ssl_ca=%2Fsrv%2Fca.pem"));',
    'const ipv6 = provider.asResource("postgresql", "ipv6", {',
    '  host: "2001:db8::1", port: 5432, database: "omop?primary#one"',
    '}, { username: "user", password: "secret" });',
    'assert.equal(ipv6.url, "omop+dbi:postgresql://[2001:db8::1]:5432/omop%3Fprimary%23one");',
    'const file = provider.asResource("sqlite", "file", {',
    '  database: "/srv/omop files/a?b#c.sqlite"',
    '}, {});',
    'assert.equal(file.url, "omop+dbi:sqlite:///srv/omop%20files/a%3Fb%23c.sqlite");',
    'process.stdout.write(provider.settings.types.map((type) => type.name).join(","));'
  ), harness)

  output <- system2(node, shQuote(c(harness, resource_js)),
                    stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  expect_true(is.null(status) || identical(status, 0L), info = paste(output, collapse = "\n"))
  expect_match(paste(output, collapse = "\n"), "postgresql")
  expect_match(paste(output, collapse = "\n"), "mysql")
  expect_match(paste(output, collapse = "\n"), "mariadb")
})

test_that("the OMOP resolver unregisters through resourcer's class API", {
  previous_resolver <- .pkg_state$resolver
  expect_false(is.null(previous_resolver))

  on.exit({
    has_omop <- any(vapply(
      resourcer::getResourceResolvers(), inherits, logical(1),
      "OMOPResourceResolver"
    ))
    if (has_omop) {
      resourcer::unregisterResourceResolver("OMOPResourceResolver")
    }
    resourcer::registerResourceResolver(previous_resolver)
    .pkg_state$resolver <- previous_resolver
  }, add = TRUE)

  expect_true(any(vapply(
    resourcer::getResourceResolvers(), inherits, logical(1),
    "OMOPResourceResolver"
  )))
  expect_silent(.unregisterOMOPResourceResolver())
  expect_null(.pkg_state$resolver)
  expect_false(any(vapply(
    resourcer::getResourceResolvers(), inherits, logical(1),
    "OMOPResourceResolver"
  )))
})

test_that(".parseOmopUrl parses a full server URL", {
  p <- .parseOmopUrl(
    "omop+dbi:postgresql://db.example.org:5432/omop?cdm_schema=cdm&vocabulary_schema=vocab")
  expect_equal(p$dbms, "postgresql")
  expect_equal(p$host, "db.example.org")
  expect_equal(p$port, 5432L)
  expect_equal(p$database, "omop")
  expect_equal(p$cdm_schema, "cdm")
  expect_equal(p$vocabulary_schema, "vocab")
})

test_that(".parseOmopUrl parses results_schema (and its 'results' alias)", {
  p <- .parseOmopUrl(
    "omop+dbi:postgresql://db.example.org:5432/omop?cdm_schema=cdm&results_schema=res")
  expect_equal(p$cdm_schema, "cdm")
  expect_equal(p$results_schema, "res")
  expect_equal(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?results=res")$results_schema,
    "res")
})

test_that(".parseOmopUrl validates driver-native TLS options", {
  pg <- .parseOmopUrl(paste0(
    "omop+dbi:postgresql://h:5432/omop?sslmode=verify-full&",
    "sslrootcert=%2Fsrv%2Fca.pem&sslcert=%2Fsrv%2Fclient.pem&",
    "sslkey=%2Fsrv%2Fclient.key"
  ))
  expect_identical(pg$sslmode, "verify-full")
  expect_identical(pg$sslrootcert, "/srv/ca.pem")
  expect_identical(pg$sslcert, "/srv/client.pem")
  expect_identical(pg$sslkey, "/srv/client.key")

  maria <- .parseOmopUrl(paste0(
    "omop+dbi:mariadb://h:3306/omop?ssl_required=true&",
    "ssl_ca=%2Fsrv%2Fca.pem&ssl_cert=%2Fsrv%2Fclient.pem&",
    "ssl_key=%2Fsrv%2Fclient.key"
  ))
  expect_true(maria$ssl_required)
  expect_identical(maria$ssl_ca, "/srv/ca.pem")
  expect_identical(maria$sslcert, "/srv/client.pem")
  expect_identical(maria$sslkey, "/srv/client.key")

  expect_error(
    .parseOmopUrl("postgresql://h/db?sslmode=trust-everything"),
    "sslmode must be one of"
  )
  expect_error(
    .parseOmopUrl("mariadb://h/db?ssl_required=perhaps"),
    "ssl_required must be true or false"
  )
})

test_that(".parseOmopUrl accepts a bare scheme (no omop+dbi wrapper)", {
  p <- .parseOmopUrl("postgresql://omopdb:5432/omop?cdm_schema=cdm")
  expect_equal(p$dbms, "postgresql")
  expect_equal(p$host, "omopdb")
  expect_equal(p$port, 5432L)
  expect_equal(p$cdm_schema, "cdm")
})

test_that(".parseOmopUrl treats the port as optional", {
  p <- .parseOmopUrl("omop+dbi:postgresql://localhost/omop")
  expect_equal(p$host, "localhost")
  expect_null(p$port)
  expect_equal(p$database, "omop")
})

test_that("remote database transport defaults fail secure", {
  expect_true(.isLoopbackDatabaseHost("localhost"))
  expect_true(.isLoopbackDatabaseHost("127.0.0.1"))
  expect_true(.isLoopbackDatabaseHost("::1"))
  expect_false(.isLoopbackDatabaseHost("db.example.org"))

  expect_equal(.effectivePostgresSslMode(NULL, "db.example.org"),
               "verify-full")
  expect_equal(.effectivePostgresSslMode(NULL, "127.0.0.1"), "disable")
  expect_equal(.effectivePostgresSslMode("require", "127.0.0.1"),
               "require")

  expect_true(.effectiveMariaTlsRequired(NULL, "db.example.org"))
  expect_false(.effectiveMariaTlsRequired(NULL, "localhost"))
  expect_false(.effectiveMariaTlsRequired(FALSE, "db.example.org"))
  expect_true(.effectiveMariaTlsRequired(
    FALSE, "localhost", list(ssl_ca = "/srv/ca.pem")
  ))

  skip_if_not_installed("RMariaDB")
  expect_gte(utils::packageVersion("RMariaDB"), package_version("1.3.2"))
  flag <- .mariaTlsClientFlag()
  expect_equal(bitwAnd(flag, as.integer(RMariaDB::CLIENT_SSL)),
               as.integer(RMariaDB::CLIENT_SSL))
  expect_equal(
    bitwAnd(flag, as.integer(RMariaDB::CLIENT_SSL_VERIFY_SERVER_CERT)),
    as.integer(RMariaDB::CLIENT_SSL_VERIFY_SERVER_CERT)
  )

  call_args <- .mariaConnectionCallArgs("driver", list(host = "db.example"))
  expect_true("group" %in% names(call_args))
  expect_null(call_args$group)
})

test_that(".parseOmopUrl normalizes SQL Server spellings to 'sqlserver'", {
  expect_equal(.parseOmopUrl("omop+dbi:sql_server://h:1433/db")$dbms, "sqlserver")
  expect_equal(.parseOmopUrl("omop+dbi:mssql://h:1433/db")$dbms, "sqlserver")
  expect_equal(.normalizeDBMS("SQL Server"), "sqlserver")
  expect_equal(.normalizeDBMS("PostgreSQL"), "postgresql")
  expect_equal(.normalizeDBMS("MariaDB"), "mariadb")
  expect_equal(.normalizeDBMS("postgres"), "postgresql")
})

test_that(".parseOmopUrl handles a file URL (empty authority, absolute path)", {
  p <- .parseOmopUrl("omop+dbi:sqlite:///srv/data/omop.sqlite")
  expect_equal(p$dbms, "sqlite")
  expect_null(p$host)
  expect_null(p$port)
  expect_equal(p$database, "/srv/data/omop.sqlite")
})

test_that(".parseOmopUrl percent-decodes path and query values", {
  p <- .parseOmopUrl("omop+dbi:postgresql://h:5432/my%20db?cdm_schema=odd%2Fschema")
  expect_equal(p$database, "my db")
  expect_equal(p$cdm_schema, "odd/schema")
})

test_that(".parseOmopUrl round-trips bracketed IPv6 and reserved path bytes", {
  network <- .parseOmopUrl(
    "omop+dbi:postgresql://[2001:db8::1]:5432/omop%3Fprimary%23one"
  )
  expect_equal(network$host, "2001:db8::1")
  expect_equal(network$port, 5432L)
  expect_equal(network$database, "omop?primary#one")

  file <- .parseOmopUrl(
    "omop+dbi:sqlite:///srv/omop%20files/a%3Fb%23c.sqlite"
  )
  expect_equal(file$database, "/srv/omop files/a?b#c.sqlite")
})

test_that(".parseOmopUrl rejects the retired base64 format", {
  expect_error(.parseOmopUrl("omop+dbi:///B64:eyJhIjoxfQ"), "base64")
})

test_that(".parseOmopUrl errors on a malformed URL", {
  expect_error(.parseOmopUrl("not-a-url"), "Malformed")
  expect_error(.parseOmopUrl(""), "empty")
})

# --- Per-DBMS Default Schema (.dbmsDefaultSchema) ---

test_that(".dbmsDefaultSchema returns the correct default for every DBMS", {
  expect_equal(.dbmsDefaultSchema("postgresql"), "public")
  expect_equal(.dbmsDefaultSchema("redshift"),   "public")
  expect_equal(.dbmsDefaultSchema("sql_server"), "dbo")
  expect_equal(.dbmsDefaultSchema("synapse"),    "dbo")
  expect_equal(.dbmsDefaultSchema("pdw"),        "dbo")
  expect_equal(.dbmsDefaultSchema("mysql",   database = "mydb"), "mydb")
  expect_equal(.dbmsDefaultSchema("mariadb", database = "mydb"), "mydb")
  expect_equal(.dbmsDefaultSchema("bigquery", database = "ds"),  "ds")
  expect_equal(.dbmsDefaultSchema("oracle",  user = "scott"),    "SCOTT")
  expect_equal(.dbmsDefaultSchema("sqlite"),     "main")
  expect_equal(.dbmsDefaultSchema("duckdb"),     "main")
  expect_equal(.dbmsDefaultSchema("snowflake"),  "PUBLIC")
  expect_equal(.dbmsDefaultSchema("spark"),      "default")
  expect_equal(.dbmsDefaultSchema("databricks"), "default")
})

test_that(".dbmsDefaultSchema returns NULL when the schema cannot be inferred", {
  expect_null(.dbmsDefaultSchema("mysql"))   # database doubles as schema, none given
  expect_null(.dbmsDefaultSchema("oracle"))  # schema is the user, none given
})

# --- Four-case CDM/vocabulary schema resolution (.createHandle) ---

# A minimal stand-in for OMOPResourceClient: .createHandle only reads the parsed
# URL, the resource identity, and stores the (here unused) connection.
fake_client <- function(parsed, identity = NULL) {
  list(
    getConnection = function() NULL,
    getParsed     = function() parsed,
    getResource   = function() list(identity = identity)
  )
}

test_that("schema resolution case 1: neither set -> both the DBMS default", {
  h <- .createHandle(fake_client(.parseOmopUrl("omop+dbi:postgresql://h:5432/omop")))
  expect_equal(h$cdm_schema, "public")
  expect_equal(h$vocab_schema, "public")
})

test_that("schema resolution case 2: only CDM set -> both that schema", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm")))
  expect_equal(h$cdm_schema, "cdm")
  expect_equal(h$vocab_schema, "cdm")
})

test_that("schema resolution case 3: only vocabulary set -> CDM default, vocab apart", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?vocabulary_schema=vocab")))
  expect_equal(h$cdm_schema, "public")
  expect_equal(h$vocab_schema, "vocab")
})

test_that("schema resolution case 4: both set -> one each", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm&vocabulary_schema=vocab")))
  expect_equal(h$cdm_schema, "cdm")
  expect_equal(h$vocab_schema, "vocab")
})

test_that("results_schema from the URL is stored as an explicit pin on the handle", {
  h <- .createHandle(fake_client(.parseOmopUrl(
    "omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm&results_schema=res")))
  expect_equal(h$results_schema, "res")
  # Not declared -> no pin (effective schema is resolved later, falling back to CDM).
  h2 <- .createHandle(fake_client(.parseOmopUrl(
    "omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm")))
  expect_null(h2$results_schema)
})

test_that("schema resolution uses the connecting user for Oracle's default", {
  h <- .createHandle(fake_client(
    .parseOmopUrl("omop+dbi:oracle://h:1521/ORCL"), identity = "scott"))
  expect_equal(h$cdm_schema, "SCOTT")
  expect_equal(h$vocab_schema, "SCOTT")
})

test_that("schema overrides are validated before they can reach SQL", {
  client <- fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm"))

  expect_error(
    .createHandle(client, cdm_schema = "cdm; DROP TABLE person--"),
    "Invalid cdm_schema"
  )
  expect_error(
    .createHandle(client, results_schema = "results' OR '1'='1"),
    "Invalid results_schema"
  )
  expect_error(
    .createHandle(client, cdm_schema = "catalog.cdm"),
    "postgresql accepts at most 1 namespace component"
  )
  expect_error(.validateIdentifier(c("cdm", "other")), "must be one")
})

test_that("schema namespaces follow each DBMS qualification grammar", {
  expect_equal(.validateSchemaNamespace("postgresql", "cdm"), "cdm")
  expect_error(
    .validateSchemaNamespace("postgresql", "database.cdm", "cdm_schema"),
    "at most 1"
  )
  expect_equal(
    .validateSchemaNamespace("sqlserver", "database.cdm"),
    "database.cdm"
  )
  expect_equal(
    .validateSchemaNamespace("snowflake", "database.cdm"),
    "database.cdm"
  )
  expect_equal(
    .validateSchemaNamespace("bigquery", "project.dataset"),
    "project.dataset"
  )
  expect_error(
    .validateSchemaNamespace("mysql", "server.database", "cdm_schema"),
    "at most 1"
  )
  expect_error(.validateSchemaNamespace("postgresql", "cdm."), "Invalid")
  expect_error(.validateSchemaNamespace("postgresql", ".cdm"), "Invalid")
  expect_error(.validateSchemaNamespace("postgresql", "cdm..private"),
               "Invalid")
  expect_equal(.validateSchemaNamespace("postgresql", "CDM-Research"),
               "CDM-Research")
})

test_that("table qualification safely quotes non-default namespaces", {
  handle <- new.env(parent = emptyenv())
  handle$dbms <- "postgresql"
  handle$target_dialect <- "postgresql"
  handle$cdm_schema <- "CDM-Research"
  expect_equal(.qualifyTable(handle, "person"),
               '"CDM-Research".person')

  handle$dbms <- "mariadb"
  handle$target_dialect <- "mysql"
  handle$cdm_schema <- "cdm-research"
  expect_equal(.qualifyTable(handle, "person"), "`cdm-research`.person")

  handle$dbms <- "bigquery"
  handle$target_dialect <- "bigquery"
  handle$cdm_schema <- "project-id.dataset"
  expect_equal(.qualifyTable(handle, "person"),
               "`project-id.dataset.person`")
})

test_that("SQLite resource handles reject unreachable attached namespaces", {
  client <- fake_client(.parseOmopUrl(
    "omop+dbi:sqlite:///tmp/omop.sqlite?cdm_schema=attached"
  ))
  expect_error(
    .createHandle(client),
    "only support the main namespace"
  )
})

test_that(".createHandle closes its resource client when initialization fails", {
  closed <- 0L
  client <- fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm"))
  client$close <- function() {
    closed <<- closed + 1L
    invisible(NULL)
  }

  expect_error(
    .createHandle(client, cdm_schema = "cdm; invalid"),
    "Invalid cdm_schema"
  )
  expect_equal(closed, 1L)
})

test_that(".createHandle fails closed when resource cleanup also fails", {
  client <- fake_client(
    .parseOmopUrl("omop+dbi:postgresql://h:5432/omop?cdm_schema=cdm"))
  client$close <- function() stop("close failed")

  expect_error(
    .createHandle(client, cdm_schema = "cdm; invalid"),
    "construction failed.*cleanup could not be proven.*close failed"
  )
})

test_that("omopInitDS rejects an active resource symbol without touching it", {
  created <- FALSE
  old_handle <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .createHandle = function(...) {
      created <<- TRUE
      stop("must not be called")
    },
    .package = "dsOMOP"
  )

  run <- function() {
    duplicate_resource <- structure(list(), class = "ResourceClient")
    key <- ".dsomop_handle_duplicate_resource"
    assign(key, old_handle, envir = environment())
    on.exit(rm(list = key, envir = environment()), add = TRUE)

    expect_error(
      omopInitDS("duplicate_resource"),
      "already active"
    )
    expect_identical(get(key, envir = environment()), old_handle)
  }

  run()
  expect_false(created)
})

test_that("omopInitDS bootstraps privacy before creating a handle", {
  events <- character(0)
  new_handle <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .dsomopDpEnsureRuntime = function() {
      events <<- c(events, "privacy")
      invisible(NULL)
    },
    .createHandle = function(...) {
      events <<- c(events, "handle")
      new_handle
    },
    .buildBlueprint = function(...) {
      events <<- c(events, "blueprint")
      invisible(NULL)
    },
    .package = "dsOMOP"
  )

  run <- function() {
    ready_resource <- structure(list(), class = "ResourceClient")
    key <- ".dsomop_handle_ready_resource"
    on.exit({
      if (exists(key, envir = environment(), inherits = FALSE)) {
        rm(list = key, envir = environment())
      }
    }, add = TRUE)
    expect_true(omopInitDS("ready_resource"))
  }

  run()
  expect_identical(events, c("privacy", "handle", "blueprint"))
})

test_that("omopInitDS closes a new handle when blueprint construction fails", {
  closed <- 0L
  new_handle <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .createHandle = function(...) new_handle,
    .buildBlueprint = function(...) stop("blueprint failed"),
    .closeHandle = function(handle) {
      expect_identical(handle, new_handle)
      closed <<- closed + 1L
      invisible(NULL)
    },
    .package = "dsOMOP"
  )

  run <- function() {
    failing_resource <- structure(list(), class = "ResourceClient")
    key <- ".dsomop_handle_failing_resource"
    expect_error(omopInitDS("failing_resource"), "blueprint failed")
    expect_false(exists(key, envir = environment(), inherits = FALSE))
  }

  run()
  expect_equal(closed, 1L)
})

test_that("omopInitDS retains a failed handle when cleanup cannot be proven", {
  new_handle <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .createHandle = function(...) new_handle,
    .buildBlueprint = function(...) stop("blueprint failed"),
    .closeHandle = function(...) stop("close failed"),
    .package = "dsOMOP"
  )

  run <- function() {
    retained_resource <- structure(list(), class = "ResourceClient")
    key <- ".dsomop_handle_retained_resource"
    on.exit({
      if (exists(key, envir = environment(), inherits = FALSE)) {
        rm(list = key, envir = environment())
      }
    }, add = TRUE)

    expect_error(
      omopInitDS("retained_resource"),
      "cleanup could not be proven.*retained"
    )
    expect_identical(get(key, envir = environment()), new_handle)
  }

  run()
})

# ===========================================================================
# DATEADD Translation Tests — one per dialect
# ===========================================================================

# --- PostgreSQL: (expr + N * INTERVAL '1 day') ---

test_that("DATEADD translates correctly for postgresql", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "postgresql")
  expect_true(grepl("start_date", sql))
  expect_true(grepl("30 \\* INTERVAL '1 day'", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("DATEADD with negative days for postgresql", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "postgresql")
  expect_true(grepl("-7 \\* INTERVAL '1 day'", sql))
})

# --- MySQL / MariaDB: DATE_ADD(expr, INTERVAL N DAY) ---

test_that("DATEADD translates correctly for mysql", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "mysql")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL 30 DAY", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("DATEADD with negative days for mysql", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "mysql")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL -7 DAY", sql))
})

# --- Oracle: (expr + N) ---

test_that("DATEADD translates correctly for oracle", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "oracle")
  expect_true(grepl("start_date \\+ 30", sql))
  expect_false(grepl("DATEADD", sql))
  expect_false(grepl("INTERVAL", sql))
})

test_that("DATEADD with negative days for oracle", {
  sql <- .sql_translate("DATEADD(day, -7, end_date)", "oracle")
  expect_true(grepl("end_date \\+ -7", sql))
})

# --- BigQuery: DATE_ADD(expr, INTERVAL N DAY) ---

test_that("DATEADD translates correctly for bigquery", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "bigquery")
  expect_true(grepl("DATE_ADD", sql))
  expect_true(grepl("INTERVAL 30 DAY", sql))
  expect_false(grepl("DATEADD", sql))
})

# --- Spark: DATE_ADD(expr, N) ---

test_that("DATEADD translates correctly for spark", {
  sql <- .sql_translate("DATEADD(day, 10, obs_date)", "spark")
  expect_true(grepl("DATE_ADD\\(obs_date, 10\\)", sql))
  expect_false(grepl("DATEADD", sql))
  expect_false(grepl("INTERVAL", sql))
})

# --- SQLite / DuckDB: DATE(expr, 'N days') ---

test_that("DATEADD translates correctly for sqlite", {
  sql <- .sql_translate("DATEADD(day, 5, start_date)", "sqlite")
  expect_true(grepl("DATE\\(start_date", sql))
  expect_true(grepl("5 days", sql))
  expect_false(grepl("DATEADD", sql))
})

# --- SQL Server / Redshift / Snowflake: DATEADD preserved (native) ---

test_that("DATEADD is preserved for sql server (native)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "sql server")
  expect_true(grepl("DATEADD", sql))
  expect_equal(sql, "DATEADD(day, 30, start_date)")
})

test_that("DATEADD is preserved for redshift (native)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "redshift")
  expect_equal(sql, "DATEADD(day, 30, start_date)")
})

test_that("DATEADD is preserved for snowflake (native)", {
  sql <- .sql_translate("DATEADD(day, 30, start_date)", "snowflake")
  expect_equal(sql, "DATEADD(day, 30, start_date)")
})

# ===========================================================================
# TOP/LIMIT Translation Tests — one per dialect
# ===========================================================================

# --- SQL Server: TOP preserved (native) ---

test_that("TOP is preserved for sql server", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "sql server")
  expect_true(grepl("TOP 10", sql))
  expect_false(grepl("LIMIT", sql))
})

# --- Oracle: FETCH FIRST N ROWS ONLY ---

test_that("TOP converts to FETCH FIRST for oracle", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "oracle")
  expect_true(grepl("FETCH FIRST 10 ROWS ONLY", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("LIMIT", sql))
})

# --- PostgreSQL: LIMIT N ---

test_that("TOP converts to LIMIT for postgresql", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "postgresql")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

# --- MySQL: LIMIT N ---

test_that("TOP converts to LIMIT for mysql", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "mysql")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

# --- SQLite: LIMIT N ---

test_that("TOP converts to LIMIT for sqlite", {
  sql <- .sql_translate("SELECT TOP 5 * FROM observation", "sqlite")
  expect_true(grepl("LIMIT 5", sql))
  expect_false(grepl("TOP", sql))
})

# --- BigQuery: LIMIT N ---

test_that("TOP converts to LIMIT for bigquery", {
  sql <- .sql_translate("SELECT TOP 100 * FROM person", "bigquery")
  expect_true(grepl("LIMIT 100", sql))
  expect_false(grepl("TOP", sql))
})

# --- Redshift: LIMIT N ---

test_that("TOP converts to LIMIT for redshift", {
  sql <- .sql_translate("SELECT TOP 50 * FROM person", "redshift")
  expect_true(grepl("LIMIT 50", sql))
  expect_false(grepl("TOP", sql))
})

# --- Snowflake: LIMIT N ---

test_that("TOP converts to LIMIT for snowflake", {
  sql <- .sql_translate("SELECT TOP 20 * FROM person", "snowflake")
  expect_true(grepl("LIMIT 20", sql))
  expect_false(grepl("TOP", sql))
})

# --- Spark: LIMIT N ---

test_that("TOP converts to LIMIT for spark", {
  sql <- .sql_translate("SELECT TOP 10 * FROM person", "spark")
  expect_true(grepl("LIMIT 10", sql))
  expect_false(grepl("TOP", sql))
})

# ===========================================================================
# Combined Translation Tests (DATEADD + TOP in same query)
# ===========================================================================

test_that("combined DATEADD + TOP translates correctly for postgresql", {
  sql <- .sql_translate(
    "SELECT TOP 10 * FROM observation WHERE obs_date > DATEADD(day, -30, GETDATE())",
    "postgresql"
  )
  expect_true(grepl("LIMIT 10", sql))
  expect_true(grepl("INTERVAL '1 day'", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("combined DATEADD + TOP translates correctly for oracle", {
  sql <- .sql_translate(
    "SELECT TOP 5 * FROM drug_exposure WHERE start_date > DATEADD(day, -90, end_date)",
    "oracle"
  )
  expect_true(grepl("FETCH FIRST 5 ROWS ONLY", sql))
  expect_true(grepl("end_date \\+ -90", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("DATEADD", sql))
})

test_that("combined DATEADD + TOP translates correctly for mysql", {
  sql <- .sql_translate(
    "SELECT TOP 10 * FROM person WHERE birth_date > DATEADD(day, 365, start_date)",
    "mysql"
  )
  expect_true(grepl("LIMIT 10", sql))
  expect_true(grepl("DATE_ADD.*INTERVAL 365 DAY", sql))
  expect_false(grepl("TOP", sql))
  expect_false(grepl("DATEADD", sql))
})

# ===========================================================================
# No-op / passthrough tests
# ===========================================================================

test_that("SQL without DATEADD or TOP passes through unchanged", {
  original <- "SELECT person_id, gender_concept_id FROM person WHERE year_of_birth > 1990"
  for (dialect in c("postgresql", "mysql", "oracle", "sqlite", "bigquery", "spark", "sql server")) {
    expect_equal(.sql_translate(original, dialect), original)
  }
})

test_that("NULL or empty dialect returns SQL unchanged", {
  sql <- "SELECT TOP 10 * FROM person"
  expect_equal(.sql_translate(sql, NULL), sql)
  expect_equal(.sql_translate(sql, ""), sql)
})

# ===========================================================================
# Parameter Substitution Tests (.sql_render)
# ===========================================================================

test_that(".sql_render substitutes parameters correctly", {
  sql <- .sql_render("SELECT * FROM @schema.@table WHERE id = @id",
                     schema = "cdm", table = "person", id = "42")
  expect_equal(sql, "SELECT * FROM cdm.person WHERE id = 42")
})

test_that(".sql_render handles longest-first substitution", {
  # @schema_name should not be partially replaced by a shorter @schema param
  sql <- .sql_render("SELECT * FROM @schema_name.person",
                     schema_name = "my_cdm", schema = "WRONG")
  expect_equal(sql, "SELECT * FROM my_cdm.person")
})

# ===========================================================================
# Statement Splitting Tests (.sql_split)
# ===========================================================================

test_that(".sql_split splits on semicolons outside quotes", {
  stmts <- .sql_split("SELECT 1; SELECT 2; SELECT 3")
  expect_equal(length(stmts), 3L)
  expect_equal(stmts[1], "SELECT 1")
})

test_that(".sql_split preserves semicolons inside single quotes", {
  stmts <- .sql_split("SELECT 'hello; world' FROM t; SELECT 2")
  expect_equal(length(stmts), 2L)
  expect_true(grepl("hello; world", stmts[1]))
})

# ===========================================================================
# Dialect Mapping Completeness
# ===========================================================================

test_that("all DBMS in resource.js enum are mapped in dialect resolver", {
  dbms_list <- c("postgresql", "sqlite", "mysql", "mariadb",
                 "sql_server", "synapse", "pdw",
                 "oracle", "redshift",
                 "bigquery", "snowflake", "spark", "databricks",
                 "duckdb")
  for (dbms in dbms_list) {
    expect_silent(.resolve_target_dialect(dbms))
  }
})

test_that("dialect aliases resolve correctly", {
  expect_equal(.resolve_target_dialect("postgres"), "postgresql")
  expect_equal(.resolve_target_dialect("sqlserver"), "sql server")
  expect_equal(.resolve_target_dialect("synapse"), "sql server")
  expect_equal(.resolve_target_dialect("pdw"), "sql server")
  expect_equal(.resolve_target_dialect("duckdb"), "duckdb")
  expect_equal(.resolve_target_dialect("databricks"), "spark")
  expect_equal(.resolve_target_dialect("mysql"), "mysql")
  expect_equal(.resolve_target_dialect("mariadb"), "mysql")
})

test_that("unsupported DBMS throws error", {
  expect_error(.resolve_target_dialect("mongodb"), "Unsupported DBMS")
  expect_error(.resolve_target_dialect("cassandra"), "Unsupported DBMS")
})
