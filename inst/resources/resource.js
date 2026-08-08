// dsOMOP resource provider for Opal.
//
// Exposes an "OMOP CDM Database" category whose types are the individual
// database engines dsOMOP can resolve. Each type shows only the fields that
// engine needs; the CDM, vocabulary, and results namespaces are optional.
//
// The Opal resource form renderer consumes the OBiBa schema-form dialect:
// parameters/credentials are { type: "array", items: [ { key, type, title } ] }
// with a separate "required" list. (Standard JSON-Schema "object"/"properties"
// is NOT rendered and falls back to a generic form under "Others".)
//
// asResource builds the readable URL the dsOMOP resolver parses (R/resource.R,
// .parseOmopUrl):
//   omop+dbi:<dbms>://<host>[:<port>]/<database>?cdm_schema=...&vocabulary_schema=...&results_schema=...
// File engines use an empty authority and an absolute path:
//   omop+dbi:sqlite:///srv/data/omop.sqlite
// The resolver matches on format == "omop.dbi.db".
//
// temp_schema is deliberately not exposed here: dsOMOP's working tables are
// connection-scoped TEMPORARY tables whose namespace is managed by the DBMS.
// Merely persisting a temp_schema URL parameter would promise routing that the
// query layer does not perform.
// SQLite likewise exposes no schema fields: the resource opens one database
// file and does not ATTACH additional databases, so separate CDM, vocabulary,
// or results namespaces would not be reachable through this provider.
var dsOMOP = (function () {

  var SCHEMA = "http://json-schema.org/schema#";

  // --- field builders (the engine-specific form inputs) --------------------
  function host(title) { return { key: "host", type: "string", title: title || "Host name or IP address" }; }
  function database(title) { return { key: "database", type: "string", title: title || "Database name" }; }
  var port = { key: "port", type: "integer", title: "Port number" };
  var cdmSchema = {
    key: "cdm_schema", type: "string", title: "CDM schema (optional)",
    description: "Schema holding the OMOP CDM tables. If omitted, the engine default is used."
  };
  var vocabularySchema = {
    key: "vocabulary_schema", type: "string", title: "Vocabulary schema (optional)",
    description: "Schema holding the vocabulary tables, when separate from the CDM schema. Defaults to the CDM schema."
  };
  var resultsSchema = {
    key: "results_schema", type: "string", title: "Results schema (optional)",
    description: "Schema containing Achilles, Data Quality Dashboard, and other OHDSI/HADES result tables. If omitted, dsOMOP probes only the configured CDM schema and schemas explicitly allowlisted by the data controller."
  };
  var warehouse = {
    key: "warehouse", type: "string", title: "Warehouse (optional)",
    description: "Snowflake virtual warehouse. Defaults to COMPUTE_WH."
  };
  var driver = {
    key: "driver", type: "string", title: "ODBC driver (optional)",
    description: "ODBC driver name to override the engine default."
  };
  var pgSslMode = {
    key: "sslmode", type: "string", title: "TLS mode (optional)",
    description: "libpq mode: disable, allow, prefer, require, verify-ca, or verify-full. If omitted, remote hosts use verify-full and loopback uses disable."
  };
  var pgSslRootCert = { key: "sslrootcert", type: "string", title: "TLS CA certificate path (optional)" };
  var pgSslCert = { key: "sslcert", type: "string", title: "TLS client certificate path (optional)" };
  var pgSslKey = { key: "sslkey", type: "string", title: "TLS client key path (optional)" };
  var mariaSslRequired = {
    key: "ssl_required", type: "boolean", title: "Require authenticated TLS transport",
    description: "If omitted, authenticated TLS is required for remote hosts and optional only on loopback. Supply ssl_ca when the server CA is not in the connector trust store."
  };
  var mariaSslCa = { key: "ssl_ca", type: "string", title: "Trusted TLS CA certificate path (optional)" };
  var mariaSslCert = { key: "ssl_cert", type: "string", title: "TLS client certificate path (optional)" };
  var mariaSslKey = { key: "ssl_key", type: "string", title: "TLS client key path (optional)" };
  var PG_TLS = [pgSslMode, pgSslRootCert, pgSslCert, pgSslKey];
  var MARIA_TLS = [mariaSslRequired, mariaSslCa, mariaSslCert, mariaSslKey];
  var SCHEMAS = [cdmSchema, vocabularySchema, resultsSchema];

  // --- credentials blocks ---------------------------------------------------
  // Every type MUST carry a credentials block, even when none is needed: a type
  // without one makes Opal's /ws/resource-providers endpoint throw (500).
  var CREDS_USERPASS = {
    "$schema": SCHEMA,
    type: "array",
    items: [
      { key: "username", type: "string", title: "Username" },
      { key: "password", type: "string", title: "Password", format: "password" }
    ],
    required: ["username", "password"]
  };
  function credsNone(description) {
    return { "$schema": SCHEMA, type: "array", items: [], description: description };
  }

  // --- type builder ----------------------------------------------------------
  function dbType(name, title, description, items, required, credentials) {
    return {
      name: name,
      title: title,
      description: description,
      tags: ["omop-cdm"],
      parameters: { "$schema": SCHEMA, type: "array", items: items, required: required },
      credentials: credentials
    };
  }

  var NET_REQ = ["host", "port", "database"];
  var DBI = '<a href="https://www.r-dbi.org" target="_blank">DBI</a>';

  var types = [
    dbType("postgresql", "PostgreSQL",
      "Connection to a PostgreSQL OMOP CDM database via " + DBI + " (requires RPostgres or RPostgreSQL and site integration testing).",
      [host(), port, database()].concat(SCHEMAS, PG_TLS), NET_REQ, CREDS_USERPASS),

    dbType("redshift", "Amazon Redshift",
      "Connection to an Amazon Redshift OMOP CDM database via " + DBI + " (uses RPostgres; requires site integration testing).",
      [host(), port, database()].concat(SCHEMAS), NET_REQ, CREDS_USERPASS),

    dbType("mysql", "MySQL",
      "Connection to a MySQL OMOP CDM database via " + DBI + " (needs the RMariaDB driver on the Rock server).",
      [host(), port, database()].concat(SCHEMAS, MARIA_TLS), NET_REQ, CREDS_USERPASS),

    dbType("mariadb", "MariaDB",
      "Connection to a MariaDB OMOP CDM database via " + DBI + " (needs the RMariaDB driver on the Rock server).",
      [host(), port, database()].concat(SCHEMAS, MARIA_TLS), NET_REQ, CREDS_USERPASS),

    dbType("sqlserver", "Microsoft SQL Server",
      "Connection to a SQL Server OMOP CDM database via " + DBI + " (needs the odbc package and a SQL Server ODBC driver).",
      [host(), port, database()].concat(SCHEMAS, [driver]), NET_REQ, CREDS_USERPASS),

    dbType("synapse", "Azure Synapse",
      "Connection to an Azure Synapse OMOP CDM database via " + DBI + " (needs the odbc package and a SQL Server ODBC driver).",
      [host(), port, database()].concat(SCHEMAS, [driver]), NET_REQ, CREDS_USERPASS),

    dbType("pdw", "Parallel Data Warehouse",
      "Connection to a Microsoft PDW OMOP CDM database via " + DBI + " (needs the odbc package and a SQL Server ODBC driver).",
      [host(), port, database()].concat(SCHEMAS, [driver]), NET_REQ, CREDS_USERPASS),

    dbType("oracle", "Oracle",
      "Connection to an Oracle OMOP CDM database via " + DBI + " (needs ROracle with the Instant Client, or the odbc package).",
      [host(), port, database("Database (SID or service name)")].concat(SCHEMAS, [driver]), NET_REQ, CREDS_USERPASS),

    dbType("snowflake", "Snowflake",
      "Connection to a Snowflake OMOP CDM database via " + DBI + " (needs the odbc package and the Snowflake ODBC driver).",
      [host("Account identifier"), database()].concat(SCHEMAS, [warehouse, driver]), ["host", "database"], CREDS_USERPASS),

    dbType("spark", "Spark",
      "Connection to a Spark SQL OMOP CDM database via " + DBI + " (needs the odbc package and the Simba Spark ODBC driver).",
      [host(), port, database()].concat(SCHEMAS, [driver]), NET_REQ, CREDS_USERPASS),

    dbType("databricks", "Databricks",
      "Connection to a Databricks OMOP CDM database via " + DBI + " (needs the odbc package and the Databricks ODBC driver).",
      [host(), port, database()].concat(SCHEMAS, [driver]), NET_REQ, CREDS_USERPASS),

    dbType("bigquery", "Google BigQuery",
      "Connection to a Google BigQuery OMOP CDM dataset via " + DBI + " (needs the bigrquery package).",
      [host("GCP project ID"), database("Dataset")].concat(SCHEMAS), ["host", "database"],
      credsNone("BigQuery authenticates through the Google Cloud credentials configured on the Rock server; no username or password is stored on the resource.")),

    dbType("sqlite", "SQLite (file)",
      "Connection to a file-backed SQLite OMOP CDM database on the Rock server (needs the RSQLite package).",
      [database("Database file path on the Rock server")], ["database"],
      credsNone("File-backed engine opened directly from the Rock server filesystem; no credentials required.")),

    dbType("duckdb", "DuckDB (file)",
      "Connection to a file-backed DuckDB OMOP CDM database on the Rock server (needs the duckdb package).",
      [database("Database file path on the Rock server")].concat(SCHEMAS), ["database"],
      credsNone("File-backed engine opened directly from the Rock server filesystem; no credentials required."))
  ];

  var settings = {
    title: "OMOP CDM Database Resources",
    description: "Provides access to OMOP CDM databases via DBI. One type per database engine; the dsOMOP resolver connects on demand.",
    web: "https://github.com/isglobal-brge/dsOMOP",
    categories: [
      {
        name: "omop-cdm",
        title: "OMOP CDM Database",
        description: 'Connection to an <a href="https://www.ohdsi.org/data-standardization/" target="_blank">OMOP CDM</a> database, resolved by dsOMOP via DBI.'
      }
    ],
    types: types
  };

  // Build the readable resource URL from the engine type + form input. Kept
  // self-contained (no closure references) so it survives serialization.
  function asResource(type, name, params, credentials) {
    var fileEngines = { sqlite: true, duckdb: true };
    var noCredEngines = { sqlite: true, duckdb: true, bigquery: true };
    var noPortEngines = { snowflake: true, bigquery: true };
    var known = {
      postgresql: true, redshift: true, mysql: true, mariadb: true,
      sqlserver: true, synapse: true, pdw: true, oracle: true,
      snowflake: true, spark: true, databricks: true, bigquery: true,
      sqlite: true, duckdb: true
    };
    if (!known[type]) return undefined;

    var enc = encodeURIComponent;
    var query = [];
    function encodePath(value) {
      return String(value || "").split("/").map(enc).join("/");
    }
    function encodeHost(value) {
      var raw = String(value || "");
      if (raw.charAt(0) === "[" && raw.charAt(raw.length - 1) === "]") {
        raw = raw.slice(1, -1);
      }
      if (raw.indexOf(":") >= 0) {
        if (!/^[0-9A-Fa-f:.%A-Za-z0-9_-]+$/.test(raw)) {
          throw new Error("Invalid IPv6 host");
        }
        return "[" + raw.replace(/%/g, "%25") + "]";
      }
      return enc(raw);
    }
    function addQuery(key, val) {
      if (val !== undefined && val !== null && String(val).length > 0) {
        query.push(key + "=" + enc(val));
      }
    }
    if (type !== "sqlite") {
      addQuery("cdm_schema", params.cdm_schema);
      addQuery("vocabulary_schema", params.vocabulary_schema);
      addQuery("results_schema", params.results_schema);
    }
    addQuery("warehouse", params.warehouse);
    addQuery("driver", params.driver);
    if (type === "postgresql") {
      addQuery("sslmode", params.sslmode);
      addQuery("sslrootcert", params.sslrootcert);
      addQuery("sslcert", params.sslcert);
      addQuery("sslkey", params.sslkey);
    }
    if (type === "mysql" || type === "mariadb") {
      addQuery("ssl_required", params.ssl_required);
      addQuery("ssl_ca", params.ssl_ca);
      addQuery("ssl_cert", params.ssl_cert);
      addQuery("ssl_key", params.ssl_key);
    }
    var qs = query.length > 0 ? ("?" + query.join("&")) : "";

    if (fileEngines[type]) {
      var path = String(params.database || "").replace(/^\/+/, "");
      return { name: name, url: "omop+dbi:" + type + ":///" + encodePath(path) + qs, format: "omop.dbi.db" };
    }

    var authority = encodeHost(params.host);
    if (!noPortEngines[type] && params.port !== undefined && params.port !== null && String(params.port).length > 0) {
      authority += ":" + params.port;
    }
    var db = String(params.database || "").replace(/^\/+/, "");
    var url = "omop+dbi:" + type + "://" + authority + "/" + enc(db) + qs;

    if (noCredEngines[type]) {
      return { name: name, url: url, format: "omop.dbi.db" };
    }
    return {
      name: name, url: url, format: "omop.dbi.db",
      identity: credentials.username, secret: credentials.password
    };
  }

  return { settings: settings, asResource: asResource };
})();
