var dsOMOP = {
  settings: {
    title: "OMOP CDM Database Resources",
    description: "Provides access to OMOP CDM databases via DBI. Supports PostgreSQL, SQLite, DuckDB, MySQL/MariaDB, SQL Server/Synapse/PDW, Oracle, Redshift, BigQuery, Snowflake, Spark, and Databricks.",
    web: "https://github.com/isglobal-brge/dsOMOP",
    categories: [
      {
        name: "omop-cdm",
        title: "OMOP CDM",
        description: "Observational Medical Outcomes Partnership Common Data Model. See <a href='https://ohdsi.github.io/CommonDataModel/' target='_blank'>OHDSI CDM documentation</a>."
      }
    ],
    types: [
      {
        name: "omop-cdm-db",
        title: "OMOP CDM Database (server)",
        description: "Connection to a networked OMOP CDM database (host + port + credentials). PostgreSQL and Redshift work out of the box; the other engines need their R driver package installed on the Rock server.",
        tags: ["omop-cdm"],
        parameters: {
          "$schema": "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            dbms: {
              type: "string",
              title: "Database Engine",
              enum: [
                "postgresql", // bundled (RPostgres) — works out of the box
                "redshift",   // bundled (RPostgres, PostgreSQL wire-compatible)
                "mysql",      // needs RMariaDB
                "mariadb",    // needs RMariaDB
                "sql_server", // needs odbc + an ODBC driver
                "synapse",    // needs odbc + an ODBC driver
                "pdw",        // needs odbc + an ODBC driver
                "oracle",     // needs ROracle (Instant Client) or odbc
                "snowflake",  // needs odbc + Snowflake ODBC driver
                "spark",      // needs odbc + Simba Spark ODBC driver
                "databricks", // needs odbc + Databricks ODBC driver
                "bigquery"    // needs bigrquery (host = GCP project, database = dataset)
              ]
            },
            host: {
              type: "string",
              title: "Host",
              description: "Database server hostname or IP. For BigQuery, the GCP project ID."
            },
            port: {
              type: "integer",
              title: "Port"
            },
            database: {
              type: "string",
              title: "Database",
              description: "Database name. For BigQuery, the dataset; for Oracle, the SID/service name."
            },
            cdm_schema: {
              type: "string",
              title: "CDM Schema",
              description: "Schema holding the OMOP CDM tables (e.g. 'cdm'). Leave empty to use the engine's default schema (PostgreSQL/Redshift 'public', SQL Server 'dbo', Oracle the user, MySQL the database, ...)."
            },
            vocabulary_schema: {
              type: "string",
              title: "Vocabulary Schema",
              description: "Schema holding the vocabulary tables, if separate from the CDM schema. Defaults to the CDM schema."
            },
            results_schema: {
              type: "string",
              title: "Results Schema",
              description: "Schema for cohort tables and analysis results."
            },
            temp_schema: {
              type: "string",
              title: "Temp Schema",
              description: "Schema for temporary tables. If not set, native DB temp tables are used."
            },
            warehouse: {
              type: "string",
              title: "Warehouse",
              description: "Snowflake virtual warehouse (Snowflake only; defaults to COMPUTE_WH)."
            },
            driver: {
              type: "string",
              title: "ODBC Driver",
              description: "ODBC driver name to override the default (SQL Server / Snowflake / Spark / Databricks / Oracle-via-ODBC only)."
            }
          },
          required: ["dbms", "host", "port"]
        },
        credentials: {
          "$schema": "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            username: {
              type: "string",
              title: "Username"
            },
            password: {
              type: "string",
              title: "Password",
              format: "password"
            }
          },
          required: ["username", "password"]
        }
      },
      {
        name: "omop-cdm-file",
        title: "OMOP CDM Database (file)",
        description: "Connection to a file-backed OMOP CDM database on the Rock server. SQLite needs RSQLite; DuckDB needs the duckdb package.",
        tags: ["omop-cdm"],
        parameters: {
          "$schema": "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            dbms: {
              type: "string",
              title: "Database Engine",
              enum: [
                "sqlite", // needs RSQLite
                "duckdb"  // needs duckdb
              ]
            },
            database: {
              type: "string",
              title: "Database File",
              description: "Absolute path to the database file on the Rock server (e.g. '/srv/omop/cdm.sqlite')."
            },
            cdm_schema: {
              type: "string",
              title: "CDM Schema",
              description: "Schema holding the OMOP CDM tables (DuckDB only; SQLite ignores schemas). Defaults to 'main'."
            },
            vocabulary_schema: {
              type: "string",
              title: "Vocabulary Schema",
              description: "Schema holding the vocabulary tables, if separate (DuckDB only). Defaults to the CDM schema."
            }
          },
          required: ["dbms", "database"]
        }
      }
    ]
  },

  asResource: function(type, name, params, credentials) {
    if (type !== "omop-cdm-db" && type !== "omop-cdm-file") return undefined;

    var enc = encodeURIComponent;

    // Optional query parameters (schemas + engine extras), only when non-empty.
    var query = [];
    function addQuery(key, val) {
      if (val !== undefined && val !== null && String(val).length > 0) {
        query.push(key + "=" + enc(val));
      }
    }
    addQuery("cdm_schema", params.cdm_schema);
    addQuery("vocabulary_schema", params.vocabulary_schema);
    addQuery("results_schema", params.results_schema);
    addQuery("temp_schema", params.temp_schema);
    addQuery("warehouse", params.warehouse);
    addQuery("driver", params.driver);
    var qs = query.length > 0 ? ("?" + query.join("&")) : "";

    if (type === "omop-cdm-file") {
      // File-backed engine: empty authority + absolute path.
      var path = String(params.database || "").replace(/^\/+/, "");
      return {
        name: name,
        url: "omop+dbi:" + params.dbms + ":///" + path + qs,
        format: "omop.dbi.db"
      };
    }

    // Networked engine: <dbms>://host[:port]/database
    var authority = params.host || "";
    if (params.port !== undefined && params.port !== null && String(params.port).length > 0) {
      authority += ":" + params.port;
    }
    var db = String(params.database || "").replace(/^\/+/, "");

    return {
      name: name,
      url: "omop+dbi:" + params.dbms + "://" + authority + "/" + db + qs,
      format: "omop.dbi.db",
      identity: credentials.username,
      secret: credentials.password
    };
  }
};
