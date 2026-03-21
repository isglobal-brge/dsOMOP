var dsOMOP = {
  settings: {
    title: "OMOP CDM Database Resources",
    description: "Provides access to OMOP CDM databases via DBI. Supports PostgreSQL, SQL Server, Oracle, Redshift, BigQuery, Snowflake, Spark, and SQLite.",
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
        title: "OMOP CDM Database",
        description: "Connection to an OMOP CDM database via DBI.",
        tags: ["omop-cdm"],
        parameters: {
          "$schema": "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            dbms: {
              type: "string",
              title: "Database Engine",
              enum: [
                "postgresql",
                "sqlite"
                // Requires additional packages (not pre-installed in Rock base):
                // "mysql",       // needs: install.packages("RMariaDB")
                // "mariadb",     // needs: install.packages("RMariaDB")
                // "sql_server",  // needs: install.packages("odbc") + unixODBC + MS ODBC driver
                // "oracle",      // needs: install.packages("odbc") + Oracle Instant Client
                // "redshift",    // needs: install.packages("odbc")
                // "bigquery",    // needs: install.packages("bigrquery")
                // "snowflake",   // needs: install.packages("odbc") + Snowflake ODBC driver
                // "spark",       // needs: install.packages("odbc") + Simba Spark ODBC driver
                // "databricks",  // needs: install.packages("odbc") + Databricks ODBC driver
                // "duckdb",      // needs: install.packages("duckdb")
              ]
            },
            host: {
              type: "string",
              title: "Host",
              description: "Database server hostname or IP address"
            },
            database: {
              type: "string",
              title: "Database",
              description: "Database name"
            },
            port: {
              type: "integer",
              title: "Port"
            },
            cdm_schema: {
              type: "string",
              title: "CDM Schema",
              description: "Schema containing OMOP CDM tables (e.g., 'cdm'). Leave empty for default schema."
            },
            vocabulary_schema: {
              type: "string",
              title: "Vocabulary Schema",
              description: "Schema containing vocabulary tables if separate from CDM schema."
            },
            results_schema: {
              type: "string",
              title: "Results Schema",
              description: "Schema for cohort tables and analysis results."
            },
            temp_schema: {
              type: "string",
              title: "Temp Schema",
              description: "Schema for temporary tables. If not set, DB temp tables are used."
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
      }
    ]
  },

  asResource: function(type, name, params, credentials) {
    if (type !== "omop-cdm-db") return undefined;

    // Build params object (only non-empty values)
    var config = {
      dbms: params.dbms,
      host: params.host,
      port: params.port
    };
    if (params.database) config.database = params.database;
    if (params.cdm_schema) config.cdm_schema = params.cdm_schema;
    if (params.vocabulary_schema) config.vocabulary_schema = params.vocabulary_schema;
    if (params.results_schema) config.results_schema = params.results_schema;
    if (params.temp_schema) config.temp_schema = params.temp_schema;

    // JSON -> base64url (safe for Opal R parser: no ?, &, =, +, / in URL body)
    var json = JSON.stringify(config);
    var b64 = btoa(json)
      .replace(/\+/g, "-")
      .replace(/\//g, "_")
      .replace(/=+$/, "");

    return {
      name: name,
      url: "omop+dbi:///B64:" + b64,
      format: "omop.dbi.db",
      identity: credentials.username,
      secret: credentials.password
    };
  }
};
