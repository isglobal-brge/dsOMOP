var dsOMOP = {
  settings: {
    title: "OMOP CDM Database Resources (HADES)",
    description: "Provides access to OMOP CDM databases. Supports PostgreSQL, SQL Server, Oracle, Redshift, BigQuery, Snowflake, Spark, and SQLite.",
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
        title: "OMOP CDM Database (HADES)",
        description: "Connection to an OMOP CDM database via OHDSI HADES tooling.",
        tags: ["omop-cdm"],
        parameters: {
          "$schema": "http://json-schema.org/draft-07/schema#",
          type: "object",
          properties: {
            dbms: {
              type: "string",
              title: "Database Engine",
              enum: ["postgresql", "sql_server", "oracle", "redshift", "bigquery", "snowflake", "spark", "sqlite"]
            },
            server: {
              type: "string",
              title: "Server",
              description: "host/database (DatabaseConnector format)"
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
          required: ["dbms", "server", "port"]
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

    var url = "omop+hades:///dbms=" + params.dbms
            + ";server=" + params.server
            + ";port=" + params.port;
    if (params.cdm_schema) url += ";cdm_schema=" + params.cdm_schema;
    if (params.vocabulary_schema) url += ";vocabulary_schema=" + params.vocabulary_schema;
    if (params.results_schema) url += ";results_schema=" + params.results_schema;
    if (params.temp_schema) url += ";temp_schema=" + params.temp_schema;

    return {
      name: name,
      url: url,
      format: "omop.hades.db",
      identity: credentials.username,
      secret: credentials.password
    };
  }
};
