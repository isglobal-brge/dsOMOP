var dsOMOP = {
  settings: {
    "title": "OMOP CDM database resources",
    "description": "Provides structured access to OMOP CDM databases, supporting various database management systems.",
    "web": "https://github.com/isglobal-brge/dsOMOP",
    "categories": [
      {
        "name": "omop-cdm-db",
        "title": "OMOP CDM database",
        "description": "The resource is an <a href=\"https://www.ohdsi.org/data-standardization/\" target=\"_blank\">OMOP CDM</a> database."
      }
    ],
    "types": [
      {
        "name": "omop-cdm-db",
        "title": "Database connection",
        "description": "Connection to an OMOP CDM database. The connection will be established using <a href=\"https://www.r-dbi.org\" target=\"_blank\">DBI</a>.",
        "tags": ["omop-cdm-db"],
        "parameters": {
          "$schema": "http://json-schema.org/schema#",
          "type": "array",
          "items": [
            {
              "key": "driver",
              "type": "string",
              "title": "Database engine",
              "enum": [
                { "key": "mariadb", "title": "MariaDB" },
                { "key": "mysql", "title": "MySQL" },
                { "key": "postgresql", "title": "PostgreSQL" }
              ]
            },
            {
              "key": "host",
              "type": "string",
              "title": "Host name or IP address",
            },
            {
              "key": "port",
              "type": "integer",
              "title": "Port number",
            },
            {
              "key": "db",
              "type": "string",
              "title": "Database name",
            }
          ],
          "required": ["driver", "host", "port", "db"]
        },
        "credentials": {
          "$schema": "http://json-schema.org/schema#",
          "type": "array",
          "items": [
            {
              "key": "username",
              "type": "string",
              "title": "Username",
            },
            {
              "key": "password",
              "type": "string",
              "title": "Password",
              "format": "password",
            }
          ],
          "required": ["username", "password"]
        }
      }
    ]
  },
  asResource: function (type, name, params, credentials) {
    var OMOPCDMResource = function (name, params, credentials) {
      var resourceUrl = params.driver + "://" + params.host + ":" + params.port + "/" + params.db;
      return {
        name: name,
        url: resourceUrl,
        format: "omop.cdm.db",
        identity: credentials.username,
        secret: credentials.password
      };
    };
    var toResourceFactories = {
      "omop-cdm-db": OMOPCDMResource
    };
    if (toResourceFactories[type]) {
      return toResourceFactories[type](name, params, credentials);
    }
    return undefined;
  }
};
