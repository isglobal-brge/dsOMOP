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
        "title": "OMOP CDM database connection",
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
  // Function to convert parameters into a specific resource type
  asResource: function (type, name, params, credentials) {
    // Function to create an OMOP CDM resource
    var OMOPCDMResource = function (name, params, credentials) {
      // Construct the resource URL from the parameters
      var resourceUrl = params.driver + "://" + params.host + ":" + params.port + "/" + params.db;
      // Return an object representing the resource
      return {
        name: name,
        url: resourceUrl,
        format: "omop.cdm.db",
        identity: credentials.username,
        secret: credentials.password
      };
    };
    // Map of resource types to their corresponding factory functions
    var toResourceFactories = {
      "omop-cdm-db": OMOPCDMResource
    };
    // If the requested type exists in the map, use its factory function to create the resource
    if (toResourceFactories[type]) {
      return toResourceFactories[type](name, params, credentials);
    }
    // If the requested type does not exist in the map, return undefined
    return undefined;
  }
};
