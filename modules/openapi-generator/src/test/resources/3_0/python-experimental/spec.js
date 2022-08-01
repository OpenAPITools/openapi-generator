{
  "openapi" : "3.0.1",
  "info" : {
    "title" : "edu-sharing Repository REST API",
    "description" : "The public restful API of the edu-sharing repository.",
    "version" : "1.1"
  },
  "servers" : [ {
    "url" : "/edu-sharing/rest"
  } ],
  "paths" : {
    "/_about" : {
      "get" : {
        "tags" : [ "ABOUT" ],
        "summary" : "Discover the API.",
        "description" : "Get all services provided by this API.",
        "operationId" : "about",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/About"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/_about/status/{mode}" : {
      "get" : {
        "tags" : [ "ABOUT" ],
        "summary" : "status of repo services",
        "description" : "returns http status 200 when ok",
        "operationId" : "status",
        "parameters" : [ {
          "name" : "mode",
          "in" : "path",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "SEARCH", "SERVICE" ]
          }
        }, {
          "name" : "timeoutSeconds",
          "in" : "query",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/applications/xml" : {
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "register/add an application via xml file",
        "description" : "register the xml file provided.",
        "operationId" : "addApplication",
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "required" : [ "xml" ],
                "type" : "object",
                "properties" : {
                  "xml" : {
                    "type" : "object",
                    "description" : "XML file for app to register"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/applications" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "list applications",
        "description" : "List all registered applications.",
        "operationId" : "getApplications",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "register/add an application",
        "description" : "register the specified application.",
        "operationId" : "addApplication_1",
        "parameters" : [ {
          "name" : "url",
          "in" : "query",
          "description" : "Remote application metadata url",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/toolpermissions/add/{name}" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "add a new toolpermissions",
        "operationId" : "addToolpermission",
        "parameters" : [ {
          "name" : "name",
          "in" : "path",
          "description" : "Name/ID of toolpermission",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Node"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/applyTemplate" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "apply a folder template",
        "description" : "apply a folder template.",
        "operationId" : "applyTemplate",
        "parameters" : [ {
          "name" : "template",
          "in" : "query",
          "description" : "Template Filename",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "group",
          "in" : "query",
          "description" : "Group name (authority name)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "folder",
          "in" : "query",
          "description" : "Folder name",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/jobs/{job}" : {
      "delete" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "cancel a running job",
        "operationId" : "cancelJob",
        "parameters" : [ {
          "name" : "job",
          "in" : "path",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/log" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Change the loglevel for classes at runtime.",
        "description" : "Root appenders are used. Check the appender treshold.",
        "operationId" : "changeLogging",
        "parameters" : [ {
          "name" : "name",
          "in" : "query",
          "description" : "name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "loglevel",
          "in" : "query",
          "description" : "loglevel",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "appender",
          "in" : "query",
          "description" : "appender",
          "schema" : {
            "type" : "string",
            "default" : "File"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/cache/clearCache" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "clear cache",
        "description" : "clear cache",
        "operationId" : "clearCache",
        "parameters" : [ {
          "name" : "bean",
          "in" : "query",
          "description" : "bean",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/deletePersons" : {
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "delete persons",
        "description" : "delete the given persons. Their status must be set to \"todelete\"",
        "operationId" : "deletePerson",
        "parameters" : [ {
          "name" : "username",
          "in" : "query",
          "description" : "names of the users to delete",
          "required" : true,
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "requestBody" : {
          "description" : "options object what and how to delete user contents",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/PersonDeleteOptions"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/PersonReport"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/lucene/export" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Search for custom lucene query and choose specific properties to load",
        "description" : "e.g. @cm\\:name:\"*\"",
        "operationId" : "exportByLucene",
        "parameters" : [ {
          "name" : "query",
          "in" : "query",
          "description" : "query",
          "schema" : {
            "type" : "string",
            "default" : "@cm\\:name:\"*\""
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "properties",
          "in" : "query",
          "description" : "properties to fetch, use parent::<property> to include parent property values",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "store",
          "in" : "query",
          "description" : "store, workspace or archive",
          "schema" : {
            "type" : "string",
            "enum" : [ "Workspace", "Archive" ]
          }
        }, {
          "name" : "authorityScope",
          "in" : "query",
          "description" : "authority scope to search for",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/export/lom" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Export Nodes with LOM Metadata Format",
        "description" : "Export Nodes with LOM Metadata Format.",
        "operationId" : "exportLOM",
        "parameters" : [ {
          "name" : "filterQuery",
          "in" : "query",
          "description" : "filterQuery",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "targetDir",
          "in" : "query",
          "description" : "targetDir",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "subObjectHandler",
          "in" : "query",
          "description" : "subObjectHandler",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/jobs/all" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "get all available jobs",
        "operationId" : "getAllJobs",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/toolpermissions/{authority}" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "get all toolpermissions for an authority",
        "description" : "Returns explicit (rights set for this authority) + effective (resulting rights for this authority) toolpermission",
        "operationId" : "getAllToolpermissions",
        "parameters" : [ {
          "name" : "authority",
          "in" : "path",
          "description" : "Authority to load (user or group)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "set toolpermissions for an authority",
        "description" : "If a toolpermission has status UNDEFINED, it will remove explicit permissions for the authority",
        "operationId" : "setToolpermissions",
        "parameters" : [ {
          "name" : "authority",
          "in" : "path",
          "description" : "Authority to set (user or group)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "string",
                  "enum" : [ "ALLOWED", "DENIED", "UNDEFINED" ]
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/applications/{xml}" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "list any xml properties (like from homeApplication.properties.xml)",
        "description" : "list any xml properties (like from homeApplication.properties.xml)",
        "operationId" : "getApplicationXML",
        "parameters" : [ {
          "name" : "xml",
          "in" : "path",
          "description" : "Properties Filename (*.xml)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "edit any properties xml (like homeApplication.properties.xml)",
        "description" : "if the key exists, it will be overwritten. Otherwise, it will be created. You only need to transfer keys you want to edit",
        "operationId" : "updateApplicationXML",
        "parameters" : [ {
          "name" : "xml",
          "in" : "path",
          "description" : "Properties Filename (*.xml)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/cache/cacheEntries/{id}" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get entries of a cache",
        "description" : "Get entries of a cache.",
        "operationId" : "getCacheEntries",
        "parameters" : [ {
          "name" : "id",
          "in" : "path",
          "description" : "Id/bean name of the cache",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/cache/cacheInfo/{id}" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get information about a cache",
        "description" : "Get information about a cache.",
        "operationId" : "getCacheInfo",
        "parameters" : [ {
          "name" : "id",
          "in" : "path",
          "description" : "Id/bean name of the cache",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CacheInfo"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/catalina" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get last info from catalina out",
        "description" : "Get catalina.out log.",
        "operationId" : "getCatalinaOut",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/clusterInfo" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get information about the Cluster",
        "description" : "Get information the Cluster",
        "operationId" : "getCluster",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CacheCluster"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/clusterInfos" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get information about the Cluster",
        "description" : "Get information the Cluster",
        "operationId" : "getClusters",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CacheCluster"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/repositoryConfig" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "get the repository config object",
        "operationId" : "getConfig",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/RepositoryConfig"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "set/update the repository config object",
        "operationId" : "setConfig",
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/RepositoryConfig"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/configFile" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "get a base system config file (e.g. edu-sharing.conf)",
        "operationId" : "getConfigFile",
        "parameters" : [ {
          "name" : "filename",
          "in" : "query",
          "description" : "filename to fetch",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "update a base system config file (e.g. edu-sharing.conf)",
        "operationId" : "updateConfigFile",
        "parameters" : [ {
          "name" : "filename",
          "in" : "query",
          "description" : "filename to fetch",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/globalGroups" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get global groups",
        "description" : "Get global groups (groups across repositories).",
        "operationId" : "getGlobalGroups",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/jobs" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "get all running jobs",
        "operationId" : "getJobs",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/config/merged" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "description" : "Get the fully merged & parsed (lightbend) backend config",
        "operationId" : "getLightbendConfig",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "object"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/import/oai/classes" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get OAI class names",
        "description" : "Get available importer classes for OAI import.",
        "operationId" : "getOaiClasses",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/propertyToMds" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Get a Mds Valuespace for all values of the given properties",
        "description" : "Get a Mds Valuespace for all values of the given properties.",
        "operationId" : "getPropertyToMds",
        "parameters" : [ {
          "name" : "properties",
          "in" : "query",
          "description" : "one or more properties",
          "required" : true,
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/statistics" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "get statistics",
        "description" : "get statistics.",
        "operationId" : "getStatistics",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AdminStatistics"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/import/collections" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "import collections via a xml file",
        "description" : "xml file must be structured as defined by the xsd standard",
        "operationId" : "importCollections",
        "parameters" : [ {
          "name" : "parent",
          "in" : "query",
          "description" : "Id of the root to initialize the collection structure, or '-root-' to inflate them on the first level",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "required" : [ "xml" ],
                "type" : "object",
                "properties" : {
                  "xml" : {
                    "type" : "object",
                    "description" : "XML file to parse (or zip file containing exactly 1 xml file to parse)"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CollectionsResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/import/excel" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Import excel data",
        "description" : "Import excel data.",
        "operationId" : "importExcel",
        "parameters" : [ {
          "name" : "parent",
          "in" : "query",
          "description" : "parent",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "addToCollection",
          "in" : "query",
          "description" : "addToCollection",
          "required" : true,
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "required" : [ "excel" ],
                "type" : "object",
                "properties" : {
                  "excel" : {
                    "type" : "object",
                    "description" : "Excel file to import"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ExcelResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/import/oai" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Import oai data",
        "description" : "Import oai data.",
        "operationId" : "importOai",
        "parameters" : [ {
          "name" : "baseUrl",
          "in" : "query",
          "description" : "base url",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "set",
          "in" : "query",
          "description" : "set/catalog id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "metadataPrefix",
          "in" : "query",
          "description" : "metadata prefix",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "metadataset",
          "in" : "query",
          "description" : "id metadataset",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "className",
          "in" : "query",
          "description" : "importer job class name (call /classes to obtain a list)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "org.edu_sharing.repository.server.jobs.quartz.ImporterJob"
          }
        }, {
          "name" : "importerClassName",
          "in" : "query",
          "description" : "importer class name (call /classes to obtain a list)",
          "schema" : {
            "type" : "string",
            "default" : "org.edu_sharing.repository.server.importer.OAIPMHLOMImporter"
          }
        }, {
          "name" : "recordHandlerClassName",
          "in" : "query",
          "description" : "RecordHandler class name",
          "schema" : {
            "type" : "string",
            "default" : "org.edu_sharing.repository.server.importer.RecordHandlerLOM"
          }
        }, {
          "name" : "binaryHandlerClassName",
          "in" : "query",
          "description" : "BinaryHandler class name (may be empty for none)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "persistentHandlerClassName",
          "in" : "query",
          "description" : "PersistentHandlerClassName class name (may be empty for none)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "fileUrl",
          "in" : "query",
          "description" : "url to file",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "oaiIds",
          "in" : "query",
          "description" : "OAI Ids to import, can be null than the whole set will be imported",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "forceUpdate",
          "in" : "query",
          "description" : "force Update of all entries",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        }, {
          "name" : "from",
          "in" : "query",
          "description" : "from: datestring yyyy-MM-dd)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "until",
          "in" : "query",
          "description" : "until: datestring yyyy-MM-dd)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "periodInDays",
          "in" : "query",
          "description" : "periodInDays: internal sets from and until. only effective if from/until not set)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Remove deleted imports",
        "description" : "Remove deleted imports.",
        "operationId" : "removeOaiImports",
        "parameters" : [ {
          "name" : "baseUrl",
          "in" : "query",
          "description" : "base url",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "set",
          "in" : "query",
          "description" : "set/catalog id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "metadataPrefix",
          "in" : "query",
          "description" : "metadata prefix",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/import/oai/xml" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Import single xml via oai (for testing)",
        "operationId" : "importOaiXML",
        "parameters" : [ {
          "name" : "recordHandlerClassName",
          "in" : "query",
          "description" : "RecordHandler class name",
          "schema" : {
            "type" : "string",
            "default" : "org.edu_sharing.repository.server.importer.RecordHandlerLOM"
          }
        }, {
          "name" : "binaryHandlerClassName",
          "in" : "query",
          "description" : "BinaryHandler class name (may be empty for none)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "properties" : {
                  "xml" : {
                    "type" : "object"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Node"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/refreshAppInfo" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "refresh app info",
        "description" : "Refresh the application info.",
        "operationId" : "refreshAppInfo",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/import/refreshCache/{folder}" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Refresh cache",
        "description" : "Refresh importer cache.",
        "operationId" : "refreshCache",
        "parameters" : [ {
          "name" : "folder",
          "in" : "path",
          "description" : "refresh cache root folder id",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-userhome-"
          }
        }, {
          "name" : "sticky",
          "in" : "query",
          "description" : "sticky",
          "required" : true,
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/cache/refreshEduGroupCache" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Refresh the Edu Group Cache",
        "description" : "Refresh the Edu Group Cache.",
        "operationId" : "refreshEduGroupCache",
        "parameters" : [ {
          "name" : "keepExisting",
          "in" : "query",
          "description" : "keep existing",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/applications/{id}" : {
      "delete" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "remove an application",
        "description" : "remove the specified application.",
        "operationId" : "removeApplication",
        "parameters" : [ {
          "name" : "id",
          "in" : "path",
          "description" : "Application id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/cache/removeCacheEntry" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "remove cache entry",
        "description" : "remove cache entry",
        "operationId" : "removeCacheEntry",
        "parameters" : [ {
          "name" : "cacheIndex",
          "in" : "query",
          "description" : "cacheIndex",
          "schema" : {
            "type" : "integer",
            "format" : "int32"
          }
        }, {
          "name" : "bean",
          "in" : "query",
          "description" : "bean",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/elastic" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Search for custom elastic DSL query",
        "operationId" : "searchByElasticDSL",
        "parameters" : [ {
          "name" : "dsl",
          "in" : "query",
          "description" : "dsl query (json encoded)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResultElastic"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/lucene" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Search for custom lucene query",
        "description" : "e.g. @cm\\:name:\"*\"",
        "operationId" : "searchByLucene",
        "parameters" : [ {
          "name" : "query",
          "in" : "query",
          "description" : "query",
          "schema" : {
            "type" : "string",
            "default" : "@cm\\:name:\"*\""
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        }, {
          "name" : "store",
          "in" : "query",
          "description" : "store, workspace or archive",
          "schema" : {
            "type" : "string",
            "enum" : [ "Workspace", "Archive" ]
          }
        }, {
          "name" : "authorityScope",
          "in" : "query",
          "description" : "authority scope to search for",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/serverUpdate/list" : {
      "get" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "list available update tasks",
        "description" : "list available update tasks",
        "operationId" : "serverUpdateList",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/serverUpdate/run/{id}" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Run an update tasks",
        "description" : "Run a specific update task (test or full update).",
        "operationId" : "serverUpdateList_1",
        "parameters" : [ {
          "name" : "id",
          "in" : "path",
          "description" : "Id of the update task",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "execute",
          "in" : "query",
          "description" : "Actually execute (if false, just runs in test mode)",
          "required" : true,
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/job/{jobClass}" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Start a Job.",
        "description" : "Start a Job.",
        "operationId" : "startJob",
        "parameters" : [ {
          "name" : "jobClass",
          "in" : "path",
          "description" : "jobClass",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "params",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "object"
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/job/{jobClass}/sync" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Start a Job.",
        "description" : "Start a Job. Wait for the result synchronously",
        "operationId" : "startJobSync",
        "parameters" : [ {
          "name" : "jobClass",
          "in" : "path",
          "description" : "jobClass",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "params",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "object"
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "object"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/authenticate/{authorityName}" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "switch the session to a known authority name",
        "operationId" : "switchAuthority",
        "parameters" : [ {
          "name" : "authorityName",
          "in" : "path",
          "description" : "the authority to use (must be a person)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/mail/{receiver}/{template}" : {
      "post" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Test a mail template",
        "description" : "Sends the given template as a test to the given receiver.",
        "operationId" : "testMail",
        "parameters" : [ {
          "name" : "receiver",
          "in" : "path",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "template",
          "in" : "path",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/admin/v1/upload/temp/{name}" : {
      "put" : {
        "tags" : [ "ADMIN v1" ],
        "summary" : "Upload a file",
        "description" : "Upload a file to tomcat temp directory, to use it on the server (e.g. an update)",
        "operationId" : "uploadTemp",
        "parameters" : [ {
          "name" : "name",
          "in" : "path",
          "description" : "filename",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "required" : [ "file" ],
                "type" : "object",
                "properties" : {
                  "file" : {
                    "type" : "object",
                    "description" : "file to upload"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/UploadResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/archive/v1/purge/{repository}" : {
      "delete" : {
        "tags" : [ "ARCHIVE v1" ],
        "summary" : "Searches for archive nodes.",
        "description" : "Searches for archive nodes.",
        "operationId" : "purge",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "archivedNodeIds",
          "in" : "query",
          "description" : "archived node",
          "required" : true,
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/archive/v1/restore/{repository}" : {
      "post" : {
        "tags" : [ "ARCHIVE v1" ],
        "summary" : "restore archived nodes.",
        "description" : "restores archived nodes. restoreStatus can have the following values: FALLBACK_PARENT_NOT_EXISTS, FALLBACK_PARENT_NO_PERMISSION, DUPLICATENAME, FINE",
        "operationId" : "restore",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "archivedNodeIds",
          "in" : "query",
          "description" : "archived nodes",
          "required" : true,
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "target",
          "in" : "query",
          "description" : "to target",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/RestoreResults"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/archive/v1/search/{repository}/{pattern}" : {
      "get" : {
        "tags" : [ "ARCHIVE v1" ],
        "summary" : "Searches for archive nodes.",
        "description" : "Searches for archive nodes.",
        "operationId" : "searchArchive",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "pattern",
          "in" : "path",
          "description" : "search pattern",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/archive/v1/search/{repository}/{pattern}/{person}" : {
      "get" : {
        "tags" : [ "ARCHIVE v1" ],
        "summary" : "Searches for archive nodes.",
        "description" : "Searches for archive nodes.",
        "operationId" : "searchArchivePerson",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "pattern",
          "in" : "path",
          "description" : "search pattern",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "person",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/bulk/v1/find" : {
      "post" : {
        "tags" : [ "BULK v1" ],
        "summary" : "gets a given node",
        "description" : "Get a given node based on the posted, multiple criteria. Make sure that they'll provide an unique result",
        "operationId" : "find",
        "requestBody" : {
          "description" : "properties that must match (with \"AND\" concatenated)",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/bulk/v1/sync/{group}" : {
      "put" : {
        "tags" : [ "BULK v1" ],
        "summary" : "Create or update a given node",
        "description" : "Depending on the given \"match\" properties either a new node will be created or the existing one will be updated",
        "operationId" : "sync",
        "parameters" : [ {
          "name" : "group",
          "in" : "path",
          "description" : "The group to which this node belongs to. Used for internal structuring. Please use simple names only",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "match",
          "in" : "query",
          "description" : "The properties that must match to identify if this node exists. Multiple properties will be and combined and compared",
          "required" : true,
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "groupBy",
          "in" : "query",
          "description" : "The properties on which the imported nodes should be grouped (for each value, a folder with the corresponding data is created)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "type",
          "in" : "query",
          "description" : "type of node. If the node already exists, this will not change the type afterwards",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "aspects",
          "in" : "query",
          "description" : "aspects of node",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "resetVersion",
          "in" : "query",
          "description" : "reset all versions (like a complete reimport), all data inside edu-sharing will be lost",
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "requestBody" : {
          "description" : "properties, they'll not get filtered via mds, so be careful what you add here",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/clientUtils/v1/getWebsiteInformation" : {
      "get" : {
        "tags" : [ "CLIENTUTILS v1" ],
        "summary" : "Read generic information about a webpage",
        "operationId" : "getWebsiteInformation",
        "parameters" : [ {
          "name" : "url",
          "in" : "query",
          "description" : "full url with http or https",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/WebsiteInformation"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/feedback" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Get feedback of collection.",
        "description" : "Requires permission \"???\" on the specific permission",
        "operationId" : "getFeedbackOfCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Post feedback to collection.",
        "description" : "Requires permission \"Feedback\" on the specific collection",
        "operationId" : "addFeedbackToCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/references/{node}" : {
      "put" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Add a node to a collection.",
        "description" : "Add a node to a collection.",
        "operationId" : "addToCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "sourceRepo",
          "in" : "query",
          "description" : "ID of source repository",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "allowDuplicate",
          "in" : "query",
          "description" : "Allow that a node that already is inside the collection can be added again",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        }, {
          "name" : "asProposal",
          "in" : "query",
          "description" : "Mark this node only as a proposal (not really adding but just marking it). This can also be used for collections where you don't have permissions",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Delete a node from a collection.",
        "description" : "Delete a node from a collection.",
        "operationId" : "deleteFromCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/icon" : {
      "post" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Writes Preview Image of a collection.",
        "description" : "Writes Preview Image of a collection.",
        "operationId" : "changeIconOfCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "mimetype",
          "in" : "query",
          "description" : "MIME-Type",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "type" : "object",
                "properties" : {
                  "file" : {
                    "type" : "object"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CollectionEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Deletes Preview Image of a collection.",
        "description" : "Deletes Preview Image of a collection.",
        "operationId" : "removeIconOfCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/children" : {
      "post" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Create a new collection.",
        "description" : "Create a new collection.",
        "operationId" : "createCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of parent collection (or \"-root-\" for level0 collections)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "collection",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Node"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CollectionEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}" : {
      "put" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Update a collection.",
        "description" : "Update a collection.",
        "operationId" : "updateCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "collection node",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Node"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Delete a collection.",
        "description" : "Delete a collection.",
        "operationId" : "deleteCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collectionId}" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Get a collection.",
        "description" : "Get a collection.",
        "operationId" : "getCollection",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collectionId",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CollectionEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/children/proposals/collections" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Get all collections containing proposals with a given state (via search index)",
        "operationId" : "getCollectionsContainingProposals",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "status",
          "in" : "query",
          "description" : "status of the proposals to search for",
          "schema" : {
            "type" : "string",
            "enum" : [ "PENDING", "ACCEPTED", "DECLINED" ],
            "default" : "PENDING"
          }
        }, {
          "name" : "fetchCounts",
          "in" : "query",
          "description" : "fetch counts of collections (materials and subcollections). This parameter will decrease performance so only enable if if you need this data",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 50
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CollectionProposalEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/children/proposals" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Get proposed objects for collection (requires edit permissions on collection).",
        "operationId" : "getCollectionsProposals",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of parent collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "status",
          "in" : "query",
          "description" : "Only show elements with given status",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "PENDING", "ACCEPTED", "DECLINED" ]
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AbstractEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/children/references" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Get references objects for collection.",
        "operationId" : "getCollectionsReferences",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of parent collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 500
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ReferenceEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/children/collections" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Get child collections for collection (or root).",
        "operationId" : "getCollectionsSubcollections",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of parent collection (or \"-root-\" for level0 collections)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "scope",
          "in" : "query",
          "description" : "scope (only relevant if parent == -root-)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "EDU_ALL", "EDU_GROUPS", "TYPE_EDITORIAL", "TYPE_MEDIA_CENTER", "MY", "RECENT" ],
            "default" : "MY"
          }
        }, {
          "name" : "fetchCounts",
          "in" : "query",
          "description" : "fetch counts of collections (materials and subcollections). This parameter will decrease performance so only enable if if you need this data",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 500
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ReferenceEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/search" : {
      "get" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Search collections.",
        "description" : "Search collections.",
        "operationId" : "searchCollections",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "query",
          "in" : "query",
          "description" : "query string",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 500
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/CollectionEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/{collection}/order" : {
      "post" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Set order of nodes in a collection. In order to work as expected, provide a list of all nodes in this collection",
        "description" : "Current order will be overriden. Requires full permissions for the parent collection",
        "operationId" : "setCollectionOrder",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "collection",
          "in" : "path",
          "description" : "ID of collection",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "List of nodes in the order to be saved. If empty, custom order of the collection will be disabled",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "array",
                "items" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/collection/v1/collections/{repository}/pinning" : {
      "post" : {
        "tags" : [ "COLLECTION v1" ],
        "summary" : "Set pinned collections.",
        "description" : "Remove all currently pinned collections and set them in the order send. Requires TOOLPERMISSION_COLLECTION_PINNING",
        "operationId" : "setPinnedCollections",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "requestBody" : {
          "description" : "List of collections that should be pinned",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "array",
                "items" : {
                  "type" : "string",
                  "default" : "-home-"
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/comment/v1/comments/{repository}/{node}" : {
      "get" : {
        "tags" : [ "COMMENT v1" ],
        "summary" : "list comments",
        "description" : "List all comments",
        "operationId" : "getComments",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Comments"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "COMMENT v1" ],
        "summary" : "create a new comment",
        "description" : "Adds a comment to the given node",
        "operationId" : "addComment",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "commentReference",
          "in" : "query",
          "description" : "In reply to an other comment, can be null",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "Text content of comment",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/comment/v1/comments/{repository}/{comment}" : {
      "post" : {
        "tags" : [ "COMMENT v1" ],
        "summary" : "edit a comment",
        "description" : "Edit the comment with the given id",
        "operationId" : "editComment",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "comment",
          "in" : "path",
          "description" : "id of the comment to edit",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "Text content of comment",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "COMMENT v1" ],
        "summary" : "delete a comment",
        "description" : "Delete the comment with the given id",
        "operationId" : "deleteComment",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "comment",
          "in" : "path",
          "description" : "id of the comment to delete",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/config/v1/values" : {
      "get" : {
        "tags" : [ "CONFIG v1" ],
        "summary" : "get repository config values",
        "description" : "Current is the actual (context-based) active config. Global is the default global config if no context is active (may be identical to the current)",
        "operationId" : "getConfig_1",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Config"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/config/v1/dynamic/{key}" : {
      "get" : {
        "tags" : [ "CONFIG v1" ],
        "summary" : "Get a config entry (appropriate rights for the entry are required)",
        "operationId" : "getDynamicValue",
        "parameters" : [ {
          "name" : "key",
          "in" : "path",
          "description" : "Key of the config value that should be fetched",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/DynamicConfig"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "CONFIG v1" ],
        "summary" : "Set a config entry (admin rights required)",
        "description" : "the body must be a json encapsulated string",
        "operationId" : "setDynamicValue",
        "parameters" : [ {
          "name" : "key",
          "in" : "path",
          "description" : "Key of the config value that should be fetched",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "public",
          "in" : "query",
          "description" : "Is everyone allowed to read the value",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "requestBody" : {
          "description" : "Must be a json-encapsulated string",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/DynamicConfig"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/config/v1/language" : {
      "get" : {
        "tags" : [ "CONFIG v1" ],
        "summary" : "get override strings for the current language",
        "description" : "Language strings",
        "operationId" : "getLanguage",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Language"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/config/v1/language/defaults" : {
      "get" : {
        "tags" : [ "CONFIG v1" ],
        "summary" : "get all inital language strings for angular",
        "operationId" : "getLanguageDefaults",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/config/v1/variables" : {
      "get" : {
        "tags" : [ "CONFIG v1" ],
        "summary" : "get global config variables",
        "description" : "global config variables",
        "operationId" : "getVariables",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Variables"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/connector/v1/connectors/{repository}/list" : {
      "get" : {
        "tags" : [ "CONNECTOR v1" ],
        "summary" : "List all available connectors",
        "operationId" : "listConnectors",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ConnectorList"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/members/{member}" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Add member to the group.",
        "description" : "Add member to the group. (admin rights are required.)",
        "operationId" : "addMembership",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "member",
          "in" : "path",
          "description" : "authorityName of member",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Delete member from the group.",
        "description" : "Delete member from the group. (admin rights are required.)",
        "operationId" : "deleteMembership",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "member",
          "in" : "path",
          "description" : "authorityName of member",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/nodeList/{list}/{node}" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Add a node to node a list of a user",
        "description" : "For guest users, the list will be temporary stored in the current session",
        "operationId" : "addNodeList",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        }, {
          "name" : "list",
          "in" : "path",
          "description" : "list name. If this list does not exist, it will be created",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Deelete a node of a node list of a user",
        "description" : "For guest users, the list will be temporary stored in the current session",
        "operationId" : "removeNodeList",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        }, {
          "name" : "list",
          "in" : "path",
          "description" : "list name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/profile" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Set profile of the group.",
        "description" : "Set profile of the group. (admin rights are required.)",
        "operationId" : "changeGroupProfile",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/GroupProfile"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/avatar" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Set avatar of the user.",
        "description" : "Set avatar of the user. (To set foreign avatars, admin rights are required.)",
        "operationId" : "changeUserAvatar",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "required" : [ "avatar" ],
                "type" : "object",
                "properties" : {
                  "avatar" : {
                    "type" : "object",
                    "description" : "avatar image"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Remove avatar of the user.",
        "description" : "Remove avatar of the user. (To Remove foreign avatars, admin rights are required.)",
        "operationId" : "removeUserAvatar",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/credential" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Change/Set password of the user.",
        "description" : "Change/Set password of the user. (To change foreign passwords or set passwords, admin rights are required.)",
        "operationId" : "changeUserPassword",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "requestBody" : {
          "description" : "credential",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/UserCredential"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/profile" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Set profile of the user.",
        "description" : "Set profile of the user. (To set foreign profiles, admin rights are required.)",
        "operationId" : "changeUserProfile",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "requestBody" : {
          "description" : "properties",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/UserProfileEdit"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/signup/list/{user}" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "put the pending user into the group",
        "description" : "Requires admin rights or org administrator on this group",
        "operationId" : "confirmSignup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "ID of group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "user",
          "in" : "path",
          "description" : "ID of user",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "IAM v1" ],
        "summary" : "reject the pending user",
        "description" : "Requires admin rights or org administrator on this group",
        "operationId" : "rejectSignup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "ID of group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "user",
          "in" : "path",
          "description" : "ID of user",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get the group.",
        "description" : "Get the group. (To get foreign profiles, admin rights are required.)",
        "operationId" : "getGroup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/GroupEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Create a new group.",
        "description" : "Create a new group. (admin rights are required.)",
        "operationId" : "createGroup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "parent",
          "in" : "query",
          "description" : "parent (will be added to this parent, also for name hashing), may be null",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/GroupProfile"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Group"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Delete the group.",
        "description" : "Delete the group. (admin rights are required.)",
        "operationId" : "deleteGroup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get the user.",
        "description" : "Get the user. (Not all information are feteched for foreign profiles if current user is not an admin)",
        "operationId" : "getUser",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/UserEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Create a new user.",
        "description" : "Create a new user. (admin rights are required.)",
        "operationId" : "createUser",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "password",
          "in" : "query",
          "description" : "Password, leave empty if you don't want to set any",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "profile",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/UserProfileEdit"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/User"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Delete the user.",
        "description" : "Delete the user. (admin rights are required.)",
        "operationId" : "deleteUser",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "force",
          "in" : "query",
          "description" : "force the deletion (if false then only persons which are previously marked for deletion are getting deleted)",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/members" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get all members of the group.",
        "description" : "Get all members of the group. (admin rights are required.)",
        "operationId" : "getMembership",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "authority name (begins with GROUP_)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "pattern",
          "in" : "query",
          "description" : "pattern",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "authorityType",
          "in" : "query",
          "description" : "authorityType either GROUP or USER, empty to show all",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AuthorityEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/nodeList/{list}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get a specific node list for a user",
        "description" : "For guest users, the list will be temporary stored in the current session",
        "operationId" : "getNodeList",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        }, {
          "name" : "list",
          "in" : "path",
          "description" : "list name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/preferences" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get preferences stored for user",
        "description" : "Will fail for guest",
        "operationId" : "getPreferences",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Preferences"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Set preferences for user",
        "description" : "Will fail for guest",
        "operationId" : "setPreferences",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "requestBody" : {
          "description" : "preferences (json string)",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string",
                "default" : "-me-"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/profileSettings" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get profileSettings configuration",
        "description" : "Will fail for guest",
        "operationId" : "getProfileSettings",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ProfileSettings"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Set profileSettings Configuration",
        "description" : "Will fail for guest",
        "operationId" : "setProfileSettings",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "requestBody" : {
          "description" : "ProfileSetting Object",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/ProfileSettings"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/authorities/{repository}/recent" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get recently invited authorities.",
        "description" : "Get the authorities the current user has recently invited.",
        "operationId" : "getRecentlyInvited",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AuthorityEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/type/{type}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get a subgroup by the specified type",
        "description" : "Get a subgroup by the specified type",
        "operationId" : "getSubgroupByType",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "authority name of the parent/primary group (begins with GROUP_)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "type",
          "in" : "path",
          "description" : "authorityType either GROUP or USER, empty to show all",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AuthorityEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/memberships" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get all groups the given user is member of.",
        "operationId" : "getUserGroups",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "authority name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "pattern",
          "in" : "query",
          "description" : "pattern",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/GroupEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/stats" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Get the user stats.",
        "description" : "Get the user stats (e.g. publicly created material count)",
        "operationId" : "getUserStats",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username (or \"-me-\" for current user)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-me-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/UserStats"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/authorities/{repository}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Search authorities.",
        "description" : "Search authorities.",
        "operationId" : "searchAuthorities",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "pattern",
          "in" : "query",
          "description" : "pattern",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "global",
          "in" : "query",
          "description" : "global search context, defaults to true, otherwise just searches for users within the organizations",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        }, {
          "name" : "groupType",
          "in" : "query",
          "description" : "find a specific groupType (does nothing for persons)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "signupMethod",
          "in" : "query",
          "description" : "find a specific signupMethod for groups (or asterisk for all including one) (does nothing for persons)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AuthorityEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Search groups.",
        "description" : "Search groups. (admin rights are required.)",
        "operationId" : "searchGroups",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "pattern",
          "in" : "query",
          "description" : "pattern",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "groupType",
          "in" : "query",
          "description" : "find a specific groupType",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "signupMethod",
          "in" : "query",
          "description" : "find a specific signupMethod for groups (or asterisk for all including one)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "global",
          "in" : "query",
          "description" : "global search context, defaults to true, otherwise just searches for groups within the organizations",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/GroupEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "Search users.",
        "description" : "Search users. (admin rights are required.)",
        "operationId" : "searchUser",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "pattern",
          "in" : "query",
          "description" : "pattern",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "global",
          "in" : "query",
          "description" : "global search context, defaults to true, otherwise just searches for users within the organizations",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        }, {
          "name" : "status",
          "in" : "query",
          "description" : "the user status (e.g. active), if not set, all users are returned",
          "schema" : {
            "type" : "string",
            "enum" : [ "active", "blocked", "todelete" ]
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/UserEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/signup" : {
      "post" : {
        "tags" : [ "IAM v1" ],
        "summary" : "let the current user signup to the given group",
        "operationId" : "signupGroup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "ID of group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "password",
          "in" : "query",
          "description" : "Password for signup (only required if signupMethod == password)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string",
                  "enum" : [ "InvalidPassword", "AlreadyInList", "AlreadyMember", "Ok" ]
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/signup/config" : {
      "post" : {
        "tags" : [ "IAM v1" ],
        "summary" : " requires admin rights",
        "description" : "set group signup options",
        "operationId" : "signupGroupDetails",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "ID of group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "Details to edit",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/GroupSignupDetails"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/groups/{repository}/{group}/signup/list" : {
      "get" : {
        "tags" : [ "IAM v1" ],
        "summary" : "list pending users that want to join this group",
        "description" : "Requires admin rights or org administrator on this group",
        "operationId" : "signupGroupList",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "ID of group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/iam/v1/people/{repository}/{person}/status/{status}" : {
      "put" : {
        "tags" : [ "IAM v1" ],
        "summary" : "update the user status.",
        "description" : "update the user status. (admin rights are required.)",
        "operationId" : "updateUserStatus",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "person",
          "in" : "path",
          "description" : "username",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "status",
          "in" : "path",
          "description" : "the new status to set",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "active", "blocked", "todelete" ]
          }
        }, {
          "name" : "notify",
          "in" : "query",
          "description" : "notify the user via mail",
          "required" : true,
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/knowledge/v1/analyze/jobs/{job}" : {
      "get" : {
        "tags" : [ "KNOWLEDGE v1" ],
        "summary" : "Get analyzing job status.",
        "description" : "Get analyzing job status.",
        "operationId" : "getAnalyzingJobStatus",
        "parameters" : [ {
          "name" : "job",
          "in" : "path",
          "description" : "ID of job ticket",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/JobEntry"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : { }
            }
          },
          "403" : {
            "description" : "The current user has insufficient rights to access the ticket.",
            "content" : {
              "application/json" : { }
            }
          },
          "404" : {
            "description" : "Job not found.",
            "content" : {
              "application/json" : { }
            }
          }
        }
      }
    },
    "/knowledge/v1/analyze/jobs" : {
      "post" : {
        "tags" : [ "KNOWLEDGE v1" ],
        "summary" : "Run analyzing job.",
        "description" : "Run analyzing job for a node.",
        "operationId" : "runAnalyzingJob",
        "parameters" : [ {
          "name" : "repository",
          "in" : "query",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "query",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "202" : {
            "description" : "Accepted.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/JobEntry"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : { }
            }
          },
          "403" : {
            "description" : "The current user has insufficient rights to read the node or to perform an analyzing job.",
            "content" : {
              "application/json" : { }
            }
          },
          "404" : {
            "description" : "Repository or node not found.",
            "content" : {
              "application/json" : { }
            }
          }
        }
      }
    },
    "/authentication/v1/appauth/{userId}" : {
      "post" : {
        "tags" : [ "AUTHENTICATION v1" ],
        "summary" : "authenticate user of an registered application.",
        "description" : "headers must be set: X-Edu-App-Id, X-Edu-App-Sig, X-Edu-App-Signed, X-Edu-App-Ts",
        "operationId" : "authenticate",
        "parameters" : [ {
          "name" : "userId",
          "in" : "path",
          "description" : "User Id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "User Profile",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/UserProfileAppAuth"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/AuthenticationToken"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/authentication/v1/hasAccessToScope" : {
      "get" : {
        "tags" : [ "AUTHENTICATION v1" ],
        "summary" : "Returns true if the current user has access to the given scope",
        "operationId" : "hasAccessToScope",
        "parameters" : [ {
          "name" : "scope",
          "in" : "query",
          "description" : "scope",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : { }
            }
          }
        }
      }
    },
    "/authentication/v1/validateSession" : {
      "get" : {
        "tags" : [ "AUTHENTICATION v1" ],
        "summary" : "Validates the Basic Auth Credentials and check if the session is a logged in user",
        "description" : "Use the Basic auth header field to transfer the credentials",
        "operationId" : "login",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Login"
                }
              }
            }
          }
        }
      }
    },
    "/authentication/v1/loginToScope" : {
      "post" : {
        "tags" : [ "AUTHENTICATION v1" ],
        "summary" : "Validates the Basic Auth Credentials and check if the session is a logged in user",
        "description" : "Use the Basic auth header field to transfer the credentials",
        "operationId" : "loginToScope",
        "requestBody" : {
          "description" : "credentials, example: test,test",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/LoginCredentials"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Login"
                }
              }
            }
          }
        }
      }
    },
    "/authentication/v1/destroySession" : {
      "get" : {
        "tags" : [ "AUTHENTICATION v1" ],
        "summary" : "Destroys the current session and logout the user",
        "operationId" : "logout",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : { }
            }
          }
        }
      }
    },
    "/lti/v13/generateDeepLinkingResponse" : {
      "get" : {
        "tags" : [ "LTI v13" ],
        "summary" : "generate DeepLinkingResponse",
        "operationId" : "generateDeepLinkingResponse",
        "parameters" : [ {
          "name" : "nodeIds",
          "in" : "query",
          "description" : "selected node id's",
          "required" : true,
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeLTIDeepLink"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/jwks" : {
      "get" : {
        "tags" : [ "LTI v13" ],
        "summary" : "LTI - returns repository JSON Web Key Sets",
        "operationId" : "jwksUri",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/RegistrationUrl"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/oidc/login_initiations" : {
      "post" : {
        "tags" : [ "LTI v13" ],
        "summary" : "lti authentication process preparation.",
        "description" : "preflight phase. prepares lti authentication process. checks it issuer is valid",
        "operationId" : "loginInitiations",
        "requestBody" : {
          "content" : {
            "application/x-www-form-urlencoded" : {
              "schema" : {
                "required" : [ "iss", "target_link_uri" ],
                "type" : "object",
                "properties" : {
                  "iss" : {
                    "type" : "string",
                    "description" : "Issuer of the request, will be validated"
                  },
                  "target_link_uri" : {
                    "type" : "string",
                    "description" : "target url of platform at the end of the flow"
                  },
                  "client_id" : {
                    "type" : "string",
                    "description" : "Id of the issuer"
                  },
                  "login_hint" : {
                    "type" : "string",
                    "description" : "context information of the platform"
                  },
                  "lti_message_hint" : {
                    "type" : "string",
                    "description" : "additional context information of the platform"
                  },
                  "lti_deployment_id" : {
                    "type" : "string",
                    "description" : "A can have multiple deployments in a platform"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/lti13" : {
      "post" : {
        "tags" : [ "LTI v13" ],
        "summary" : "lti tool redirect.",
        "description" : "lti tool redirect",
        "operationId" : "lti",
        "requestBody" : {
          "content" : {
            "application/x-www-form-urlencoded" : {
              "schema" : {
                "required" : [ "id_token", "state" ],
                "type" : "object",
                "properties" : {
                  "id_token" : {
                    "type" : "string",
                    "description" : "Issuer of the request, will be validated"
                  },
                  "state" : {
                    "type" : "string",
                    "description" : "Issuer of the request, will be validated"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/registration/dynamic/{token}" : {
      "get" : {
        "tags" : [ "LTI v13" ],
        "summary" : "LTI Dynamic Registration - Initiate registration",
        "operationId" : "ltiRegistrationDynamic",
        "parameters" : [ {
          "name" : "openid_configuration",
          "in" : "query",
          "description" : "the endpoint to the open id configuration to be used for this registration",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "registration_token",
          "in" : "query",
          "description" : "the registration access token. If present, it must be used as the access token by the tool when making the registration request to the registration endpoint exposed in the openid configuration.",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "token",
          "in" : "path",
          "description" : "one time usage token which is autogenerated with the url in edu-sharing admin gui.",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/registration/url" : {
      "get" : {
        "tags" : [ "LTI v13" ],
        "summary" : "LTI Dynamic Registration - generates url for platform",
        "operationId" : "ltiRegistrationUrl",
        "parameters" : [ {
          "name" : "generate",
          "in" : "query",
          "description" : "if to add a ne url to the list",
          "required" : true,
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/DynamicRegistrationTokens"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/lti13/{nodeId}" : {
      "post" : {
        "tags" : [ "LTI v13" ],
        "summary" : "lti tool resource link target.",
        "description" : "used by some platforms for direct (without oidc login_init) launch requests",
        "operationId" : "ltiTarget",
        "parameters" : [ {
          "name" : "nodeId",
          "in" : "path",
          "description" : "edu-sharing node id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/x-www-form-urlencoded" : {
              "schema" : {
                "required" : [ "id_token", "state" ],
                "type" : "object",
                "properties" : {
                  "id_token" : {
                    "type" : "string",
                    "description" : "Issuer of the request, will be validated"
                  },
                  "state" : {
                    "type" : "string",
                    "description" : "Issuer of the request, will be validated"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "text/html" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/registration/{type}" : {
      "post" : {
        "tags" : [ "LTI v13" ],
        "summary" : "register LTI platform",
        "operationId" : "registerByType",
        "parameters" : [ {
          "name" : "type",
          "in" : "path",
          "description" : "lti platform typ i.e. moodle",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "moodle" ]
          }
        }, {
          "name" : "baseUrl",
          "in" : "query",
          "description" : "base url i.e. http://localhost/moodle used as platformId",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "client_id",
          "in" : "query",
          "description" : "client id",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "deployment_id",
          "in" : "query",
          "description" : "deployment id",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/registration/static" : {
      "post" : {
        "tags" : [ "LTI v13" ],
        "summary" : "register LTI platform",
        "operationId" : "registerTest",
        "parameters" : [ {
          "name" : "platformId",
          "in" : "query",
          "description" : "the issuer",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "client_id",
          "in" : "query",
          "description" : "client id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "deployment_id",
          "in" : "query",
          "description" : "deployment id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "authentication_request_url",
          "in" : "query",
          "description" : "oidc endpoint, authentication request url",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "keyset_url",
          "in" : "query",
          "description" : "jwks endpoint, keyset url",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "key_id",
          "in" : "query",
          "description" : "jwks key id",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "auth_token_url",
          "in" : "query",
          "description" : "auth token url",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/lti/v13/registration/url/{token}" : {
      "delete" : {
        "tags" : [ "LTI v13" ],
        "summary" : "LTI Dynamic Regitration - delete url",
        "operationId" : "removeLtiRegistrationUrl",
        "parameters" : [ {
          "name" : "token",
          "in" : "path",
          "description" : "the token of the link you have to remove",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/DynamicRegistrationTokens"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          }
        }
      }
    },
    "/mds/v1/metadatasets/{repository}/{metadataset}" : {
      "get" : {
        "tags" : [ "MDS v1" ],
        "summary" : "Get metadata set new.",
        "description" : "Get metadata set new.",
        "operationId" : "getMetadataSet",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Mds"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mds/v1/metadatasets/{repository}" : {
      "get" : {
        "tags" : [ "MDS v1" ],
        "summary" : "Get metadata sets V2 of repository.",
        "description" : "Get metadata sets V2 of repository.",
        "operationId" : "getMetadataSets",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/MdsEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mds/v1/metadatasets/{repository}/{metadataset}/values" : {
      "post" : {
        "tags" : [ "MDS v1" ],
        "summary" : "Get values.",
        "description" : "Get values.",
        "operationId" : "getValues",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        } ],
        "requestBody" : {
          "description" : "suggestionParam",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/SuggestionParam"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Mds"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mds/v1/metadatasets/{repository}/{metadataset}/values_for_keys" : {
      "post" : {
        "tags" : [ "MDS v1" ],
        "summary" : "Get values for keys.",
        "description" : "Get values for keys.",
        "operationId" : "getValues4Keys",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        }, {
          "name" : "query",
          "in" : "query",
          "description" : "query",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "property",
          "in" : "query",
          "description" : "property",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "keys",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "array",
                "items" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Suggestions"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mds/v1/metadatasets/{repository}/{metadataset}/values/{widget}/suggest" : {
      "post" : {
        "tags" : [ "MDS v1" ],
        "summary" : "Suggest a value.",
        "description" : "Suggest a new value for a given metadataset and widget. The suggestion will be forwarded to the corresponding person in the metadataset file",
        "operationId" : "suggestValue",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        }, {
          "name" : "widget",
          "in" : "path",
          "description" : "widget id, e.g. cm:name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "caption",
          "in" : "query",
          "description" : "caption of the new entry (id will be auto-generated)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "parent",
          "in" : "query",
          "description" : "parent id of the new entry (might be null)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "nodeId",
          "in" : "query",
          "description" : "One or more nodes this suggestion relates to (optional, only for extended mail data)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/MdsValue"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/mediacenter/{repository}/{mediacenter}/manages/{group}" : {
      "put" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "add a group that is managed by the given mediacenter",
        "description" : "although not restricted, it is recommended that the group is an edu-sharing organization (admin rights are required)",
        "operationId" : "addMediacenterGroup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "authorityName of the mediacenter that should manage the group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "authorityName of the group that should be managed by that mediacenter",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "delete a group that is managed by the given mediacenter",
        "description" : "admin rights are required.",
        "operationId" : "removeMediacenterGroup",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "authorityName of the mediacenter that should manage the group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "group",
          "in" : "path",
          "description" : "authorityName of the group that should not longer be managed by that mediacenter",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/mediacenter/{repository}/{mediacenter}" : {
      "put" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "edit a mediacenter in repository.",
        "operationId" : "editMediacenter",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "mediacenter name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Profile"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Mediacenter"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "create new mediacenter in repository.",
        "description" : "admin rights are required.",
        "operationId" : "createMediacenter",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "mediacenter name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Profile"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Mediacenter"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "delete a mediacenter group and it's admin group and proxy group",
        "description" : "admin rights are required.",
        "operationId" : "deleteMediacenter",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "authorityName of the mediacenter that should manage the group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/mediacenter/{repository}/{mediacenter}/manages" : {
      "get" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "get groups that are managed by the given mediacenter",
        "operationId" : "getMediacenterGroups",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "authorityName of the mediacenter that should manage the group",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/mediacenter/{repository}/{mediacenter}/licenses" : {
      "post" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "get nodes that are licensed by the given mediacenter",
        "operationId" : "getMediacenterLicensedNodes",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        }, {
          "name" : "mediacenter",
          "in" : "path",
          "description" : "authorityName of the mediacenter that licenses nodes",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "searchword",
          "in" : "query",
          "description" : "searchword of licensed nodes",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "search parameters",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/SearchParameters"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/mediacenter/{repository}" : {
      "get" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "get mediacenters in the repository.",
        "description" : "Only shows the one available/managing the current user (only admin can access all)",
        "operationId" : "getMediacenters",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/import/mc_org" : {
      "post" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "Import Mediacenter Organisation Connection",
        "description" : "Import Mediacenter Organisation Connection.",
        "operationId" : "importMcOrgConnections",
        "parameters" : [ {
          "name" : "removeSchoolsFromMC",
          "in" : "query",
          "description" : "removeSchoolsFromMC",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "required" : [ "mcOrgs" ],
                "type" : "object",
                "properties" : {
                  "mcOrgs" : {
                    "type" : "object",
                    "description" : "Mediacenter Organisation Connection csv to import"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/McOrgConnectResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/import/mediacenters" : {
      "post" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "Import mediacenters",
        "description" : "Import mediacenters.",
        "operationId" : "importMediacenters",
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "required" : [ "mediacenters" ],
                "type" : "object",
                "properties" : {
                  "mediacenters" : {
                    "type" : "object",
                    "description" : "Mediacenters csv to import"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/MediacentersImportResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/mediacenter/v1/import/organisations" : {
      "post" : {
        "tags" : [ "MEDIACENTER v1" ],
        "summary" : "Import Organisations",
        "description" : "Import Organisations.",
        "operationId" : "importOrganisations",
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "required" : [ "organisations" ],
                "type" : "object",
                "properties" : {
                  "organisations" : {
                    "type" : "object",
                    "description" : "Organisations csv to import"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/OrganisationsImportResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/network/v1/services" : {
      "get" : {
        "tags" : [ "NETWORK v1" ],
        "summary" : "Get services.",
        "description" : "Get registerted services.",
        "operationId" : "getServices",
        "parameters" : [ {
          "name" : "query",
          "in" : "query",
          "description" : "search or filter for services",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "NETWORK v1" ],
        "summary" : "Register service.",
        "description" : "Register a new service.",
        "operationId" : "addService",
        "requestBody" : {
          "description" : "Service data object",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Service"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/StoredService"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/network/v1/repositories" : {
      "get" : {
        "tags" : [ "NETWORK v1" ],
        "summary" : "Get repositories.",
        "description" : "Get repositories.",
        "operationId" : "getRepositories",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/network/v1/service" : {
      "get" : {
        "tags" : [ "NETWORK v1" ],
        "summary" : "Get own service.",
        "description" : "Get the servic entry from the current repository.",
        "operationId" : "getService",
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/StoredService"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/network/v1/services/{id}" : {
      "put" : {
        "tags" : [ "NETWORK v1" ],
        "summary" : "Update a service.",
        "description" : "Update an existing service.",
        "operationId" : "updateService",
        "parameters" : [ {
          "name" : "id",
          "in" : "path",
          "description" : "Service id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "Service data object",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Service"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/StoredService"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/aspects" : {
      "put" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Add aspect to node.",
        "description" : "Add aspect to node.",
        "operationId" : "addAspects",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "aspect name, e.g. ccm:lomreplication",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "array",
                "items" : {
                  "type" : "string"
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/workflow" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get workflow history.",
        "description" : "Get workflow history of node.",
        "operationId" : "getWorkflowHistory",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Add workflow.",
        "description" : "Add workflow entry to node.",
        "operationId" : "addWorkflowHistory",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "The history entry to put (editor and time can be null and will be filled automatically)",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/WorkflowHistory"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/content" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Change content of node.",
        "description" : "Change content of node.",
        "operationId" : "changeContent",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment, leave empty = no new version, otherwise new version is generated",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "mimetype",
          "in" : "query",
          "description" : "MIME-Type",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "type" : "object",
                "properties" : {
                  "file" : {
                    "type" : "object"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/textContent" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get the text content of a document.",
        "description" : "May fails with 500 if the node can not be read.",
        "operationId" : "getTextContent",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeText"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Change content of node as text.",
        "description" : "Change content of node as text.",
        "operationId" : "changeContentAsText",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment, leave empty = no new version, otherwise new version is generated",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "mimetype",
          "in" : "query",
          "description" : "MIME-Type",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "The content data to write (text)",
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "type" : "string"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/metadata" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get metadata of node.",
        "description" : "Get metadata of node.",
        "operationId" : "getMetadata",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Change metadata of node.",
        "description" : "Change metadata of node.",
        "operationId" : "changeMetadata",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Change metadata of node (new version).",
        "description" : "Change metadata of node (new version).",
        "operationId" : "changeMetadataWithVersioning",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/preview" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Change preview of node.",
        "description" : "Change preview of node.",
        "operationId" : "changePreview",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "mimetype",
          "in" : "query",
          "description" : "MIME-Type",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "content" : {
            "multipart/form-data" : {
              "schema" : {
                "type" : "object",
                "properties" : {
                  "image" : {
                    "type" : "object"
                  }
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Delete preview of node.",
        "operationId" : "deletePreview",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/metadata/template" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get the metadata template + status for this folder.",
        "description" : "All the given metadata will be inherited to child nodes.",
        "operationId" : "getTemplateMetadata",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Set the metadata template for this folder.",
        "description" : "All the given metadata will be inherited to child nodes.",
        "operationId" : "changeTemplateMetadata",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "enable",
          "in" : "query",
          "description" : "Is the inherition currently enabled",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "requestBody" : {
          "description" : "properties",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/children" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get children of node.",
        "description" : "Get children of node.",
        "operationId" : "getChildren",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of parent node (or \"-userhome-\" for home directory of current user, \"-shared_files-\" for shared folders, \"-to_me_shared_files\" for shared files for the user,\"-my_shared_files-\" for files shared by the user, \"-inbox-\" for the inbox, \"-workflow_receive-\" for files assigned by workflow, \"-saved_search-\" for saved searches of the user)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 500
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "filter",
          "in" : "query",
          "description" : "filter by type files,folders",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "assocName",
          "in" : "query",
          "description" : "Filter for a specific association. May be empty",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Create a new child.",
        "description" : "Create a new child.",
        "operationId" : "createChild",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of parent node use -userhome- for userhome or -inbox- for inbox node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "type",
          "in" : "query",
          "description" : "type of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "aspects",
          "in" : "query",
          "description" : "aspects of node",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "renameIfExists",
          "in" : "query",
          "description" : "rename if the same node name exists",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment, leave empty = no inital version",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "assocType",
          "in" : "query",
          "description" : "Association type, can be empty",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties, example: {\"{http://www.alfresco.org/model/content/1.0}name\": [\"test\"]}",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/children/_copy" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Create a new child by copying.",
        "description" : "Create a new child by copying.",
        "operationId" : "createChildByCopying",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of parent node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "source",
          "in" : "query",
          "description" : "ID of source node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "withChildren",
          "in" : "query",
          "description" : "flag for children",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/children/_move" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Create a new child by moving.",
        "description" : "Create a new child by moving.",
        "operationId" : "createChildByMoving",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of parent node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "source",
          "in" : "query",
          "description" : "ID of source node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/children/_fork" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Create a copy of a node by creating a forked version (variant).",
        "operationId" : "createForkOfNode",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of parent node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "source",
          "in" : "query",
          "description" : "ID of source node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "withChildren",
          "in" : "query",
          "description" : "flag for children",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/shares" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get shares of node.",
        "description" : "Get list of shares (via mail/token) for a node.",
        "operationId" : "getShares",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "email",
          "in" : "query",
          "description" : "Filter for a specific email or use LINK for link shares (Optional)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Create a share for a node.",
        "description" : "Create a new share for a node",
        "operationId" : "createShare",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "expiryDate",
          "in" : "query",
          "description" : "expiry date for this share, leave empty or -1 for unlimited",
          "schema" : {
            "type" : "integer",
            "format" : "int64",
            "default" : -1
          }
        }, {
          "name" : "password",
          "in" : "query",
          "description" : "password for this share, use none to not use a password",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeShare"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}" : {
      "delete" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Delete node.",
        "description" : "Delete node.",
        "operationId" : "delete",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "recycle",
          "in" : "query",
          "description" : "move the node to recycle",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        }, {
          "name" : "protocol",
          "in" : "query",
          "description" : "protocol",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "store",
          "in" : "query",
          "description" : "store",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/assocs" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get related nodes.",
        "description" : "Get nodes related based on an assoc.",
        "operationId" : "getAssocs",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 500
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "direction",
          "in" : "query",
          "description" : "Either where the given node should be the \"SOURCE\" or the \"TARGET\"",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "SOURCE", "TARGET" ]
          }
        }, {
          "name" : "assocName",
          "in" : "query",
          "description" : "Association name (e.g. ccm:forkio).",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Searching nodes.",
        "description" : "Searching nodes.",
        "operationId" : "getNodes",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "query",
          "in" : "query",
          "description" : "lucene query",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "facets",
          "in" : "query",
          "description" : "facets",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResult"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/notifys" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get notifys (sharing history) of the node.",
        "description" : "Ordered by the time of each notify",
        "operationId" : "getNotifyList",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/parents" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get parents of node.",
        "description" : "Get all parents metadata + own metadata of node. Index 0 is always the current node",
        "operationId" : "getParents",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "fullPath",
          "in" : "query",
          "description" : "activate to return the full alfresco path, otherwise the path for the user home is resolved",
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ParentEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/permissions" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get all permission of node.",
        "description" : "Get all permission of node.",
        "operationId" : "getPermission",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodePermissionEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Set local permissions of node.",
        "description" : "Set local permissions of node.",
        "operationId" : "setPermission",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "mailtext",
          "in" : "query",
          "description" : "mailtext",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "sendMail",
          "in" : "query",
          "description" : "sendMail",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        }, {
          "name" : "sendCopy",
          "in" : "query",
          "description" : "sendCopy",
          "required" : true,
          "schema" : {
            "type" : "boolean"
          }
        } ],
        "requestBody" : {
          "description" : "permissions",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/ACL"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/publish" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Publish",
        "description" : "Get all published copies of the current node",
        "operationId" : "getPublishedCopies",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Publish",
        "description" : "Create a published copy of the current node ",
        "operationId" : "publishCopy",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "handleMode",
          "in" : "query",
          "description" : "handle mode, if a handle should be created. Skip this parameter if you don't want an handle",
          "schema" : {
            "type" : "string",
            "enum" : [ "distinct", "update" ]
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/versions/{major}/{minor}/metadata" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get metadata of node version.",
        "description" : "Get metadata of node version.",
        "operationId" : "getVersionMetadata",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "major",
          "in" : "path",
          "description" : "major version",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int32"
          }
        }, {
          "name" : "minor",
          "in" : "path",
          "description" : "minor version",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int32"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeVersionEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/versions" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Get all versions of node.",
        "description" : "Get all versions of node.",
        "operationId" : "getVersions",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeVersionRefEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/permissions/{user}" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Which permissions has user/group for node.",
        "description" : "Check for actual permissions (also when user is in groups) for a specific node",
        "operationId" : "hasPermission",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "user",
          "in" : "path",
          "description" : "Authority (user/group) to check (use \"-me-\" for current user",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/import" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Import node",
        "description" : "Import a node from a foreign repository to the local repository.",
        "operationId" : "importNode",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "The id of the foreign repository",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "parent",
          "in" : "query",
          "description" : "Parent node where to store it locally, may also use -userhome- or -inbox-",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/lock/status" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "locked status of a node.",
        "description" : "locked status of a node.",
        "operationId" : "islocked",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeLocked"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/prepareUsage" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "create remote object and get properties.",
        "description" : "create remote object and get properties.",
        "operationId" : "prepareUsage",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeRemote"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/shares/{shareId}" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "update share of a node.",
        "description" : "update the specified share id",
        "operationId" : "updateShare",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "shareId",
          "in" : "path",
          "description" : "share id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "expiryDate",
          "in" : "query",
          "description" : "expiry date for this share, leave empty or -1 for unlimited",
          "schema" : {
            "type" : "integer",
            "format" : "int64",
            "default" : -1
          }
        }, {
          "name" : "password",
          "in" : "query",
          "description" : "new password for share, leave empty if you don't want to change it",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeShare"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Remove share of a node.",
        "description" : "Remove the specified share id",
        "operationId" : "removeShare",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "shareId",
          "in" : "path",
          "description" : "share id",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/report" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Report the node.",
        "description" : "Report a node to notify the admin about an issue)",
        "operationId" : "reportNode",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "reason",
          "in" : "query",
          "description" : "the reason for the report",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "userEmail",
          "in" : "query",
          "description" : "mail of reporting user",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "userComment",
          "in" : "query",
          "description" : "additional user comment",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/versions/{major}/{minor}/_revert" : {
      "put" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Revert to node version.",
        "description" : "Revert to node version.",
        "operationId" : "revertVersion",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "major",
          "in" : "path",
          "description" : "major version",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int32"
          }
        }, {
          "name" : "minor",
          "in" : "path",
          "description" : "minor version",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int32"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/owner" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Set owner of node.",
        "description" : "Set owner of node.",
        "operationId" : "setOwner",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "username",
          "in" : "query",
          "description" : "username",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/property" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Set single property of node.",
        "description" : "When the property is unset (null), it will be removed",
        "operationId" : "setProperty",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "property",
          "in" : "query",
          "description" : "property",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "value",
          "in" : "query",
          "description" : "value",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/xapi" : {
      "post" : {
        "tags" : [ "NODE v1" ],
        "summary" : "Store xApi-Conform data for a given node",
        "operationId" : "storeXApiData",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "xApi conform json data",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "object"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/node/v1/nodes/{repository}/{node}/lock/unlock" : {
      "get" : {
        "tags" : [ "NODE v1" ],
        "summary" : "unlock node.",
        "description" : "unlock node.",
        "operationId" : "unlock",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/organization/v1/organizations/{repository}/{organization}" : {
      "get" : {
        "tags" : [ "ORGANIZATION v1" ],
        "summary" : "Get organization by id.",
        "description" : "Get organization by id.",
        "operationId" : "getOrganization",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "organization",
          "in" : "path",
          "description" : "ID of organization",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Organization"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "put" : {
        "tags" : [ "ORGANIZATION v1" ],
        "summary" : "create organization in repository.",
        "description" : "create organization in repository.",
        "operationId" : "createOrganizations",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "organization",
          "in" : "path",
          "description" : "organization name",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "eduscope",
          "in" : "query",
          "description" : "eduscope (may be null)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Organization"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "ORGANIZATION v1" ],
        "summary" : "Delete organization of repository.",
        "description" : "Delete organization of repository.",
        "operationId" : "deleteOrganizations",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "organization",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/organization/v1/organizations/{repository}" : {
      "get" : {
        "tags" : [ "ORGANIZATION v1" ],
        "summary" : "Get organizations of repository.",
        "description" : "Get organizations of repository the current user is member. May returns an empty list.",
        "operationId" : "getOrganizations",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "pattern",
          "in" : "query",
          "description" : "pattern",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "onlyMemberships",
          "in" : "query",
          "description" : "search only in memberships, false can only be done by admin",
          "schema" : {
            "type" : "boolean",
            "default" : true
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/OrganizationEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/organization/v1/organizations/{repository}/{organization}/member/{member}" : {
      "delete" : {
        "tags" : [ "ORGANIZATION v1" ],
        "summary" : "Remove member from organization.",
        "description" : "Remove member from organization.",
        "operationId" : "removeFromOrganization",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "organization",
          "in" : "path",
          "description" : "groupname",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "member",
          "in" : "path",
          "description" : "authorityName of member",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/rating/v1/ratings/{repository}/{node}" : {
      "put" : {
        "tags" : [ "RATING v1" ],
        "summary" : "create or update a rating",
        "description" : "Adds the rating. If the current user already rated that element, the rating will be altered",
        "operationId" : "addOrUpdateRating",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "rating",
          "in" : "query",
          "description" : "The rating (usually in range 1-5)",
          "required" : true,
          "schema" : {
            "type" : "number",
            "format" : "double"
          }
        } ],
        "requestBody" : {
          "description" : "Text content of rating",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "string"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "delete" : {
        "tags" : [ "RATING v1" ],
        "summary" : "delete a comment",
        "description" : "Delete the comment with the given id",
        "operationId" : "deleteRating",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/register/v1/activate/{key}" : {
      "post" : {
        "tags" : [ "REGISTER v1" ],
        "summary" : "Activate a new user (by using a supplied key)",
        "operationId" : "activate",
        "parameters" : [ {
          "name" : "key",
          "in" : "path",
          "description" : "The key for the user to activate",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/register/v1/exists/{mail}" : {
      "get" : {
        "tags" : [ "REGISTER v1" ],
        "summary" : "Check if the given mail is already successfully registered",
        "operationId" : "mailExists",
        "parameters" : [ {
          "name" : "mail",
          "in" : "path",
          "description" : "The mail (authority) of the user to check",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/RegisterExists"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/register/v1/recover/{mail}" : {
      "post" : {
        "tags" : [ "REGISTER v1" ],
        "summary" : "Send a mail to recover/reset password",
        "operationId" : "recoverPassword",
        "parameters" : [ {
          "name" : "mail",
          "in" : "path",
          "description" : "The mail (authority) of the user to recover",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/register/v1/register" : {
      "post" : {
        "tags" : [ "REGISTER v1" ],
        "summary" : "Register a new user",
        "operationId" : "register",
        "requestBody" : {
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/RegisterInformation"
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/register/v1/resend/{mail}" : {
      "post" : {
        "tags" : [ "REGISTER v1" ],
        "summary" : "Resend a registration mail for a given mail address",
        "description" : "The method will return false if there is no pending registration for the given mail",
        "operationId" : "resendMail",
        "parameters" : [ {
          "name" : "mail",
          "in" : "path",
          "description" : "The mail a registration is pending for and should be resend to",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/register/v1/reset/{key}/{password}" : {
      "post" : {
        "tags" : [ "REGISTER v1" ],
        "summary" : "Send a mail to recover/reset password",
        "operationId" : "resetPassword",
        "parameters" : [ {
          "name" : "key",
          "in" : "path",
          "description" : "The key for the password reset request",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "password",
          "in" : "path",
          "description" : "The new password for the user",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/rendering/v1/details/{repository}/{node}" : {
      "get" : {
        "tags" : [ "RENDERING v1" ],
        "summary" : "Get metadata of node.",
        "description" : "Get metadata of node.",
        "operationId" : "getDetailsSnippet",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "version",
          "in" : "query",
          "description" : "version of node",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "displayMode",
          "in" : "query",
          "description" : "Rendering displayMode",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/RenderingDetailsEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "RENDERING v1" ],
        "summary" : "Get metadata of node.",
        "description" : "Get metadata of node.",
        "operationId" : "getDetailsSnippetWithParameters",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "version",
          "in" : "query",
          "description" : "version of node",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "displayMode",
          "in" : "query",
          "description" : "Rendering displayMode",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "additional parameters to send to the rendering service",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/RenderingDetailsEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/metadata/{repository}" : {
      "get" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "get nodes with metadata and collections",
        "operationId" : "getMetdata",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "nodeIds",
          "in" : "query",
          "description" : "nodeIds",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/relevant/{repository}" : {
      "get" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Get relevant nodes for the current user",
        "operationId" : "getRelevantNodes",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResultNode"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/queries/load/{nodeId}" : {
      "get" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Load a saved search query.",
        "description" : "Load a saved search query.",
        "operationId" : "loadSaveSearch",
        "parameters" : [ {
          "name" : "nodeId",
          "in" : "path",
          "description" : "Node id of the search item",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "contentType",
          "in" : "query",
          "description" : "Type of element",
          "schema" : {
            "type" : "string",
            "enum" : [ "FILES", "FOLDERS", "FILES_AND_FOLDERS", "COLLECTIONS", "TOOLPERMISSIONS", "COLLECTION_PROPOSALS", "ALL" ]
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "requestBody" : {
          "description" : "facets",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "array",
                "items" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Node"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/queries/{repository}/{metadataset}/{query}/save" : {
      "post" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Save a search query.",
        "description" : "Save a search query.",
        "operationId" : "saveSearch",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        }, {
          "name" : "query",
          "in" : "path",
          "description" : "ID of query",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "name",
          "in" : "query",
          "description" : "Name of the new search item",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "replace",
          "in" : "query",
          "description" : "Replace if search with the same name exists",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        } ],
        "requestBody" : {
          "description" : "search parameters",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "array",
                "items" : {
                  "$ref" : "#/components/schemas/MdsQueryCriteria"
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/queries/{repository}/{metadataset}/{query}" : {
      "post" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Perform queries based on metadata sets.",
        "description" : "Perform queries based on metadata sets.",
        "operationId" : "search",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        }, {
          "name" : "query",
          "in" : "path",
          "description" : "ID of query",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "contentType",
          "in" : "query",
          "description" : "Type of element",
          "schema" : {
            "type" : "string",
            "enum" : [ "FILES", "FOLDERS", "FILES_AND_FOLDERS", "COLLECTIONS", "TOOLPERMISSIONS", "COLLECTION_PROPOSALS", "ALL" ]
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "requestBody" : {
          "description" : "search parameters",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/SearchParameters"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResultNode"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/custom/{repository}" : {
      "get" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Search for custom properties with custom values",
        "description" : "e.g. property=cm:name, value:*Test*",
        "operationId" : "searchByProperty",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "contentType",
          "in" : "query",
          "description" : "Type of element",
          "schema" : {
            "type" : "string",
            "enum" : [ "FILES", "FOLDERS", "FILES_AND_FOLDERS", "COLLECTIONS", "TOOLPERMISSIONS", "COLLECTION_PROPOSALS", "ALL" ]
          }
        }, {
          "name" : "combineMode",
          "in" : "query",
          "description" : "Combine mode, AND or OR, defaults to AND",
          "schema" : {
            "type" : "string",
            "enum" : [ "AND", "OR" ]
          }
        }, {
          "name" : "property",
          "in" : "query",
          "description" : "One (or more) properties to search for, will be combined by specified combine mode",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "value",
          "in" : "query",
          "description" : "One (or more) values to search for, matching the properties defined before",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "comparator",
          "in" : "query",
          "description" : "(Optional) comparator, only relevant for date or numerical fields, currently allowed =, <=, >=",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResultNode"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/queries/{repository}/contributor" : {
      "get" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Search for contributors",
        "operationId" : "searchContributor",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "searchWord",
          "in" : "query",
          "description" : "search word",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "contributorKind",
          "in" : "query",
          "description" : "contributor kind",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "PERSON", "ORGANIZATION" ],
            "default" : "PERSON"
          }
        }, {
          "name" : "fields",
          "in" : "query",
          "description" : "define which authority fields should be searched: ['firstname', 'lastname', 'email', 'uuid', 'url']",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "contributorProperties",
          "in" : "query",
          "description" : "define which contributor props should be searched: ['ccm:lifecyclecontributer_author', 'ccm:lifecyclecontributer_publisher', ..., 'ccm:metadatacontributer_creator', 'ccm:metadatacontributer_validator']",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/queries/{repository}/{metadataset}/{query}/facets" : {
      "post" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Search in facets.",
        "description" : "Perform queries based on metadata sets.",
        "operationId" : "searchFacets",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "metadataset",
          "in" : "path",
          "description" : "ID of metadataset (or \"-default-\" for default metadata set)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-default-"
          }
        }, {
          "name" : "query",
          "in" : "path",
          "description" : "ID of query",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "facet parameters",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/SearchParametersFacets"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResultNode"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/search/v1/queries/{repository}/fingerprint/{nodeid}" : {
      "post" : {
        "tags" : [ "SEARCH v1" ],
        "summary" : "Perform queries based on metadata sets.",
        "description" : "Perform queries based on metadata sets.",
        "operationId" : "searchFingerprint",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "nodeid",
          "in" : "path",
          "description" : "nodeid",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        }, {
          "name" : "propertyFilter",
          "in" : "query",
          "description" : "property filter for result nodes (or \"-all-\" for all properties)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "default" : "-all-"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SearchResultNode"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/sharing/v1/sharing/{repository}/{node}/{share}/children" : {
      "get" : {
        "tags" : [ "SHARING v1" ],
        "summary" : "Get all children of this share.",
        "description" : "Only valid for shared folders",
        "operationId" : "getChildren_1",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "share",
          "in" : "path",
          "description" : "Share token",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "password",
          "in" : "query",
          "description" : "Password (required if share is locked)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 500
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntries"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/sharing/v1/sharing/{repository}/{node}/{share}" : {
      "get" : {
        "tags" : [ "SHARING v1" ],
        "summary" : "Get general info of a share.",
        "operationId" : "getInfo",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "share",
          "in" : "path",
          "description" : "Share token",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "password",
          "in" : "query",
          "description" : "Password to validate (optional)",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/SharingInfo"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/statistic/v1/facets/{context}" : {
      "post" : {
        "tags" : [ "STATISTIC v1" ],
        "summary" : "Get statistics of repository.",
        "description" : "Statistics.",
        "operationId" : "get",
        "parameters" : [ {
          "name" : "context",
          "in" : "path",
          "description" : "context, the node where to start",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-root-"
          }
        }, {
          "name" : "properties",
          "in" : "query",
          "description" : "properties",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "requestBody" : {
          "description" : "filter",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/Filter"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Statistics"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/statistic/v1/public" : {
      "get" : {
        "tags" : [ "STATISTIC v1" ],
        "summary" : "Get stats.",
        "description" : "Get global statistics for this repository.",
        "operationId" : "getGlobalStatistics",
        "parameters" : [ {
          "name" : "group",
          "in" : "query",
          "description" : "primary property to build facets and count+group values",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "subGroup",
          "in" : "query",
          "description" : "additional properties to build facets and count+sub-group values",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/StatisticsGlobal"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/statistic/v1/statistics/nodes/node/{id}" : {
      "get" : {
        "tags" : [ "STATISTIC v1" ],
        "summary" : "get the range of nodes which had tracked actions since a given timestamp",
        "description" : "requires admin",
        "operationId" : "getNodeData",
        "parameters" : [ {
          "name" : "id",
          "in" : "path",
          "description" : "node id to fetch data for",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "dateFrom",
          "in" : "query",
          "description" : "date range from",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/statistic/v1/statistics/nodes/altered" : {
      "get" : {
        "tags" : [ "STATISTIC v1" ],
        "summary" : "get the range of nodes which had tracked actions since a given timestamp",
        "description" : "requires admin",
        "operationId" : "getNodesAlteredInRange",
        "parameters" : [ {
          "name" : "dateFrom",
          "in" : "query",
          "description" : "date range from",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/statistic/v1/statistics/nodes" : {
      "post" : {
        "tags" : [ "STATISTIC v1" ],
        "summary" : "get statistics for node actions",
        "description" : "requires either toolpermission TOOLPERMISSION_GLOBAL_STATISTICS_NODES for global stats or to be admin of the requested mediacenter",
        "operationId" : "getStatisticsNode",
        "parameters" : [ {
          "name" : "grouping",
          "in" : "query",
          "description" : "Grouping type (by date)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "None", "Daily", "Monthly", "Yearly", "Node" ]
          }
        }, {
          "name" : "dateFrom",
          "in" : "query",
          "description" : "date range from",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        }, {
          "name" : "dateTo",
          "in" : "query",
          "description" : "date range to",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        }, {
          "name" : "mediacenter",
          "in" : "query",
          "description" : "the mediacenter to filter for statistics",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "additionalFields",
          "in" : "query",
          "description" : "additionals fields of the custom json object stored in each query that should be returned",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "groupField",
          "in" : "query",
          "description" : "grouping fields of the custom json object stored in each query (currently only meant to be combined with no grouping by date)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "requestBody" : {
          "description" : "filters for the custom json object stored in each entry",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/statistic/v1/statistics/users" : {
      "post" : {
        "tags" : [ "STATISTIC v1" ],
        "summary" : "get statistics for user actions (login, logout)",
        "description" : "requires either toolpermission TOOLPERMISSION_GLOBAL_STATISTICS_USER for global stats or to be admin of the requested mediacenter",
        "operationId" : "getStatisticsUser",
        "parameters" : [ {
          "name" : "grouping",
          "in" : "query",
          "description" : "Grouping type (by date)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "None", "Daily", "Monthly", "Yearly", "Node" ]
          }
        }, {
          "name" : "dateFrom",
          "in" : "query",
          "description" : "date range from",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        }, {
          "name" : "dateTo",
          "in" : "query",
          "description" : "date range to",
          "required" : true,
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        }, {
          "name" : "mediacenter",
          "in" : "query",
          "description" : "the mediacenter to filter for statistics",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "additionalFields",
          "in" : "query",
          "description" : "additionals fields of the custom json object stored in each query that should be returned",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "groupField",
          "in" : "query",
          "description" : "grouping fields of the custom json object stored in each query (currently only meant to be combined with no grouping by date)",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        } ],
        "requestBody" : {
          "description" : "filters for the custom json object stored in each entry",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/stream/v1/add/{repository}" : {
      "put" : {
        "tags" : [ "STREAM v1" ],
        "summary" : "add a new stream object.",
        "description" : "will return the object and add the id to the object if creation succeeded",
        "operationId" : "addEntry",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "requestBody" : {
          "description" : "Stream object to add",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/StreamEntryInput"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/StreamEntryInput"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/stream/v1/access/{repository}/{node}" : {
      "get" : {
        "tags" : [ "STREAM v1" ],
        "summary" : "test",
        "operationId" : "canAccess",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "node",
          "in" : "path",
          "description" : "The property to aggregate",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/stream/v1/delete/{repository}/{entry}" : {
      "delete" : {
        "tags" : [ "STREAM v1" ],
        "summary" : "delete a stream object",
        "description" : "the current user must be author of the given stream object",
        "operationId" : "deleteEntry",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "entry",
          "in" : "path",
          "description" : "entry id to delete",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/stream/v1/properties/{repository}/{property}" : {
      "get" : {
        "tags" : [ "STREAM v1" ],
        "summary" : "Get top values for a property",
        "operationId" : "getPropertyValues",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "property",
          "in" : "path",
          "description" : "The property to aggregate",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/stream/v1/search/{repository}" : {
      "post" : {
        "tags" : [ "STREAM v1" ],
        "summary" : "Get the stream content for the current user with the given status.",
        "operationId" : "search_1",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "status",
          "in" : "query",
          "description" : "Stream object status to search for",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "query",
          "in" : "query",
          "description" : "generic text to search for (in title or description)",
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "maxItems",
          "in" : "query",
          "description" : "maximum items per page",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          }
        }, {
          "name" : "skipCount",
          "in" : "query",
          "description" : "skip a number of items",
          "schema" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 0
          }
        }, {
          "name" : "sortProperties",
          "in" : "query",
          "description" : "sort properties, currently supported: created, priority, default: priority desc, created desc",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }, {
          "name" : "sortAscending",
          "in" : "query",
          "description" : "sort ascending, true if not set. Use multiple values to change the direction according to the given property at the same index",
          "schema" : {
            "type" : "array",
            "items" : {
              "type" : "boolean"
            }
          }
        } ],
        "requestBody" : {
          "description" : "map with property + value to search",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "string"
                }
              }
            }
          }
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/StreamList"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/stream/v1/status/{repository}/{entry}" : {
      "put" : {
        "tags" : [ "STREAM v1" ],
        "summary" : "update status for a stream object and authority",
        "operationId" : "updateEntry",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "entry",
          "in" : "path",
          "description" : "entry id to update",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "authority",
          "in" : "query",
          "description" : "authority to set/change status",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "status",
          "in" : "query",
          "description" : "New status for this authority",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/tool/v1/tools/{repository}/tooldefinitions" : {
      "get" : {
        "tags" : [ "TOOL v1" ],
        "summary" : "Get all ToolDefinitions.",
        "description" : "Get all ToolDefinitions.",
        "operationId" : "getAllToolDefinitions",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "TOOL v1" ],
        "summary" : "Create a new tool definition object.",
        "description" : "Create a new tool definition object.",
        "operationId" : "createToolDefintition",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "renameIfExists",
          "in" : "query",
          "description" : "rename if the same node name exists",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment, leave empty = no inital version",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties, example: {\"{http://www.alfresco.org/model/content/1.0}name\": [\"test\"]}",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/tool/v1/tools/{repository}/{toolDefinition}/toolinstances" : {
      "get" : {
        "tags" : [ "TOOL v1" ],
        "summary" : "Get Instances of a ToolDefinition.",
        "description" : "Get Instances of a ToolDefinition.",
        "operationId" : "getInstances",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "toolDefinition",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      },
      "post" : {
        "tags" : [ "TOOL v1" ],
        "summary" : "Create a new tool Instance object.",
        "description" : "Create a new tool Instance object.",
        "operationId" : "createToolInstance",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "toolDefinition",
          "in" : "path",
          "description" : "ID of parent node must have tool_definition aspect",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "renameIfExists",
          "in" : "query",
          "description" : "rename if the same node name exists",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment, leave empty = no inital version",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties, example: {\"{http://www.alfresco.org/model/content/1.0}name\": [\"test\"]}",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/tool/v1/tools/{repository}/{toolinstance}/toolobject" : {
      "post" : {
        "tags" : [ "TOOL v1" ],
        "summary" : "Create a new tool object for a given tool instance.",
        "description" : "Create a new tool object for a given tool instance.",
        "operationId" : "createToolObject",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "toolinstance",
          "in" : "path",
          "description" : "ID of parent node (a tool instance object)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "renameIfExists",
          "in" : "query",
          "description" : "rename if the same node name exists",
          "schema" : {
            "type" : "boolean",
            "default" : false
          }
        }, {
          "name" : "versionComment",
          "in" : "query",
          "description" : "comment, leave empty = no inital version",
          "schema" : {
            "type" : "string"
          }
        } ],
        "requestBody" : {
          "description" : "properties, example: {\"{http://www.alfresco.org/model/content/1.0}name\": [\"test\"]}",
          "content" : {
            "application/json" : {
              "schema" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "array",
                  "items" : {
                    "type" : "string"
                  }
                }
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "409" : {
            "description" : "Duplicate Entity/Node conflict (Node with same name exists)",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/tool/v1/tools/{repository}/{nodeid}/toolinstance" : {
      "get" : {
        "tags" : [ "TOOL v1" ],
        "summary" : "Get Instances of a ToolDefinition.",
        "description" : "Get Instances of a ToolDefinition.",
        "operationId" : "getInstance",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "nodeid",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/NodeEntry"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/tracking/v1/tracking/{repository}/{event}" : {
      "put" : {
        "tags" : [ "TRACKING v1" ],
        "summary" : "Track a user interaction",
        "description" : "Currently limited to video / audio play interactions",
        "operationId" : "trackEvent",
        "parameters" : [ {
          "name" : "repository",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "event",
          "in" : "path",
          "description" : "type of event to track",
          "required" : true,
          "schema" : {
            "type" : "string",
            "enum" : [ "DOWNLOAD_MATERIAL", "VIEW_MATERIAL", "VIEW_MATERIAL_EMBEDDED", "VIEW_MATERIAL_PLAY_MEDIA", "LOGIN_USER_SESSION", "LOGIN_USER_OAUTH_PASSWORD", "LOGIN_USER_OAUTH_REFRESH_TOKEN", "LOGOUT_USER_TIMEOUT", "LOGOUT_USER_REGULAR" ]
          }
        }, {
          "name" : "node",
          "in" : "query",
          "description" : "node id for which the event is tracked. For some event, this can be null",
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : { }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/usage/v1/usages/node/{nodeId}/{usageId}" : {
      "delete" : {
        "tags" : [ "USAGE v1" ],
        "summary" : "Delete an usage of a node.",
        "operationId" : "deleteUsage",
        "parameters" : [ {
          "name" : "nodeId",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "usageId",
          "in" : "path",
          "description" : "ID of usage",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Usages"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/usage/v1/usages/{appId}" : {
      "get" : {
        "tags" : [ "USAGE v1" ],
        "summary" : "Get all usages of an application.",
        "description" : "Get all usages of an application.",
        "operationId" : "getUsages",
        "parameters" : [ {
          "name" : "appId",
          "in" : "path",
          "description" : "ID of application (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Usages"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/usage/v1/usages/repository/{repositoryId}/{nodeId}" : {
      "get" : {
        "tags" : [ "USAGE v1" ],
        "operationId" : "getUsages_1",
        "parameters" : [ {
          "name" : "repositoryId",
          "in" : "path",
          "description" : "ID of repository",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        }, {
          "name" : "nodeId",
          "in" : "path",
          "description" : "ID of node. Use -all- for getting usages of all nodes",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-all-"
          }
        }, {
          "name" : "from",
          "in" : "query",
          "description" : "from date",
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        }, {
          "name" : "to",
          "in" : "query",
          "description" : "to date",
          "schema" : {
            "type" : "integer",
            "format" : "int64"
          }
        } ],
        "responses" : {
          "default" : {
            "description" : "default response",
            "content" : {
              "application/json" : { }
            }
          }
        }
      }
    },
    "/usage/v1/usages/course/{appId}/{courseId}" : {
      "get" : {
        "tags" : [ "USAGE v1" ],
        "summary" : "Get all usages of an course.",
        "description" : "Get all usages of an course.",
        "operationId" : "getUsagesByCourse",
        "parameters" : [ {
          "name" : "appId",
          "in" : "path",
          "description" : "ID of application (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        }, {
          "name" : "courseId",
          "in" : "path",
          "description" : "ID of course",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Usages"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/usage/v1/usages/node/{nodeId}" : {
      "get" : {
        "tags" : [ "USAGE v1" ],
        "summary" : "Get all usages of an node.",
        "description" : "Get all usages of an node.",
        "operationId" : "getUsagesByNode",
        "parameters" : [ {
          "name" : "nodeId",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Usages"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/usage/v1/usages/node/{nodeId}/collections" : {
      "get" : {
        "tags" : [ "USAGE v1" ],
        "summary" : "Get all collections where this node is used.",
        "operationId" : "getUsagesByNodeCollections",
        "parameters" : [ {
          "name" : "nodeId",
          "in" : "path",
          "description" : "ID of node",
          "required" : true,
          "schema" : {
            "type" : "string"
          }
        } ],
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "type" : "string"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    },
    "/usage/v1/usages/repository/{repositoryId}" : {
      "post" : {
        "tags" : [ "USAGE v1" ],
        "summary" : "Set a usage for a node. app signature headers and authenticated user required.",
        "description" : "headers must be set: X-Edu-App-Id, X-Edu-App-Sig, X-Edu-App-Signed, X-Edu-App-Ts",
        "operationId" : "setUsage",
        "parameters" : [ {
          "name" : "repositoryId",
          "in" : "path",
          "description" : "ID of repository (or \"-home-\" for home repository)",
          "required" : true,
          "schema" : {
            "type" : "string",
            "default" : "-home-"
          }
        } ],
        "requestBody" : {
          "description" : " usage date",
          "content" : {
            "application/json" : {
              "schema" : {
                "$ref" : "#/components/schemas/CreateUsage"
              }
            }
          },
          "required" : true
        },
        "responses" : {
          "200" : {
            "description" : "OK.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/Usage"
                }
              }
            }
          },
          "400" : {
            "description" : "Preconditions are not present.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "401" : {
            "description" : "Authorization failed.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "403" : {
            "description" : "Session user has insufficient rights to perform this operation.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "404" : {
            "description" : "Ressources are not found.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          },
          "500" : {
            "description" : "Fatal error occured.",
            "content" : {
              "application/json" : {
                "schema" : {
                  "$ref" : "#/components/schemas/ErrorResponse"
                }
              }
            }
          }
        }
      }
    }
  },
  "components" : {
    "schemas" : {
      "About" : {
        "required" : [ "services", "version" ],
        "type" : "object",
        "properties" : {
          "themesUrl" : {
            "type" : "string"
          },
          "lastCacheUpdate" : {
            "type" : "integer",
            "format" : "int64"
          },
          "version" : {
            "$ref" : "#/components/schemas/ServiceVersion"
          },
          "services" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Service"
            }
          }
        }
      },
      "Service" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "logo" : {
            "type" : "string"
          },
          "inLanguage" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "audience" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Audience"
            }
          },
          "provider" : {
            "$ref" : "#/components/schemas/Provider"
          },
          "startDate" : {
            "type" : "string"
          },
          "interfaces" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Interface"
            }
          },
          "about" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "isAccessibleForFree" : {
            "type" : "boolean"
          }
        }
      },
      "ServiceInstance" : {
        "required" : [ "endpoint", "version" ],
        "type" : "object",
        "properties" : {
          "version" : {
            "$ref" : "#/components/schemas/ServiceVersion"
          },
          "endpoint" : {
            "type" : "string"
          }
        }
      },
      "ServiceVersion" : {
        "required" : [ "major", "minor" ],
        "type" : "object",
        "properties" : {
          "repository" : {
            "type" : "string"
          },
          "renderservice" : {
            "type" : "string"
          },
          "major" : {
            "type" : "integer",
            "format" : "int32"
          },
          "minor" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "ErrorResponse" : {
        "required" : [ "error", "message", "stacktraceArray" ],
        "type" : "object",
        "properties" : {
          "stacktrace" : {
            "type" : "string",
            "writeOnly" : true
          },
          "error" : {
            "type" : "string"
          },
          "message" : {
            "type" : "string"
          },
          "logLevel" : {
            "type" : "string"
          },
          "stacktraceArray" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Collection" : {
        "required" : [ "fromUser", "level0", "title", "type", "viewtype" ],
        "type" : "object",
        "properties" : {
          "scope" : {
            "type" : "string"
          },
          "authorFreetext" : {
            "type" : "string"
          },
          "orderAscending" : {
            "type" : "boolean"
          },
          "level0" : {
            "type" : "boolean",
            "description" : "false"
          },
          "title" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          },
          "viewtype" : {
            "type" : "string"
          },
          "orderMode" : {
            "type" : "string"
          },
          "x" : {
            "type" : "integer",
            "format" : "int32"
          },
          "y" : {
            "type" : "integer",
            "format" : "int32"
          },
          "z" : {
            "type" : "integer",
            "format" : "int32"
          },
          "color" : {
            "type" : "string"
          },
          "fromUser" : {
            "type" : "boolean",
            "description" : "false"
          },
          "pinned" : {
            "type" : "boolean"
          },
          "childCollectionsCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "childReferencesCount" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "Content" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          },
          "hash" : {
            "type" : "string"
          },
          "version" : {
            "type" : "string"
          }
        }
      },
      "Contributor" : {
        "type" : "object",
        "properties" : {
          "property" : {
            "type" : "string"
          },
          "firstname" : {
            "type" : "string"
          },
          "lastname" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "vcard" : {
            "type" : "string"
          },
          "org" : {
            "type" : "string"
          }
        }
      },
      "License" : {
        "type" : "object",
        "properties" : {
          "icon" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          }
        }
      },
      "Node" : {
        "required" : [ "access", "collection", "createdAt", "createdBy", "downloadUrl", "name", "owner", "ref" ],
        "type" : "object",
        "properties" : {
          "nodeLTIDeepLink" : {
            "$ref" : "#/components/schemas/NodeLTIDeepLink"
          },
          "remote" : {
            "$ref" : "#/components/schemas/Remote"
          },
          "content" : {
            "$ref" : "#/components/schemas/Content"
          },
          "license" : {
            "$ref" : "#/components/schemas/License"
          },
          "isDirectory" : {
            "type" : "boolean"
          },
          "commentCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "rating" : {
            "$ref" : "#/components/schemas/RatingDetails"
          },
          "usedInCollections" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "relations" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "contributors" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Contributor"
            }
          },
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "parent" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "type" : {
            "type" : "string"
          },
          "aspects" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "name" : {
            "type" : "string"
          },
          "title" : {
            "type" : "string"
          },
          "metadataset" : {
            "type" : "string"
          },
          "repositoryType" : {
            "type" : "string"
          },
          "createdAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "createdBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "modifiedAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "modifiedBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "access" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "downloadUrl" : {
            "type" : "string"
          },
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "mimetype" : {
            "type" : "string"
          },
          "mediatype" : {
            "type" : "string"
          },
          "size" : {
            "type" : "string"
          },
          "preview" : {
            "$ref" : "#/components/schemas/Preview"
          },
          "iconURL" : {
            "type" : "string"
          },
          "collection" : {
            "$ref" : "#/components/schemas/Collection"
          },
          "owner" : {
            "$ref" : "#/components/schemas/Person"
          },
          "isPublic" : {
            "type" : "boolean"
          }
        }
      },
      "NodeLTIDeepLink" : {
        "type" : "object",
        "properties" : {
          "ltiDeepLinkReturnUrl" : {
            "type" : "string"
          },
          "jwtDeepLinkResponse" : {
            "type" : "string"
          }
        }
      },
      "NodeRef" : {
        "required" : [ "archived", "id", "repo" ],
        "type" : "object",
        "properties" : {
          "repo" : {
            "type" : "string"
          },
          "id" : {
            "type" : "string"
          },
          "archived" : {
            "type" : "boolean"
          },
          "isHomeRepo" : {
            "type" : "boolean"
          }
        }
      },
      "Person" : {
        "type" : "object",
        "properties" : {
          "profile" : {
            "$ref" : "#/components/schemas/UserProfile"
          },
          "firstName" : {
            "type" : "string"
          },
          "lastName" : {
            "type" : "string"
          },
          "mailbox" : {
            "type" : "string"
          }
        }
      },
      "Preview" : {
        "required" : [ "height", "isIcon", "url", "width" ],
        "type" : "object",
        "properties" : {
          "isIcon" : {
            "type" : "boolean"
          },
          "isGenerated" : {
            "type" : "boolean"
          },
          "type" : {
            "type" : "string"
          },
          "mimetype" : {
            "type" : "string"
          },
          "data" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "format" : "byte"
            }
          },
          "url" : {
            "type" : "string"
          },
          "width" : {
            "type" : "integer",
            "format" : "int32"
          },
          "height" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "RatingData" : {
        "type" : "object",
        "properties" : {
          "sum" : {
            "type" : "number",
            "format" : "double"
          },
          "count" : {
            "type" : "integer",
            "format" : "int64"
          },
          "rating" : {
            "type" : "number",
            "format" : "double"
          }
        }
      },
      "RatingDetails" : {
        "type" : "object",
        "properties" : {
          "overall" : {
            "$ref" : "#/components/schemas/RatingData"
          },
          "affiliation" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/RatingData"
            }
          },
          "user" : {
            "type" : "number",
            "format" : "double"
          }
        }
      },
      "Remote" : {
        "type" : "object",
        "properties" : {
          "repository" : {
            "$ref" : "#/components/schemas/Repo"
          },
          "id" : {
            "type" : "string"
          }
        }
      },
      "Repo" : {
        "type" : "object",
        "properties" : {
          "repositoryType" : {
            "type" : "string"
          },
          "renderingSupported" : {
            "type" : "boolean"
          },
          "id" : {
            "type" : "string"
          },
          "title" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "logo" : {
            "type" : "string"
          },
          "isHomeRepo" : {
            "type" : "boolean"
          }
        }
      },
      "UserProfile" : {
        "type" : "object",
        "properties" : {
          "primaryAffiliation" : {
            "type" : "string"
          },
          "skills" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "types" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "vcard" : {
            "type" : "string"
          },
          "type" : {
            "type" : "array",
            "writeOnly" : true,
            "items" : {
              "type" : "string"
            }
          },
          "firstName" : {
            "type" : "string"
          },
          "lastName" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "avatar" : {
            "type" : "string"
          },
          "about" : {
            "type" : "string"
          }
        }
      },
      "CollectionCounts" : {
        "type" : "object",
        "properties" : {
          "refs" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Element"
            }
          },
          "collections" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Element"
            }
          }
        }
      },
      "CollectionOptions" : {
        "type" : "object",
        "properties" : {
          "privateCollections" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "publicCollections" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          }
        }
      },
      "Counts" : {
        "type" : "object",
        "properties" : {
          "elements" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Element"
            }
          }
        }
      },
      "DeleteOption" : {
        "type" : "object",
        "properties" : {
          "delete" : {
            "type" : "boolean"
          }
        }
      },
      "Element" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "name" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          }
        }
      },
      "HomeFolderOptions" : {
        "type" : "object",
        "properties" : {
          "folders" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "privateFiles" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "ccFiles" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "keepFolderStructure" : {
            "type" : "boolean"
          }
        }
      },
      "PersonDeleteOptions" : {
        "type" : "object",
        "properties" : {
          "cleanupMetadata" : {
            "type" : "boolean"
          },
          "homeFolder" : {
            "$ref" : "#/components/schemas/HomeFolderOptions"
          },
          "sharedFolders" : {
            "$ref" : "#/components/schemas/SharedFolderOptions"
          },
          "collections" : {
            "$ref" : "#/components/schemas/CollectionOptions"
          },
          "ratings" : {
            "$ref" : "#/components/schemas/DeleteOption"
          },
          "comments" : {
            "$ref" : "#/components/schemas/DeleteOption"
          },
          "collectionFeedback" : {
            "$ref" : "#/components/schemas/DeleteOption"
          },
          "statistics" : {
            "$ref" : "#/components/schemas/DeleteOption"
          },
          "stream" : {
            "$ref" : "#/components/schemas/DeleteOption"
          },
          "receiver" : {
            "type" : "string"
          },
          "receiverGroup" : {
            "type" : "string"
          }
        }
      },
      "PersonDeleteResult" : {
        "type" : "object",
        "properties" : {
          "authorityName" : {
            "type" : "string"
          },
          "deletedName" : {
            "type" : "string"
          },
          "homeFolder" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/Counts"
            }
          },
          "sharedFolders" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/Counts"
            }
          },
          "collections" : {
            "$ref" : "#/components/schemas/CollectionCounts"
          },
          "comments" : {
            "type" : "integer",
            "format" : "int32"
          },
          "ratings" : {
            "type" : "integer",
            "format" : "int32"
          },
          "collectionFeedback" : {
            "type" : "integer",
            "format" : "int32"
          },
          "stream" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "PersonReport" : {
        "type" : "object",
        "properties" : {
          "options" : {
            "$ref" : "#/components/schemas/PersonDeleteOptions"
          },
          "results" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/PersonDeleteResult"
            }
          }
        }
      },
      "SharedFolderOptions" : {
        "type" : "object",
        "properties" : {
          "folders" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "privateFiles" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "ccFiles" : {
            "type" : "string",
            "enum" : [ "none", "assign", "delete" ]
          },
          "move" : {
            "type" : "boolean"
          }
        }
      },
      "JobDescription" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "params" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/JobFieldDescription"
            }
          },
          "tags" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "enum" : [ "DeletePersonJob" ]
            }
          }
        }
      },
      "JobFieldDescription" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "file" : {
            "type" : "boolean"
          },
          "sampleValue" : {
            "type" : "string"
          },
          "isArray" : {
            "type" : "boolean",
            "writeOnly" : true
          },
          "array" : {
            "type" : "boolean"
          }
        }
      },
      "Application" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "title" : {
            "type" : "string"
          },
          "webserverUrl" : {
            "type" : "string"
          },
          "clientBaseUrl" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          },
          "subtype" : {
            "type" : "string"
          },
          "repositoryType" : {
            "type" : "string"
          },
          "xml" : {
            "type" : "string"
          },
          "file" : {
            "type" : "string"
          },
          "contentUrl" : {
            "type" : "string"
          },
          "configUrl" : {
            "type" : "string"
          }
        }
      },
      "CacheInfo" : {
        "type" : "object",
        "properties" : {
          "size" : {
            "type" : "integer",
            "format" : "int32"
          },
          "statisticHits" : {
            "type" : "integer",
            "format" : "int64"
          },
          "name" : {
            "type" : "string"
          },
          "backupCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "backupEntryCount" : {
            "type" : "integer",
            "format" : "int64"
          },
          "backupEntryMemoryCost" : {
            "type" : "integer",
            "format" : "int64"
          },
          "heapCost" : {
            "type" : "integer",
            "format" : "int64"
          },
          "ownedEntryCount" : {
            "type" : "integer",
            "format" : "int64"
          },
          "getOwnedEntryMemoryCost" : {
            "type" : "integer",
            "format" : "int64"
          },
          "sizeInMemory" : {
            "type" : "integer",
            "format" : "int64"
          },
          "member" : {
            "type" : "string"
          },
          "groupName" : {
            "type" : "string"
          },
          "maxSize" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "CacheCluster" : {
        "type" : "object",
        "properties" : {
          "instances" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/CacheMember"
            }
          },
          "cacheInfos" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/CacheInfo"
            }
          },
          "localMember" : {
            "type" : "string"
          },
          "freeMemory" : {
            "type" : "integer",
            "format" : "int64"
          },
          "totalMemory" : {
            "type" : "integer",
            "format" : "int64"
          },
          "maxMemory" : {
            "type" : "integer",
            "format" : "int64"
          },
          "availableProcessors" : {
            "type" : "integer",
            "format" : "int32"
          },
          "timeStamp" : {
            "type" : "string",
            "format" : "date-time"
          },
          "groupName" : {
            "type" : "string"
          }
        }
      },
      "CacheMember" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          }
        }
      },
      "Condition" : {
        "type" : "object",
        "properties" : {
          "type" : {
            "type" : "string",
            "enum" : [ "TOOLPERMISSION" ]
          },
          "negate" : {
            "type" : "boolean"
          },
          "value" : {
            "type" : "string"
          }
        }
      },
      "Frontpage" : {
        "type" : "object",
        "properties" : {
          "totalCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "displayCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "mode" : {
            "type" : "string",
            "enum" : [ "collection", "rating", "views", "downloads" ]
          },
          "timespan" : {
            "type" : "integer",
            "format" : "int32"
          },
          "timespanAll" : {
            "type" : "boolean"
          },
          "queries" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Query"
            }
          },
          "collection" : {
            "type" : "string"
          }
        }
      },
      "Query" : {
        "type" : "object",
        "properties" : {
          "condition" : {
            "$ref" : "#/components/schemas/Condition"
          },
          "query" : {
            "type" : "string"
          }
        }
      },
      "RepositoryConfig" : {
        "type" : "object",
        "properties" : {
          "frontpage" : {
            "$ref" : "#/components/schemas/Frontpage"
          }
        }
      },
      "Group" : {
        "required" : [ "authorityName" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "editable" : {
            "type" : "boolean"
          },
          "signupMethod" : {
            "type" : "string",
            "enum" : [ "simple", "password", "list" ]
          },
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "aspects" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "organizations" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Organization"
            }
          },
          "authorityName" : {
            "type" : "string"
          },
          "authorityType" : {
            "type" : "string",
            "enum" : [ "USER", "GROUP", "OWNER", "EVERYONE", "GUEST" ]
          },
          "groupName" : {
            "type" : "string"
          },
          "profile" : {
            "$ref" : "#/components/schemas/GroupProfile"
          }
        }
      },
      "GroupProfile" : {
        "type" : "object",
        "properties" : {
          "groupEmail" : {
            "type" : "string"
          },
          "displayName" : {
            "type" : "string"
          },
          "groupType" : {
            "type" : "string"
          },
          "scopeType" : {
            "type" : "string"
          }
        }
      },
      "Organization" : {
        "required" : [ "authorityName" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "editable" : {
            "type" : "boolean"
          },
          "signupMethod" : {
            "type" : "string",
            "enum" : [ "simple", "password", "list" ]
          },
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "aspects" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "authorityName" : {
            "type" : "string"
          },
          "authorityType" : {
            "type" : "string",
            "enum" : [ "USER", "GROUP", "OWNER", "EVERYONE", "GUEST" ]
          },
          "groupName" : {
            "type" : "string"
          },
          "profile" : {
            "$ref" : "#/components/schemas/GroupProfile"
          },
          "administrationAccess" : {
            "type" : "boolean"
          },
          "sharedFolder" : {
            "$ref" : "#/components/schemas/NodeRef"
          }
        }
      },
      "JobDataMap" : {
        "type" : "object",
        "properties" : {
          "dirty" : {
            "type" : "boolean"
          },
          "allowsTransientData" : {
            "type" : "boolean"
          },
          "keys" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "wrappedMap" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object"
            }
          },
          "empty" : {
            "type" : "boolean"
          }
        },
        "additionalProperties" : {
          "type" : "object"
        }
      },
      "JobDetail" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "group" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "jobDataMap" : {
            "type" : "object",
            "properties" : {
              "dirty" : {
                "type" : "boolean"
              },
              "allowsTransientData" : {
                "type" : "boolean"
              },
              "keys" : {
                "type" : "array",
                "items" : {
                  "type" : "string"
                }
              },
              "wrappedMap" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "object"
                }
              },
              "empty" : {
                "type" : "boolean"
              }
            },
            "additionalProperties" : {
              "type" : "object"
            }
          },
          "volatility" : {
            "type" : "boolean",
            "writeOnly" : true
          },
          "durability" : {
            "type" : "boolean",
            "writeOnly" : true
          },
          "key" : {
            "$ref" : "#/components/schemas/Key"
          },
          "durable" : {
            "type" : "boolean"
          },
          "stateful" : {
            "type" : "boolean"
          },
          "requestsRecovery" : {
            "type" : "boolean",
            "writeOnly" : true
          },
          "jobListenerNames" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "fullName" : {
            "type" : "string"
          },
          "volatile" : {
            "type" : "boolean"
          }
        }
      },
      "JobInfo" : {
        "type" : "object",
        "properties" : {
          "startTime" : {
            "type" : "integer",
            "format" : "int64"
          },
          "finishTime" : {
            "type" : "integer",
            "format" : "int64"
          },
          "status" : {
            "type" : "string",
            "enum" : [ "Running", "Failed", "Aborted", "Finished" ]
          },
          "worstLevel" : {
            "$ref" : "#/components/schemas/Level"
          },
          "log" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/LogEntry"
            }
          },
          "jobDetail" : {
            "$ref" : "#/components/schemas/JobDetail"
          }
        }
      },
      "Key" : {
        "type" : "object",
        "properties" : {
          "first" : {
            "type" : "object"
          },
          "second" : {
            "type" : "object"
          },
          "name" : {
            "type" : "string"
          },
          "group" : {
            "type" : "string"
          }
        }
      },
      "Level" : {
        "type" : "object",
        "properties" : {
          "syslogEquivalent" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "LogEntry" : {
        "type" : "object",
        "properties" : {
          "className" : {
            "type" : "string"
          },
          "level" : {
            "$ref" : "#/components/schemas/Level"
          },
          "date" : {
            "type" : "integer",
            "format" : "int64"
          },
          "message" : {
            "type" : "string"
          }
        }
      },
      "AdminStatistics" : {
        "type" : "object",
        "properties" : {
          "activeSessions" : {
            "type" : "integer",
            "format" : "int32"
          },
          "numberOfPreviews" : {
            "type" : "integer",
            "format" : "int32"
          },
          "maxMemory" : {
            "type" : "integer",
            "format" : "int64"
          },
          "allocatedMemory" : {
            "type" : "integer",
            "format" : "int64"
          },
          "previewCacheSize" : {
            "type" : "integer",
            "format" : "int64"
          },
          "activeLocks" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          }
        }
      },
      "CollectionsResult" : {
        "type" : "object",
        "properties" : {
          "count" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "ExcelResult" : {
        "type" : "object",
        "properties" : {
          "rows" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "Facet" : {
        "required" : [ "property", "values" ],
        "type" : "object",
        "properties" : {
          "property" : {
            "type" : "string"
          },
          "values" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Value"
            }
          },
          "sumOtherDocCount" : {
            "type" : "integer",
            "format" : "int64"
          }
        }
      },
      "Pagination" : {
        "required" : [ "count", "from", "total" ],
        "type" : "object",
        "properties" : {
          "total" : {
            "type" : "integer",
            "format" : "int32"
          },
          "from" : {
            "type" : "integer",
            "format" : "int32"
          },
          "count" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "SearchResultElastic" : {
        "required" : [ "facets", "nodes", "pagination" ],
        "type" : "object",
        "properties" : {
          "suggests" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Suggest"
            }
          },
          "elasticResponse" : {
            "type" : "string"
          },
          "nodes" : {
            "type" : "array",
            "items" : {
              "type" : "object"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "facets" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Facet"
            }
          },
          "ignored" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Suggest" : {
        "required" : [ "score", "text" ],
        "type" : "object",
        "properties" : {
          "text" : {
            "type" : "string",
            "description" : "suggested text"
          },
          "highlighted" : {
            "type" : "string",
            "description" : "suggested text with corrected words highlighted"
          },
          "score" : {
            "type" : "number",
            "description" : "score of the suggestion",
            "format" : "float"
          }
        }
      },
      "Value" : {
        "required" : [ "count", "value" ],
        "type" : "object",
        "properties" : {
          "value" : {
            "type" : "string"
          },
          "count" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "SearchResult" : {
        "required" : [ "facets", "nodes", "pagination" ],
        "type" : "object",
        "properties" : {
          "nodes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "facets" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Facet"
            }
          }
        }
      },
      "ServerUpdateInfo" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "executedAt" : {
            "type" : "integer",
            "format" : "int64"
          }
        }
      },
      "UploadResult" : {
        "type" : "object",
        "properties" : {
          "file" : {
            "type" : "string"
          }
        }
      },
      "RestoreResult" : {
        "required" : [ "archiveNodeId", "name", "nodeId", "parent", "path", "restoreStatus" ],
        "type" : "object",
        "properties" : {
          "archiveNodeId" : {
            "type" : "string"
          },
          "nodeId" : {
            "type" : "string"
          },
          "parent" : {
            "type" : "string"
          },
          "path" : {
            "type" : "string"
          },
          "name" : {
            "type" : "string"
          },
          "restoreStatus" : {
            "type" : "string"
          }
        }
      },
      "RestoreResults" : {
        "required" : [ "results" ],
        "type" : "object",
        "properties" : {
          "results" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/RestoreResult"
            }
          }
        }
      },
      "NodeEntry" : {
        "required" : [ "node" ],
        "type" : "object",
        "properties" : {
          "node" : {
            "$ref" : "#/components/schemas/Node"
          }
        }
      },
      "WebsiteInformation" : {
        "type" : "object",
        "properties" : {
          "duplicateNodes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "title" : {
            "type" : "string"
          },
          "page" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "license" : {
            "type" : "string"
          },
          "keywords" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "CollectionEntry" : {
        "required" : [ "collection" ],
        "type" : "object",
        "properties" : {
          "collection" : {
            "$ref" : "#/components/schemas/Node"
          }
        }
      },
      "CollectionProposalEntries" : {
        "required" : [ "collections" ],
        "type" : "object",
        "properties" : {
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "collections" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/NodeCollectionProposalCount"
            }
          }
        }
      },
      "NodeCollectionProposalCount" : {
        "required" : [ "access", "collection", "createdAt", "createdBy", "downloadUrl", "name", "owner", "ref" ],
        "type" : "object",
        "properties" : {
          "nodeLTIDeepLink" : {
            "$ref" : "#/components/schemas/NodeLTIDeepLink"
          },
          "remote" : {
            "$ref" : "#/components/schemas/Remote"
          },
          "content" : {
            "$ref" : "#/components/schemas/Content"
          },
          "license" : {
            "$ref" : "#/components/schemas/License"
          },
          "isDirectory" : {
            "type" : "boolean"
          },
          "commentCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "rating" : {
            "$ref" : "#/components/schemas/RatingDetails"
          },
          "usedInCollections" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "relations" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "contributors" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Contributor"
            }
          },
          "proposalCounts" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "integer",
              "format" : "int32"
            }
          },
          "proposalCount" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "integer",
              "format" : "int32"
            },
            "writeOnly" : true
          },
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "parent" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "type" : {
            "type" : "string"
          },
          "aspects" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "name" : {
            "type" : "string"
          },
          "title" : {
            "type" : "string"
          },
          "metadataset" : {
            "type" : "string"
          },
          "repositoryType" : {
            "type" : "string"
          },
          "createdAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "createdBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "modifiedAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "modifiedBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "access" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "downloadUrl" : {
            "type" : "string"
          },
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "mimetype" : {
            "type" : "string"
          },
          "mediatype" : {
            "type" : "string"
          },
          "size" : {
            "type" : "string"
          },
          "preview" : {
            "$ref" : "#/components/schemas/Preview"
          },
          "iconURL" : {
            "type" : "string"
          },
          "collection" : {
            "$ref" : "#/components/schemas/Collection"
          },
          "owner" : {
            "$ref" : "#/components/schemas/Person"
          },
          "isPublic" : {
            "type" : "boolean"
          }
        }
      },
      "AbstractEntries" : {
        "required" : [ "nodes", "pagination" ],
        "type" : "object",
        "properties" : {
          "nodes" : {
            "type" : "array",
            "items" : {
              "type" : "object"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "CollectionReference" : {
        "required" : [ "access", "collection", "createdAt", "createdBy", "downloadUrl", "name", "owner", "ref" ],
        "type" : "object",
        "properties" : {
          "nodeLTIDeepLink" : {
            "$ref" : "#/components/schemas/NodeLTIDeepLink"
          },
          "remote" : {
            "$ref" : "#/components/schemas/Remote"
          },
          "content" : {
            "$ref" : "#/components/schemas/Content"
          },
          "license" : {
            "$ref" : "#/components/schemas/License"
          },
          "isDirectory" : {
            "type" : "boolean"
          },
          "commentCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "rating" : {
            "$ref" : "#/components/schemas/RatingDetails"
          },
          "usedInCollections" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "relations" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "contributors" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Contributor"
            }
          },
          "accessOriginal" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "originalRestrictedAccess" : {
            "type" : "boolean"
          },
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "parent" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "type" : {
            "type" : "string"
          },
          "aspects" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "name" : {
            "type" : "string"
          },
          "title" : {
            "type" : "string"
          },
          "metadataset" : {
            "type" : "string"
          },
          "repositoryType" : {
            "type" : "string"
          },
          "createdAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "createdBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "modifiedAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "modifiedBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "access" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "downloadUrl" : {
            "type" : "string"
          },
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "mimetype" : {
            "type" : "string"
          },
          "mediatype" : {
            "type" : "string"
          },
          "size" : {
            "type" : "string"
          },
          "preview" : {
            "$ref" : "#/components/schemas/Preview"
          },
          "iconURL" : {
            "type" : "string"
          },
          "collection" : {
            "$ref" : "#/components/schemas/Collection"
          },
          "owner" : {
            "$ref" : "#/components/schemas/Person"
          },
          "originalId" : {
            "type" : "string"
          },
          "isPublic" : {
            "type" : "boolean"
          }
        }
      },
      "ReferenceEntries" : {
        "required" : [ "references" ],
        "type" : "object",
        "properties" : {
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "references" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/CollectionReference"
            }
          }
        }
      },
      "CollectionFeedback" : {
        "type" : "object",
        "properties" : {
          "createdAt" : {
            "type" : "string",
            "format" : "date-time"
          },
          "creator" : {
            "type" : "string"
          },
          "feedback" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object"
            }
          }
        }
      },
      "CollectionEntries" : {
        "required" : [ "collections" ],
        "type" : "object",
        "properties" : {
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "collections" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          }
        }
      },
      "Comment" : {
        "type" : "object",
        "properties" : {
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "replyTo" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "creator" : {
            "$ref" : "#/components/schemas/UserSimple"
          },
          "created" : {
            "type" : "integer",
            "format" : "int64"
          },
          "comment" : {
            "type" : "string"
          }
        }
      },
      "Comments" : {
        "type" : "object",
        "properties" : {
          "comments" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Comment"
            }
          }
        }
      },
      "UserSimple" : {
        "required" : [ "authorityName" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "editable" : {
            "type" : "boolean"
          },
          "status" : {
            "$ref" : "#/components/schemas/UserStatus"
          },
          "organizations" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Organization"
            }
          },
          "authorityName" : {
            "type" : "string"
          },
          "authorityType" : {
            "type" : "string",
            "enum" : [ "USER", "GROUP", "OWNER", "EVERYONE", "GUEST" ]
          },
          "userName" : {
            "type" : "string"
          },
          "profile" : {
            "$ref" : "#/components/schemas/UserProfile"
          }
        }
      },
      "UserStatus" : {
        "type" : "object",
        "properties" : {
          "status" : {
            "type" : "string",
            "enum" : [ "active", "blocked", "todelete" ]
          },
          "date" : {
            "type" : "integer",
            "format" : "int64"
          }
        }
      },
      "Admin" : {
        "type" : "object",
        "properties" : {
          "statistics" : {
            "$ref" : "#/components/schemas/Statistics"
          }
        }
      },
      "AvailableMds" : {
        "type" : "object",
        "properties" : {
          "repository" : {
            "type" : "string"
          },
          "mds" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Banner" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          },
          "href" : {
            "type" : "string"
          },
          "components" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Collections" : {
        "type" : "object",
        "properties" : {
          "colors" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Config" : {
        "type" : "object",
        "properties" : {
          "current" : {
            "$ref" : "#/components/schemas/Values"
          },
          "global" : {
            "$ref" : "#/components/schemas/Values"
          },
          "language" : {
            "$ref" : "#/components/schemas/Language"
          }
        }
      },
      "ConfigFrontpage" : {
        "type" : "object",
        "properties" : {
          "enabled" : {
            "type" : "boolean"
          }
        }
      },
      "ConfigPrivacy" : {
        "type" : "object",
        "properties" : {
          "cookieDisclaimer" : {
            "type" : "boolean"
          }
        }
      },
      "ConfigPublish" : {
        "type" : "object",
        "properties" : {
          "licenseMandatory" : {
            "type" : "boolean"
          },
          "authorMandatory" : {
            "type" : "boolean"
          }
        }
      },
      "ConfigRating" : {
        "type" : "object",
        "properties" : {
          "mode" : {
            "type" : "string",
            "enum" : [ "none", "likes", "stars" ]
          }
        }
      },
      "ConfigRemote" : {
        "type" : "object",
        "properties" : {
          "rocketchat" : {
            "$ref" : "#/components/schemas/ConfigRemoteRocketchat"
          }
        }
      },
      "ConfigRemoteRocketchat" : {
        "type" : "object"
      },
      "ConfigUpload" : {
        "type" : "object",
        "properties" : {
          "postDialog" : {
            "type" : "string",
            "enum" : [ "SimpleEdit", "Mds" ]
          }
        }
      },
      "ConfigWorkflow" : {
        "type" : "object",
        "properties" : {
          "defaultReceiver" : {
            "type" : "string"
          },
          "defaultStatus" : {
            "type" : "string"
          },
          "commentRequired" : {
            "type" : "boolean"
          },
          "workflows" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/ConfigWorkflowList"
            }
          }
        }
      },
      "ConfigWorkflowList" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "color" : {
            "type" : "string"
          },
          "hasReceiver" : {
            "type" : "boolean"
          },
          "next" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "ContextMenuEntry" : {
        "type" : "object",
        "properties" : {
          "position" : {
            "type" : "integer",
            "format" : "int32"
          },
          "icon" : {
            "type" : "string"
          },
          "name" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "isDisabled" : {
            "type" : "boolean"
          },
          "openInNew" : {
            "type" : "boolean"
          },
          "isSeparate" : {
            "type" : "boolean"
          },
          "isSeparateBottom" : {
            "type" : "boolean"
          },
          "onlyDesktop" : {
            "type" : "boolean"
          },
          "onlyWeb" : {
            "type" : "boolean"
          },
          "mode" : {
            "type" : "string"
          },
          "scopes" : {
            "type" : "array",
            "items" : {
              "type" : "string",
              "enum" : [ "Render", "Search", "CollectionsReferences", "CollectionsCollection", "WorkspaceList", "WorkspaceTree", "Oer", "CreateMenu" ]
            }
          },
          "ajax" : {
            "type" : "boolean"
          },
          "group" : {
            "type" : "string"
          },
          "permission" : {
            "type" : "string"
          },
          "toolpermission" : {
            "type" : "string"
          },
          "isDirectory" : {
            "type" : "boolean"
          },
          "showAsAction" : {
            "type" : "boolean"
          },
          "multiple" : {
            "type" : "boolean"
          },
          "changeStrategy" : {
            "type" : "string",
            "enum" : [ "update", "remove" ]
          }
        }
      },
      "FontIcon" : {
        "type" : "object",
        "properties" : {
          "original" : {
            "type" : "string"
          },
          "replace" : {
            "type" : "string"
          }
        }
      },
      "Guest" : {
        "type" : "object",
        "properties" : {
          "enabled" : {
            "type" : "boolean"
          }
        }
      },
      "HelpMenuOptions" : {
        "type" : "object",
        "properties" : {
          "key" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          }
        }
      },
      "Icon" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          }
        }
      },
      "Image" : {
        "type" : "object",
        "properties" : {
          "src" : {
            "type" : "string"
          },
          "replace" : {
            "type" : "string"
          }
        }
      },
      "KeyValuePair" : {
        "type" : "object",
        "properties" : {
          "key" : {
            "type" : "string",
            "xml" : {
              "attribute" : true
            }
          },
          "value" : {
            "type" : "string"
          }
        }
      },
      "Language" : {
        "type" : "object",
        "properties" : {
          "global" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "string"
            }
          },
          "current" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "string"
            }
          },
          "currentLanguage" : {
            "type" : "string"
          }
        }
      },
      "LicenseAgreement" : {
        "type" : "object",
        "properties" : {
          "nodeId" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/LicenseAgreementNode"
            }
          }
        }
      },
      "LicenseAgreementNode" : {
        "type" : "object",
        "properties" : {
          "language" : {
            "type" : "string",
            "xml" : {
              "attribute" : true
            }
          },
          "value" : {
            "type" : "string"
          }
        }
      },
      "LogoutInfo" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          },
          "destroySession" : {
            "type" : "boolean"
          },
          "ajax" : {
            "type" : "boolean"
          },
          "next" : {
            "type" : "string"
          }
        }
      },
      "Mainnav" : {
        "type" : "object",
        "properties" : {
          "icon" : {
            "$ref" : "#/components/schemas/Icon"
          },
          "mainMenuStyle" : {
            "type" : "string"
          }
        }
      },
      "MenuEntry" : {
        "type" : "object",
        "properties" : {
          "position" : {
            "type" : "integer",
            "format" : "int32"
          },
          "icon" : {
            "type" : "string"
          },
          "name" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "isDisabled" : {
            "type" : "boolean"
          },
          "openInNew" : {
            "type" : "boolean"
          },
          "isSeparate" : {
            "type" : "boolean"
          },
          "isSeparateBottom" : {
            "type" : "boolean"
          },
          "onlyDesktop" : {
            "type" : "boolean"
          },
          "onlyWeb" : {
            "type" : "boolean"
          },
          "path" : {
            "type" : "string"
          },
          "scope" : {
            "type" : "string"
          }
        }
      },
      "Register" : {
        "type" : "object",
        "properties" : {
          "local" : {
            "type" : "boolean"
          },
          "recoverPassword" : {
            "type" : "boolean"
          },
          "loginUrl" : {
            "type" : "string"
          },
          "recoverUrl" : {
            "type" : "string"
          },
          "requiredFields" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Rendering" : {
        "type" : "object",
        "properties" : {
          "showPreview" : {
            "type" : "boolean"
          },
          "showDownloadButton" : {
            "type" : "boolean"
          },
          "prerender" : {
            "type" : "boolean"
          }
        }
      },
      "Services" : {
        "type" : "object",
        "properties" : {
          "visualization" : {
            "type" : "string"
          }
        }
      },
      "SessionExpiredDialog" : {
        "type" : "object"
      },
      "SimpleEdit" : {
        "type" : "object",
        "properties" : {
          "globalGroups" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/SimpleEditGlobalGroups"
            }
          },
          "organization" : {
            "$ref" : "#/components/schemas/SimpleEditOrganization"
          },
          "organizationFilter" : {
            "type" : "string"
          },
          "licenses" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "SimpleEditGlobalGroups" : {
        "type" : "object",
        "properties" : {
          "toolpermission" : {
            "type" : "string"
          },
          "groups" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "SimpleEditOrganization" : {
        "type" : "object",
        "properties" : {
          "groupTypes" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "Statistics" : {
        "required" : [ "entries" ],
        "type" : "object",
        "properties" : {
          "entries" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/StatisticEntry"
            }
          }
        }
      },
      "Stream" : {
        "type" : "object",
        "properties" : {
          "enabled" : {
            "type" : "boolean"
          }
        }
      },
      "Values" : {
        "type" : "object",
        "properties" : {
          "supportedLanguages" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "extension" : {
            "type" : "string"
          },
          "loginUrl" : {
            "type" : "string"
          },
          "loginAllowLocal" : {
            "type" : "boolean"
          },
          "loginProvidersUrl" : {
            "type" : "string"
          },
          "loginProviderTargetUrl" : {
            "type" : "string"
          },
          "register" : {
            "$ref" : "#/components/schemas/Register"
          },
          "recoverPasswordUrl" : {
            "type" : "string"
          },
          "imprintUrl" : {
            "type" : "string"
          },
          "privacyInformationUrl" : {
            "type" : "string"
          },
          "helpUrl" : {
            "type" : "string"
          },
          "whatsNewUrl" : {
            "type" : "string"
          },
          "editProfileUrl" : {
            "type" : "string"
          },
          "editProfile" : {
            "type" : "boolean"
          },
          "workspaceColumns" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "workspaceSharedToMeDefaultAll" : {
            "type" : "boolean"
          },
          "hideMainMenu" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "logout" : {
            "$ref" : "#/components/schemas/LogoutInfo"
          },
          "menuEntries" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MenuEntry"
            }
          },
          "customOptions" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/ContextMenuEntry"
            }
          },
          "userMenuOverrides" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/ContextMenuEntry"
            }
          },
          "allowedLicenses" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "customLicenses" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/License"
            }
          },
          "workflow" : {
            "$ref" : "#/components/schemas/ConfigWorkflow"
          },
          "licenseDialogOnUpload" : {
            "type" : "boolean"
          },
          "nodeReport" : {
            "type" : "boolean"
          },
          "branding" : {
            "type" : "boolean"
          },
          "rating" : {
            "$ref" : "#/components/schemas/ConfigRating"
          },
          "publishingNotice" : {
            "type" : "boolean"
          },
          "siteTitle" : {
            "type" : "string"
          },
          "userDisplayName" : {
            "type" : "string"
          },
          "userSecondaryDisplayName" : {
            "type" : "string"
          },
          "userAffiliation" : {
            "type" : "boolean"
          },
          "defaultUsername" : {
            "type" : "string"
          },
          "defaultPassword" : {
            "type" : "string"
          },
          "banner" : {
            "$ref" : "#/components/schemas/Banner"
          },
          "availableMds" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/AvailableMds"
            }
          },
          "availableRepositories" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "searchViewType" : {
            "type" : "integer",
            "format" : "int32"
          },
          "workspaceViewType" : {
            "type" : "integer",
            "format" : "int32"
          },
          "itemsPerRequest" : {
            "type" : "integer",
            "format" : "int32"
          },
          "rendering" : {
            "$ref" : "#/components/schemas/Rendering"
          },
          "sessionExpiredDialog" : {
            "$ref" : "#/components/schemas/SessionExpiredDialog"
          },
          "loginDefaultLocation" : {
            "type" : "string"
          },
          "searchGroupResults" : {
            "type" : "boolean"
          },
          "mainnav" : {
            "$ref" : "#/components/schemas/Mainnav"
          },
          "searchSidenavMode" : {
            "type" : "string"
          },
          "guest" : {
            "$ref" : "#/components/schemas/Guest"
          },
          "collections" : {
            "$ref" : "#/components/schemas/Collections"
          },
          "licenseAgreement" : {
            "$ref" : "#/components/schemas/LicenseAgreement"
          },
          "services" : {
            "$ref" : "#/components/schemas/Services"
          },
          "helpMenuOptions" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/HelpMenuOptions"
            }
          },
          "images" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Image"
            }
          },
          "icons" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/FontIcon"
            }
          },
          "stream" : {
            "$ref" : "#/components/schemas/Stream"
          },
          "admin" : {
            "$ref" : "#/components/schemas/Admin"
          },
          "simpleEdit" : {
            "$ref" : "#/components/schemas/SimpleEdit"
          },
          "frontpage" : {
            "$ref" : "#/components/schemas/ConfigFrontpage"
          },
          "upload" : {
            "$ref" : "#/components/schemas/ConfigUpload"
          },
          "publish" : {
            "$ref" : "#/components/schemas/ConfigPublish"
          },
          "remote" : {
            "$ref" : "#/components/schemas/ConfigRemote"
          },
          "customCSS" : {
            "type" : "string"
          },
          "privacy" : {
            "$ref" : "#/components/schemas/ConfigPrivacy"
          }
        }
      },
      "DynamicConfig" : {
        "type" : "object",
        "properties" : {
          "nodeId" : {
            "type" : "string"
          },
          "value" : {
            "type" : "string"
          }
        }
      },
      "Variables" : {
        "type" : "object",
        "properties" : {
          "global" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "string"
            }
          },
          "current" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "string"
            }
          }
        }
      },
      "Connector" : {
        "required" : [ "showNew" ],
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "showNew" : {
            "type" : "boolean",
            "description" : "false"
          },
          "parameters" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "filetypes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/ConnectorFileType"
            }
          },
          "onlyDesktop" : {
            "type" : "boolean"
          },
          "hasViewMode" : {
            "type" : "boolean"
          }
        }
      },
      "ConnectorFileType" : {
        "type" : "object",
        "properties" : {
          "ccressourceversion" : {
            "type" : "string"
          },
          "ccressourcetype" : {
            "type" : "string"
          },
          "ccresourcesubtype" : {
            "type" : "string"
          },
          "editorType" : {
            "type" : "string"
          },
          "mimetype" : {
            "type" : "string"
          },
          "filetype" : {
            "type" : "string"
          },
          "creatable" : {
            "type" : "boolean"
          },
          "editable" : {
            "type" : "boolean"
          }
        }
      },
      "ConnectorList" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          },
          "connectors" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Connector"
            }
          }
        }
      },
      "UserCredential" : {
        "required" : [ "newPassword" ],
        "type" : "object",
        "properties" : {
          "oldPassword" : {
            "type" : "string"
          },
          "newPassword" : {
            "type" : "string"
          }
        }
      },
      "UserProfileEdit" : {
        "type" : "object",
        "properties" : {
          "primaryAffiliation" : {
            "type" : "string"
          },
          "skills" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "types" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "sizeQuota" : {
            "type" : "integer",
            "format" : "int64"
          },
          "vcard" : {
            "type" : "string"
          },
          "type" : {
            "type" : "array",
            "writeOnly" : true,
            "items" : {
              "type" : "string"
            }
          },
          "firstName" : {
            "type" : "string"
          },
          "lastName" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "avatar" : {
            "type" : "string"
          },
          "about" : {
            "type" : "string"
          }
        }
      },
      "User" : {
        "required" : [ "authorityName", "homeFolder" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "editable" : {
            "type" : "boolean"
          },
          "status" : {
            "$ref" : "#/components/schemas/UserStatus"
          },
          "organizations" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Organization"
            }
          },
          "quota" : {
            "$ref" : "#/components/schemas/UserQuota"
          },
          "authorityName" : {
            "type" : "string"
          },
          "authorityType" : {
            "type" : "string",
            "enum" : [ "USER", "GROUP", "OWNER", "EVERYONE", "GUEST" ]
          },
          "userName" : {
            "type" : "string"
          },
          "profile" : {
            "$ref" : "#/components/schemas/UserProfile"
          },
          "homeFolder" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "sharedFolders" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/NodeRef"
            }
          }
        }
      },
      "UserQuota" : {
        "type" : "object",
        "properties" : {
          "enabled" : {
            "type" : "boolean"
          },
          "sizeCurrent" : {
            "type" : "integer",
            "format" : "int64"
          },
          "sizeQuota" : {
            "type" : "integer",
            "format" : "int64"
          }
        }
      },
      "GroupEntry" : {
        "required" : [ "group" ],
        "type" : "object",
        "properties" : {
          "group" : {
            "$ref" : "#/components/schemas/Group"
          }
        }
      },
      "Authority" : {
        "required" : [ "authorityName" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "editable" : {
            "type" : "boolean"
          },
          "authorityName" : {
            "type" : "string"
          },
          "authorityType" : {
            "type" : "string",
            "enum" : [ "USER", "GROUP", "OWNER", "EVERYONE", "GUEST" ]
          }
        }
      },
      "AuthorityEntries" : {
        "required" : [ "authorities", "pagination" ],
        "type" : "object",
        "properties" : {
          "authorities" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Authority"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "NodeEntries" : {
        "required" : [ "nodes", "pagination" ],
        "type" : "object",
        "properties" : {
          "nodes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "Preferences" : {
        "type" : "object",
        "properties" : {
          "preferences" : {
            "type" : "string"
          }
        }
      },
      "ProfileSettings" : {
        "required" : [ "showEmail" ],
        "type" : "object",
        "properties" : {
          "showEmail" : {
            "type" : "boolean",
            "description" : "false"
          }
        }
      },
      "UserEntry" : {
        "required" : [ "person" ],
        "type" : "object",
        "properties" : {
          "editProfile" : {
            "type" : "boolean"
          },
          "person" : {
            "$ref" : "#/components/schemas/User"
          }
        }
      },
      "GroupEntries" : {
        "required" : [ "groups", "pagination" ],
        "type" : "object",
        "properties" : {
          "groups" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Group"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "UserStats" : {
        "type" : "object",
        "properties" : {
          "nodeCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "nodeCountCC" : {
            "type" : "integer",
            "format" : "int32"
          },
          "collectionCount" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "UserEntries" : {
        "required" : [ "pagination", "users" ],
        "type" : "object",
        "properties" : {
          "users" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/UserSimple"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "GroupSignupDetails" : {
        "type" : "object",
        "properties" : {
          "signupMethod" : {
            "type" : "string",
            "enum" : [ "simple", "password", "list" ]
          },
          "signupPassword" : {
            "type" : "string"
          }
        }
      },
      "Job" : {
        "required" : [ "id", "status" ],
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "status" : {
            "type" : "string"
          }
        }
      },
      "JobEntry" : {
        "required" : [ "data" ],
        "type" : "object",
        "properties" : {
          "data" : {
            "$ref" : "#/components/schemas/Job"
          }
        }
      },
      "AuthenticationToken" : {
        "type" : "object",
        "properties" : {
          "userId" : {
            "type" : "string"
          },
          "ticket" : {
            "type" : "string"
          }
        }
      },
      "UserProfileAppAuth" : {
        "type" : "object",
        "properties" : {
          "primaryAffiliation" : {
            "type" : "string"
          },
          "skills" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "types" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "extendedAttributes" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "vcard" : {
            "type" : "string"
          },
          "type" : {
            "type" : "array",
            "writeOnly" : true,
            "items" : {
              "type" : "string"
            }
          },
          "firstName" : {
            "type" : "string"
          },
          "lastName" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "avatar" : {
            "type" : "string"
          },
          "about" : {
            "type" : "string"
          }
        }
      },
      "LTISession" : {
        "type" : "object",
        "properties" : {
          "acceptMultiple" : {
            "type" : "boolean"
          },
          "deeplinkReturnUrl" : {
            "type" : "string"
          },
          "acceptTypes" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "acceptPresentationDocumentTargets" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "canConfirm" : {
            "type" : "boolean"
          },
          "title" : {
            "type" : "string"
          },
          "text" : {
            "type" : "string"
          }
        }
      },
      "Login" : {
        "required" : [ "currentScope", "isAdmin", "isGuest", "isValidLogin", "sessionTimeout" ],
        "type" : "object",
        "properties" : {
          "remoteAuthentications" : {
            "type" : "object",
            "additionalProperties" : {
              "$ref" : "#/components/schemas/RemoteAuthDescription"
            }
          },
          "isValidLogin" : {
            "type" : "boolean"
          },
          "isAdmin" : {
            "type" : "boolean"
          },
          "ltiSession" : {
            "$ref" : "#/components/schemas/LTISession"
          },
          "currentScope" : {
            "type" : "string"
          },
          "userHome" : {
            "type" : "string"
          },
          "sessionTimeout" : {
            "type" : "integer",
            "format" : "int32"
          },
          "toolPermissions" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "statusCode" : {
            "type" : "string"
          },
          "authorityName" : {
            "type" : "string"
          },
          "isGuest" : {
            "type" : "boolean"
          }
        }
      },
      "RemoteAuthDescription" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          },
          "token" : {
            "type" : "string"
          }
        }
      },
      "LoginCredentials" : {
        "required" : [ "password", "scope", "userName" ],
        "type" : "object",
        "properties" : {
          "userName" : {
            "type" : "string"
          },
          "password" : {
            "type" : "string"
          },
          "scope" : {
            "type" : "string"
          }
        }
      },
      "RegistrationUrl" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          }
        }
      },
      "DynamicRegistrationToken" : {
        "type" : "object",
        "properties" : {
          "token" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "registeredAppId" : {
            "type" : "string"
          },
          "tsCreated" : {
            "type" : "integer",
            "format" : "int64"
          },
          "tsExpiry" : {
            "type" : "integer",
            "format" : "int64"
          },
          "valid" : {
            "type" : "boolean"
          }
        }
      },
      "DynamicRegistrationTokens" : {
        "type" : "object",
        "properties" : {
          "registrationLinks" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/DynamicRegistrationToken"
            }
          }
        }
      },
      "Create" : {
        "type" : "object",
        "properties" : {
          "onlyMetadata" : {
            "type" : "boolean"
          }
        }
      },
      "Mds" : {
        "required" : [ "groups", "lists", "name", "sorts", "views", "widgets" ],
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "create" : {
            "$ref" : "#/components/schemas/Create"
          },
          "widgets" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsWidget"
            }
          },
          "views" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsView"
            }
          },
          "groups" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsGroup"
            }
          },
          "lists" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsList"
            }
          },
          "sorts" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsSort"
            }
          }
        }
      },
      "MdsColumn" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "format" : {
            "type" : "string"
          },
          "showDefault" : {
            "type" : "boolean"
          }
        }
      },
      "MdsGroup" : {
        "type" : "object",
        "properties" : {
          "rendering" : {
            "type" : "string",
            "enum" : [ "legacy", "angular" ]
          },
          "id" : {
            "type" : "string"
          },
          "views" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "MdsList" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "columns" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsColumn"
            }
          }
        }
      },
      "MdsSort" : {
        "required" : [ "id" ],
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "columns" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsSortColumn"
            }
          },
          "default" : {
            "$ref" : "#/components/schemas/MdsSortDefault"
          }
        }
      },
      "MdsSortColumn" : {
        "required" : [ "id" ],
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "mode" : {
            "type" : "string"
          }
        }
      },
      "MdsSortDefault" : {
        "required" : [ "sortAscending", "sortBy" ],
        "type" : "object",
        "properties" : {
          "sortBy" : {
            "type" : "string"
          },
          "sortAscending" : {
            "type" : "boolean"
          }
        }
      },
      "MdsSubwidget" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          }
        }
      },
      "MdsValue" : {
        "required" : [ "id" ],
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "caption" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "parent" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "alternativeIds" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "MdsView" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "caption" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "html" : {
            "type" : "string"
          },
          "rel" : {
            "type" : "string",
            "enum" : [ "suggestions" ]
          },
          "hideIfEmpty" : {
            "type" : "boolean"
          },
          "isExtended" : {
            "type" : "boolean"
          }
        }
      },
      "MdsWidget" : {
        "type" : "object",
        "properties" : {
          "link" : {
            "type" : "string"
          },
          "configuration" : {
            "type" : "string"
          },
          "format" : {
            "type" : "string"
          },
          "allowValuespaceSuggestions" : {
            "type" : "boolean"
          },
          "condition" : {
            "$ref" : "#/components/schemas/MdsWidgetCondition"
          },
          "maxlength" : {
            "type" : "integer",
            "format" : "int32"
          },
          "interactionType" : {
            "type" : "string",
            "enum" : [ "Input", "None" ]
          },
          "subwidgets" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsSubwidget"
            }
          },
          "required" : {
            "type" : "string",
            "writeOnly" : true,
            "enum" : [ "mandatory", "mandatoryForPublish", "recommended", "optional", "ignore" ]
          },
          "id" : {
            "type" : "string"
          },
          "caption" : {
            "type" : "string"
          },
          "bottomCaption" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          },
          "template" : {
            "type" : "string"
          },
          "hasValues" : {
            "type" : "boolean"
          },
          "values" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsValue"
            }
          },
          "placeholder" : {
            "type" : "string"
          },
          "unit" : {
            "type" : "string"
          },
          "min" : {
            "type" : "integer",
            "format" : "int32"
          },
          "max" : {
            "type" : "integer",
            "format" : "int32"
          },
          "defaultMin" : {
            "type" : "integer",
            "format" : "int32"
          },
          "defaultMax" : {
            "type" : "integer",
            "format" : "int32"
          },
          "step" : {
            "type" : "integer",
            "format" : "int32"
          },
          "isRequired" : {
            "type" : "string",
            "enum" : [ "mandatory", "mandatoryForPublish", "recommended", "optional", "ignore" ]
          },
          "allowempty" : {
            "type" : "boolean"
          },
          "defaultvalue" : {
            "type" : "string"
          },
          "isSearchable" : {
            "type" : "boolean"
          },
          "isExtended" : {
            "type" : "boolean"
          },
          "hideIfEmpty" : {
            "type" : "boolean"
          }
        }
      },
      "MdsWidgetCondition" : {
        "required" : [ "dynamic", "negate", "type", "value" ],
        "type" : "object",
        "properties" : {
          "type" : {
            "type" : "string",
            "enum" : [ "PROPERTY", "TOOLPERMISSION" ]
          },
          "value" : {
            "type" : "string"
          },
          "negate" : {
            "type" : "boolean"
          },
          "dynamic" : {
            "type" : "boolean"
          },
          "pattern" : {
            "type" : "string"
          }
        }
      },
      "MdsEntries" : {
        "required" : [ "metadatasets" ],
        "type" : "object",
        "properties" : {
          "metadatasets" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MetadataSetInfo"
            }
          }
        }
      },
      "MetadataSetInfo" : {
        "required" : [ "id", "name" ],
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "name" : {
            "type" : "string"
          }
        }
      },
      "MdsQueryCriteria" : {
        "required" : [ "property", "values" ],
        "type" : "object",
        "properties" : {
          "property" : {
            "type" : "string"
          },
          "values" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "SuggestionParam" : {
        "type" : "object",
        "properties" : {
          "valueParameters" : {
            "$ref" : "#/components/schemas/ValueParameters"
          },
          "criteria" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsQueryCriteria"
            }
          }
        }
      },
      "ValueParameters" : {
        "required" : [ "pattern", "property", "query" ],
        "type" : "object",
        "properties" : {
          "query" : {
            "type" : "string"
          },
          "property" : {
            "type" : "string"
          },
          "pattern" : {
            "type" : "string",
            "description" : "prefix of the value (or \"-all-\" for all values)"
          }
        }
      },
      "Suggestion" : {
        "required" : [ "displayString", "replacementString" ],
        "type" : "object",
        "properties" : {
          "replacementString" : {
            "type" : "string"
          },
          "displayString" : {
            "type" : "string"
          },
          "key" : {
            "type" : "string"
          }
        }
      },
      "Suggestions" : {
        "required" : [ "values" ],
        "type" : "object",
        "properties" : {
          "values" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Suggestion"
            }
          }
        }
      },
      "Mediacenter" : {
        "required" : [ "authorityName" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "editable" : {
            "type" : "boolean"
          },
          "signupMethod" : {
            "type" : "string",
            "enum" : [ "simple", "password", "list" ]
          },
          "ref" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "aspects" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "organizations" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Organization"
            }
          },
          "authorityName" : {
            "type" : "string"
          },
          "authorityType" : {
            "type" : "string",
            "enum" : [ "USER", "GROUP", "OWNER", "EVERYONE", "GUEST" ]
          },
          "groupName" : {
            "type" : "string"
          },
          "profile" : {
            "$ref" : "#/components/schemas/GroupProfile"
          },
          "administrationAccess" : {
            "type" : "boolean"
          }
        }
      },
      "Catalog" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          }
        }
      },
      "MediacenterProfileExtension" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "location" : {
            "type" : "string"
          },
          "districtAbbreviation" : {
            "type" : "string"
          },
          "mainUrl" : {
            "type" : "string"
          },
          "catalogs" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Catalog"
            }
          },
          "contentStatus" : {
            "type" : "string",
            "enum" : [ "Activated", "Deactivated" ]
          }
        }
      },
      "Profile" : {
        "type" : "object",
        "properties" : {
          "groupEmail" : {
            "type" : "string"
          },
          "mediacenter" : {
            "$ref" : "#/components/schemas/MediacenterProfileExtension"
          },
          "displayName" : {
            "type" : "string"
          },
          "groupType" : {
            "type" : "string"
          },
          "scopeType" : {
            "type" : "string"
          }
        }
      },
      "SearchParameters" : {
        "required" : [ "criteria" ],
        "type" : "object",
        "properties" : {
          "permissions" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "resolveCollections" : {
            "type" : "boolean"
          },
          "returnSuggestions" : {
            "type" : "boolean"
          },
          "facets" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "facetMinCount" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 5
          },
          "facetLimit" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          },
          "facetSuggest" : {
            "type" : "string"
          },
          "criteria" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsQueryCriteria"
            }
          }
        }
      },
      "McOrgConnectResult" : {
        "type" : "object",
        "properties" : {
          "rows" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "MediacentersImportResult" : {
        "type" : "object",
        "properties" : {
          "rows" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "OrganisationsImportResult" : {
        "type" : "object",
        "properties" : {
          "rows" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "Audience" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          }
        }
      },
      "Geo" : {
        "type" : "object",
        "properties" : {
          "longitude" : {
            "type" : "number",
            "format" : "double"
          },
          "latitude" : {
            "type" : "number",
            "format" : "double"
          },
          "addressCountry" : {
            "type" : "string"
          }
        }
      },
      "Interface" : {
        "type" : "object",
        "properties" : {
          "url" : {
            "type" : "string"
          },
          "set" : {
            "type" : "string"
          },
          "metadataPrefix" : {
            "type" : "string"
          },
          "documentation" : {
            "type" : "string"
          },
          "format" : {
            "type" : "string",
            "enum" : [ "Json", "XML", "Text" ]
          },
          "type" : {
            "type" : "string",
            "enum" : [ "Search", "Sitemap", "Statistics", "OAI", "Generic_Api" ]
          }
        }
      },
      "Location" : {
        "type" : "object",
        "properties" : {
          "geo" : {
            "$ref" : "#/components/schemas/Geo"
          }
        }
      },
      "Provider" : {
        "type" : "object",
        "properties" : {
          "legalName" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "areaServed" : {
            "type" : "string",
            "enum" : [ "Organization", "City", "State", "Country", "Continent", "World" ]
          },
          "location" : {
            "$ref" : "#/components/schemas/Location"
          }
        }
      },
      "StoredService" : {
        "type" : "object",
        "properties" : {
          "name" : {
            "type" : "string"
          },
          "url" : {
            "type" : "string"
          },
          "icon" : {
            "type" : "string"
          },
          "logo" : {
            "type" : "string"
          },
          "inLanguage" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "audience" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Audience"
            }
          },
          "provider" : {
            "$ref" : "#/components/schemas/Provider"
          },
          "startDate" : {
            "type" : "string"
          },
          "interfaces" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Interface"
            }
          },
          "about" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "id" : {
            "type" : "string"
          },
          "isAccessibleForFree" : {
            "type" : "boolean"
          }
        }
      },
      "RepoEntries" : {
        "required" : [ "repositories" ],
        "type" : "object",
        "properties" : {
          "repositories" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Repo"
            }
          }
        }
      },
      "WorkflowHistory" : {
        "type" : "object",
        "properties" : {
          "time" : {
            "type" : "integer",
            "format" : "int64"
          },
          "editor" : {
            "$ref" : "#/components/schemas/UserSimple"
          },
          "receiver" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Authority"
            }
          },
          "status" : {
            "type" : "string"
          },
          "comment" : {
            "type" : "string"
          }
        }
      },
      "NodeShare" : {
        "type" : "object",
        "properties" : {
          "password" : {
            "type" : "boolean"
          },
          "token" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "expiryDate" : {
            "type" : "integer",
            "format" : "int64"
          },
          "invitedAt" : {
            "type" : "integer",
            "format" : "int64"
          },
          "downloadCount" : {
            "type" : "integer",
            "format" : "int32"
          },
          "url" : {
            "type" : "string"
          },
          "shareId" : {
            "type" : "string"
          }
        }
      },
      "ACE" : {
        "required" : [ "authority", "permissions" ],
        "type" : "object",
        "properties" : {
          "editable" : {
            "type" : "boolean"
          },
          "authority" : {
            "$ref" : "#/components/schemas/Authority"
          },
          "user" : {
            "$ref" : "#/components/schemas/UserProfile"
          },
          "group" : {
            "$ref" : "#/components/schemas/GroupProfile"
          },
          "permissions" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "ACL" : {
        "required" : [ "inherited", "permissions" ],
        "type" : "object",
        "properties" : {
          "inherited" : {
            "type" : "boolean"
          },
          "permissions" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/ACE"
            }
          }
        }
      },
      "NotifyEntry" : {
        "required" : [ "action", "date", "permissions", "user" ],
        "type" : "object",
        "properties" : {
          "date" : {
            "type" : "integer",
            "format" : "int64"
          },
          "permissions" : {
            "$ref" : "#/components/schemas/ACL"
          },
          "user" : {
            "$ref" : "#/components/schemas/User"
          },
          "action" : {
            "type" : "string"
          }
        }
      },
      "ParentEntries" : {
        "required" : [ "nodes", "pagination" ],
        "type" : "object",
        "properties" : {
          "scope" : {
            "type" : "string"
          },
          "nodes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "NodePermissionEntry" : {
        "required" : [ "permissions" ],
        "type" : "object",
        "properties" : {
          "permissions" : {
            "$ref" : "#/components/schemas/NodePermissions"
          }
        }
      },
      "NodePermissions" : {
        "required" : [ "inheritedPermissions", "localPermissions" ],
        "type" : "object",
        "properties" : {
          "localPermissions" : {
            "$ref" : "#/components/schemas/ACL"
          },
          "inheritedPermissions" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/ACE"
            }
          }
        }
      },
      "NodeText" : {
        "type" : "object",
        "properties" : {
          "text" : {
            "type" : "string"
          },
          "html" : {
            "type" : "string"
          },
          "raw" : {
            "type" : "string"
          }
        }
      },
      "NodeVersion" : {
        "required" : [ "comment", "modifiedAt", "modifiedBy", "version" ],
        "type" : "object",
        "properties" : {
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "array",
              "items" : {
                "type" : "string"
              }
            }
          },
          "version" : {
            "$ref" : "#/components/schemas/NodeVersionRef"
          },
          "comment" : {
            "type" : "string"
          },
          "modifiedAt" : {
            "type" : "string"
          },
          "modifiedBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "contentUrl" : {
            "type" : "string"
          }
        }
      },
      "NodeVersionEntry" : {
        "required" : [ "version" ],
        "type" : "object",
        "properties" : {
          "version" : {
            "$ref" : "#/components/schemas/NodeVersion"
          }
        }
      },
      "NodeVersionRef" : {
        "required" : [ "major", "minor", "node" ],
        "type" : "object",
        "properties" : {
          "node" : {
            "$ref" : "#/components/schemas/NodeRef"
          },
          "major" : {
            "type" : "integer",
            "format" : "int32"
          },
          "minor" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "NodeVersionRefEntries" : {
        "required" : [ "versions" ],
        "type" : "object",
        "properties" : {
          "versions" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/NodeVersionRef"
            }
          }
        }
      },
      "NodeLocked" : {
        "required" : [ "isLocked" ],
        "type" : "object",
        "properties" : {
          "isLocked" : {
            "type" : "boolean"
          }
        }
      },
      "NodeRemote" : {
        "required" : [ "node", "remote" ],
        "type" : "object",
        "properties" : {
          "node" : {
            "$ref" : "#/components/schemas/Node"
          },
          "remote" : {
            "$ref" : "#/components/schemas/Node"
          }
        }
      },
      "OrganizationEntries" : {
        "required" : [ "organizations", "pagination" ],
        "type" : "object",
        "properties" : {
          "organizations" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Organization"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "canCreate" : {
            "type" : "boolean"
          }
        }
      },
      "RegisterExists" : {
        "type" : "object",
        "properties" : {
          "exists" : {
            "type" : "boolean"
          }
        }
      },
      "RegisterInformation" : {
        "type" : "object",
        "properties" : {
          "firstName" : {
            "type" : "string"
          },
          "lastName" : {
            "type" : "string"
          },
          "email" : {
            "type" : "string"
          },
          "password" : {
            "type" : "string"
          },
          "organization" : {
            "type" : "string"
          },
          "allowNotifications" : {
            "type" : "boolean"
          },
          "authorityName" : {
            "type" : "string"
          }
        }
      },
      "RenderingDetailsEntry" : {
        "required" : [ "detailsSnippet", "mimeType", "node" ],
        "type" : "object",
        "properties" : {
          "detailsSnippet" : {
            "type" : "string"
          },
          "mimeType" : {
            "type" : "string"
          },
          "node" : {
            "$ref" : "#/components/schemas/Node"
          }
        }
      },
      "SearchResultNode" : {
        "required" : [ "facets", "nodes", "pagination" ],
        "type" : "object",
        "properties" : {
          "suggests" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Suggest"
            }
          },
          "nodes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          },
          "facets" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Facet"
            }
          },
          "ignored" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "SearchVCard" : {
        "type" : "object",
        "properties" : {
          "vcard" : {
            "type" : "string"
          }
        }
      },
      "SearchParametersFacets" : {
        "required" : [ "criteria", "facets" ],
        "type" : "object",
        "properties" : {
          "facets" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "facetMinCount" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 5
          },
          "facetLimit" : {
            "type" : "integer",
            "format" : "int32",
            "default" : 10
          },
          "facetSuggest" : {
            "type" : "string"
          },
          "criteria" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/MdsQueryCriteria"
            }
          }
        }
      },
      "SharingInfo" : {
        "type" : "object",
        "properties" : {
          "passwordMatches" : {
            "type" : "boolean"
          },
          "password" : {
            "type" : "boolean"
          },
          "expired" : {
            "type" : "boolean"
          },
          "invitedBy" : {
            "$ref" : "#/components/schemas/Person"
          },
          "node" : {
            "$ref" : "#/components/schemas/Node"
          }
        }
      },
      "StatisticEntity" : {
        "required" : [ "count", "value" ],
        "type" : "object",
        "properties" : {
          "value" : {
            "type" : "string"
          },
          "count" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "StatisticEntry" : {
        "required" : [ "entities", "property" ],
        "type" : "object",
        "properties" : {
          "property" : {
            "type" : "string"
          },
          "entities" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/StatisticEntity"
            }
          }
        }
      },
      "Filter" : {
        "required" : [ "entries" ],
        "type" : "object",
        "properties" : {
          "entries" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/FilterEntry"
            }
          }
        }
      },
      "FilterEntry" : {
        "required" : [ "property", "values" ],
        "type" : "object",
        "properties" : {
          "property" : {
            "type" : "string"
          },
          "values" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          }
        }
      },
      "StatisticsGlobal" : {
        "type" : "object",
        "properties" : {
          "overall" : {
            "$ref" : "#/components/schemas/StatisticsGroup"
          },
          "groups" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/StatisticsKeyGroup"
            }
          },
          "user" : {
            "$ref" : "#/components/schemas/StatisticsUser"
          }
        }
      },
      "StatisticsGroup" : {
        "type" : "object",
        "properties" : {
          "count" : {
            "type" : "integer",
            "format" : "int32"
          },
          "subGroups" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/StatisticsSubGroup"
            }
          }
        }
      },
      "StatisticsKeyGroup" : {
        "type" : "object",
        "properties" : {
          "key" : {
            "type" : "string"
          },
          "displayName" : {
            "type" : "string"
          },
          "count" : {
            "type" : "integer",
            "format" : "int32"
          },
          "subGroups" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/StatisticsSubGroup"
            }
          }
        }
      },
      "StatisticsSubGroup" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "count" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/SubGroupItem"
            }
          }
        }
      },
      "StatisticsUser" : {
        "type" : "object",
        "properties" : {
          "count" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "SubGroupItem" : {
        "type" : "object",
        "properties" : {
          "key" : {
            "type" : "string"
          },
          "displayName" : {
            "type" : "string"
          },
          "count" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "NodeData" : {
        "type" : "object",
        "properties" : {
          "timestamp" : {
            "type" : "string"
          },
          "counts" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "integer",
              "format" : "int32"
            }
          }
        }
      },
      "TrackingAuthority" : {
        "type" : "object",
        "properties" : {
          "hash" : {
            "type" : "string"
          },
          "organization" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Organization"
            }
          },
          "mediacenter" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Group"
            }
          }
        }
      },
      "TrackingNode" : {
        "type" : "object",
        "properties" : {
          "counts" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "integer",
              "format" : "int32"
            }
          },
          "date" : {
            "type" : "string"
          },
          "fields" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object"
            }
          },
          "groups" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object",
              "additionalProperties" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "integer",
                  "format" : "int64"
                }
              }
            }
          },
          "node" : {
            "$ref" : "#/components/schemas/Node"
          },
          "authority" : {
            "$ref" : "#/components/schemas/TrackingAuthority"
          }
        }
      },
      "Tracking" : {
        "type" : "object",
        "properties" : {
          "counts" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "integer",
              "format" : "int32"
            }
          },
          "date" : {
            "type" : "string"
          },
          "fields" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object"
            }
          },
          "groups" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object",
              "additionalProperties" : {
                "type" : "object",
                "additionalProperties" : {
                  "type" : "integer",
                  "format" : "int64"
                }
              }
            }
          },
          "authority" : {
            "$ref" : "#/components/schemas/TrackingAuthority"
          }
        }
      },
      "StreamEntryInput" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "title" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "nodes" : {
            "type" : "array",
            "items" : {
              "type" : "string"
            }
          },
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object"
            }
          },
          "priority" : {
            "type" : "integer",
            "format" : "int32"
          }
        }
      },
      "StreamEntry" : {
        "type" : "object",
        "properties" : {
          "id" : {
            "type" : "string"
          },
          "description" : {
            "type" : "string"
          },
          "nodes" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Node"
            }
          },
          "properties" : {
            "type" : "object",
            "additionalProperties" : {
              "type" : "object"
            }
          },
          "priority" : {
            "type" : "integer",
            "format" : "int32"
          },
          "author" : {
            "$ref" : "#/components/schemas/UserSimple"
          },
          "created" : {
            "type" : "integer",
            "format" : "int64"
          },
          "modified" : {
            "type" : "integer",
            "format" : "int64"
          }
        }
      },
      "StreamList" : {
        "type" : "object",
        "properties" : {
          "stream" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/StreamEntry"
            }
          },
          "pagination" : {
            "$ref" : "#/components/schemas/Pagination"
          }
        }
      },
      "General" : {
        "type" : "object",
        "properties" : {
          "referencedInName" : {
            "type" : "string"
          },
          "referencedInType" : {
            "type" : "string"
          },
          "referencedInInstance" : {
            "type" : "string"
          }
        }
      },
      "Parameters" : {
        "type" : "object",
        "properties" : {
          "general" : {
            "$ref" : "#/components/schemas/General"
          }
        },
        "xml" : {
          "name" : "usage"
        }
      },
      "Usage" : {
        "required" : [ "appId", "appUser", "appUserMail", "courseId", "nodeId", "parentNodeId", "resourceId", "usageVersion" ],
        "type" : "object",
        "properties" : {
          "fromUsed" : {
            "type" : "string",
            "format" : "date-time"
          },
          "toUsed" : {
            "type" : "string",
            "format" : "date-time"
          },
          "usageCounter" : {
            "type" : "integer",
            "format" : "int32"
          },
          "appSubtype" : {
            "type" : "string"
          },
          "appType" : {
            "type" : "string"
          },
          "type" : {
            "type" : "string"
          },
          "created" : {
            "type" : "string",
            "format" : "date-time"
          },
          "modified" : {
            "type" : "string",
            "format" : "date-time"
          },
          "appUser" : {
            "type" : "string"
          },
          "appUserMail" : {
            "type" : "string"
          },
          "courseId" : {
            "type" : "string"
          },
          "distinctPersons" : {
            "type" : "integer",
            "format" : "int32"
          },
          "appId" : {
            "type" : "string"
          },
          "nodeId" : {
            "type" : "string"
          },
          "parentNodeId" : {
            "type" : "string"
          },
          "usageVersion" : {
            "type" : "string"
          },
          "usageXmlParams" : {
            "$ref" : "#/components/schemas/Parameters"
          },
          "usageXmlParamsRaw" : {
            "type" : "string"
          },
          "resourceId" : {
            "type" : "string"
          },
          "guid" : {
            "type" : "string"
          }
        }
      },
      "Usages" : {
        "type" : "object",
        "properties" : {
          "usages" : {
            "type" : "array",
            "items" : {
              "$ref" : "#/components/schemas/Usage"
            }
          }
        }
      },
      "CreateUsage" : {
        "type" : "object",
        "properties" : {
          "appId" : {
            "type" : "string"
          },
          "courseId" : {
            "type" : "string"
          },
          "resourceId" : {
            "type" : "string"
          },
          "nodeId" : {
            "type" : "string"
          },
          "nodeVersion" : {
            "type" : "string"
          }
        }
      }
    }
  }
}