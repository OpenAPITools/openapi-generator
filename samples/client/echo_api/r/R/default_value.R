#' Create a new DefaultValue
#'
#' @description
#' to test the default value of properties
#'
#' @docType class
#' @title DefaultValue
#' @description DefaultValue Class
#' @format An \code{R6Class} generator object
#' @field array_string_enum_ref_default  list(\link{StringEnumRef}) [optional]
#' @field array_string_enum_default  list(character) [optional]
#' @field array_string_default  list(character) [optional]
#' @field array_integer_default  list(integer) [optional]
#' @field array_string  list(character) [optional]
#' @field array_string_nullable  list(character) [optional]
#' @field array_string_extension_nullable  list(character) [optional]
#' @field string_nullable  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DefaultValue <- R6::R6Class(
  "DefaultValue",
  public = list(
    `array_string_enum_ref_default` = NULL,
    `array_string_enum_default` = NULL,
    `array_string_default` = NULL,
    `array_integer_default` = NULL,
    `array_string` = NULL,
    `array_string_nullable` = NULL,
    `array_string_extension_nullable` = NULL,
    `string_nullable` = NULL,
    #' Initialize a new DefaultValue class.
    #'
    #' @description
    #' Initialize a new DefaultValue class.
    #'
    #' @param array_string_enum_ref_default array_string_enum_ref_default. Default to ["success","failure"].
    #' @param array_string_enum_default array_string_enum_default. Default to ["success","failure"].
    #' @param array_string_default array_string_default. Default to ["failure","skipped"].
    #' @param array_integer_default array_integer_default. Default to [1,3].
    #' @param array_string array_string
    #' @param array_string_nullable array_string_nullable
    #' @param array_string_extension_nullable array_string_extension_nullable
    #' @param string_nullable string_nullable
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`array_string_enum_ref_default` = ["success","failure"], `array_string_enum_default` = ["success","failure"], `array_string_default` = ["failure","skipped"], `array_integer_default` = [1,3], `array_string` = NULL, `array_string_nullable` = NULL, `array_string_extension_nullable` = NULL, `string_nullable` = NULL, ...) {
      if (!is.null(`array_string_enum_ref_default`)) {
        stopifnot(is.vector(`array_string_enum_ref_default`), length(`array_string_enum_ref_default`) != 0)
        sapply(`array_string_enum_ref_default`, function(x) stopifnot(R6::is.R6(x)))
        self$`array_string_enum_ref_default` <- `array_string_enum_ref_default`
      }
      if (!is.null(`array_string_enum_default`)) {
        stopifnot(is.vector(`array_string_enum_default`), length(`array_string_enum_default`) != 0)
        sapply(`array_string_enum_default`, function(x) stopifnot(is.character(x)))
        self$`array_string_enum_default` <- `array_string_enum_default`
      }
      if (!is.null(`array_string_default`)) {
        stopifnot(is.vector(`array_string_default`), length(`array_string_default`) != 0)
        sapply(`array_string_default`, function(x) stopifnot(is.character(x)))
        self$`array_string_default` <- `array_string_default`
      }
      if (!is.null(`array_integer_default`)) {
        stopifnot(is.vector(`array_integer_default`), length(`array_integer_default`) != 0)
        sapply(`array_integer_default`, function(x) stopifnot(is.character(x)))
        self$`array_integer_default` <- `array_integer_default`
      }
      if (!is.null(`array_string`)) {
        stopifnot(is.vector(`array_string`), length(`array_string`) != 0)
        sapply(`array_string`, function(x) stopifnot(is.character(x)))
        self$`array_string` <- `array_string`
      }
      if (!is.null(`array_string_nullable`)) {
        stopifnot(is.vector(`array_string_nullable`), length(`array_string_nullable`) != 0)
        sapply(`array_string_nullable`, function(x) stopifnot(is.character(x)))
        self$`array_string_nullable` <- `array_string_nullable`
      }
      if (!is.null(`array_string_extension_nullable`)) {
        stopifnot(is.vector(`array_string_extension_nullable`), length(`array_string_extension_nullable`) != 0)
        sapply(`array_string_extension_nullable`, function(x) stopifnot(is.character(x)))
        self$`array_string_extension_nullable` <- `array_string_extension_nullable`
      }
      if (!is.null(`string_nullable`)) {
        if (!(is.character(`string_nullable`) && length(`string_nullable`) == 1)) {
          stop(paste("Error! Invalid data for `string_nullable`. Must be a string:", `string_nullable`))
        }
        self$`string_nullable` <- `string_nullable`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DefaultValue in JSON format
    #' @export
    toJSON = function() {
      DefaultValueObject <- list()
      if (!is.null(self$`array_string_enum_ref_default`)) {
        DefaultValueObject[["array_string_enum_ref_default"]] <-
          lapply(self$`array_string_enum_ref_default`, function(x) x$toJSON())
      }
      if (!is.null(self$`array_string_enum_default`)) {
        DefaultValueObject[["array_string_enum_default"]] <-
          self$`array_string_enum_default`
      }
      if (!is.null(self$`array_string_default`)) {
        DefaultValueObject[["array_string_default"]] <-
          self$`array_string_default`
      }
      if (!is.null(self$`array_integer_default`)) {
        DefaultValueObject[["array_integer_default"]] <-
          self$`array_integer_default`
      }
      if (!is.null(self$`array_string`)) {
        DefaultValueObject[["array_string"]] <-
          self$`array_string`
      }
      if (!is.null(self$`array_string_nullable`)) {
        DefaultValueObject[["array_string_nullable"]] <-
          self$`array_string_nullable`
      }
      if (!is.null(self$`array_string_extension_nullable`)) {
        DefaultValueObject[["array_string_extension_nullable"]] <-
          self$`array_string_extension_nullable`
      }
      if (!is.null(self$`string_nullable`)) {
        DefaultValueObject[["string_nullable"]] <-
          self$`string_nullable`
      }
      DefaultValueObject
    },
    #' Deserialize JSON string into an instance of DefaultValue
    #'
    #' @description
    #' Deserialize JSON string into an instance of DefaultValue
    #'
    #' @param input_json the JSON input
    #' @return the instance of DefaultValue
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`array_string_enum_ref_default`)) {
        self$`array_string_enum_ref_default` <- ApiClient$new()$deserializeObj(this_object$`array_string_enum_ref_default`, "array[StringEnumRef]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`array_string_enum_default`)) {
        self$`array_string_enum_default` <- ApiClient$new()$deserializeObj(this_object$`array_string_enum_default`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`array_string_default`)) {
        self$`array_string_default` <- ApiClient$new()$deserializeObj(this_object$`array_string_default`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`array_integer_default`)) {
        self$`array_integer_default` <- ApiClient$new()$deserializeObj(this_object$`array_integer_default`, "array[integer]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`array_string`)) {
        self$`array_string` <- ApiClient$new()$deserializeObj(this_object$`array_string`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`array_string_nullable`)) {
        self$`array_string_nullable` <- ApiClient$new()$deserializeObj(this_object$`array_string_nullable`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`array_string_extension_nullable`)) {
        self$`array_string_extension_nullable` <- ApiClient$new()$deserializeObj(this_object$`array_string_extension_nullable`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`string_nullable`)) {
        self$`string_nullable` <- this_object$`string_nullable`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DefaultValue in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`array_string_enum_ref_default`)) {
          sprintf(
          '"array_string_enum_ref_default":
          [%s]
',
          paste(sapply(self$`array_string_enum_ref_default`, function(x) jsonlite::toJSON(x$toJSON(), auto_unbox = TRUE, digits = NA)), collapse = ",")
          )
        },
        if (!is.null(self$`array_string_enum_default`)) {
          sprintf(
          '"array_string_enum_default":
             [%s]
          ',
          paste(unlist(lapply(self$`array_string_enum_default`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`array_string_default`)) {
          sprintf(
          '"array_string_default":
             [%s]
          ',
          paste(unlist(lapply(self$`array_string_default`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`array_integer_default`)) {
          sprintf(
          '"array_integer_default":
             [%s]
          ',
          paste(unlist(lapply(self$`array_integer_default`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`array_string`)) {
          sprintf(
          '"array_string":
             [%s]
          ',
          paste(unlist(lapply(self$`array_string`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`array_string_nullable`)) {
          sprintf(
          '"array_string_nullable":
             [%s]
          ',
          paste(unlist(lapply(self$`array_string_nullable`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`array_string_extension_nullable`)) {
          sprintf(
          '"array_string_extension_nullable":
             [%s]
          ',
          paste(unlist(lapply(self$`array_string_extension_nullable`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`string_nullable`)) {
          sprintf(
          '"string_nullable":
            "%s"
                    ',
          self$`string_nullable`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DefaultValue
    #'
    #' @description
    #' Deserialize JSON string into an instance of DefaultValue
    #'
    #' @param input_json the JSON input
    #' @return the instance of DefaultValue
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`array_string_enum_ref_default` <- ApiClient$new()$deserializeObj(this_object$`array_string_enum_ref_default`, "array[StringEnumRef]", loadNamespace("openapi"))
      self$`array_string_enum_default` <- ApiClient$new()$deserializeObj(this_object$`array_string_enum_default`, "array[character]", loadNamespace("openapi"))
      self$`array_string_default` <- ApiClient$new()$deserializeObj(this_object$`array_string_default`, "array[character]", loadNamespace("openapi"))
      self$`array_integer_default` <- ApiClient$new()$deserializeObj(this_object$`array_integer_default`, "array[integer]", loadNamespace("openapi"))
      self$`array_string` <- ApiClient$new()$deserializeObj(this_object$`array_string`, "array[character]", loadNamespace("openapi"))
      self$`array_string_nullable` <- ApiClient$new()$deserializeObj(this_object$`array_string_nullable`, "array[character]", loadNamespace("openapi"))
      self$`array_string_extension_nullable` <- ApiClient$new()$deserializeObj(this_object$`array_string_extension_nullable`, "array[character]", loadNamespace("openapi"))
      self$`string_nullable` <- this_object$`string_nullable`
      self
    },
    #' Validate JSON input with respect to DefaultValue
    #'
    #' @description
    #' Validate JSON input with respect to DefaultValue and throw an exception if invalid
    #'
    #' @param input the JSON input
    #' @export
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of DefaultValue
    #' @export
    toString = function() {
      self$toJSONString()
    },
    #' Return true if the values in all fields are valid.
    #'
    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    #' @export
    isValid = function() {
      TRUE
    },
    #' Return a list of invalid fields (if any).
    #'
    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    #' @export
    getInvalidFields = function() {
      invalid_fields <- list()
      invalid_fields
    },
    #' Print the object
    #'
    #' @description
    #' Print the object
    #'
    #' @export
    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# DefaultValue$unlock()
#
## Below is an example to define the print function
# DefaultValue$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# DefaultValue$lock()

