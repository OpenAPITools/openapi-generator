#' Create a new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
#'
#' @description
#' TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter Class
#'
#' @docType class
#' @title TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
#' @description TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter Class
#' @format An \code{R6Class} generator object
#' @field values  list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter <- R6::R6Class(
  "TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter",
  public = list(
    `values` = NULL,
    #' Initialize a new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter class.
    #'
    #' @description
    #' Initialize a new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter class.
    #'
    #' @param values values
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`values` = NULL, ...) {
      if (!is.null(`values`)) {
        stopifnot(is.vector(`values`), length(`values`) != 0)
        sapply(`values`, function(x) stopifnot(is.character(x)))
        self$`values` <- `values`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter in JSON format
    #' @export
    toJSON = function() {
      TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterObject <- list()
      if (!is.null(self$`values`)) {
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterObject[["values"]] <-
          self$`values`
      }
      TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterObject
    },
    #' Deserialize JSON string into an instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @description
    #' Deserialize JSON string into an instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`values`)) {
        self$`values` <- ApiClient$new()$deserializeObj(this_object$`values`, "array[character]", loadNamespace("openapi"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`values`)) {
          sprintf(
          '"values":
             [%s]
          ',
          paste(unlist(lapply(self$`values`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @description
    #' Deserialize JSON string into an instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`values` <- ApiClient$new()$deserializeObj(this_object$`values`, "array[character]", loadNamespace("openapi"))
      self
    },
    #' Validate JSON input with respect to TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @description
    #' Validate JSON input with respect to TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter and throw an exception if invalid
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
    #' @return String representation of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
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
# TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter$unlock()
#
## Below is an example to define the print function
# TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter$lock()

