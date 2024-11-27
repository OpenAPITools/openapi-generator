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

    #' @description
    #' Initialize a new TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter class.
    #'
    #' @param values values
    #' @param ... Other optional arguments.
    initialize = function(`values` = NULL, ...) {
      if (!is.null(`values`)) {
        stopifnot(is.vector(`values`), length(`values`) != 0)
        sapply(`values`, function(x) stopifnot(is.character(x)))
        self$`values` <- `values`
      }
    },

    #' @description
    #' Convert to an R object. This method is deprecated. Use `toSimpleType()` instead.
    toJSON = function() {
      .Deprecated(new = "toSimpleType", msg = "Use the '$toSimpleType()' method instead since that is more clearly named. Use '$toJSONString()' to get a JSON string")
      return(self$toSimpleType())
    },

    #' @description
    #' Convert to a List
    #'
    #' Convert the R6 object to a list to work more easily with other tooling.
    #'
    #' @return TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter as a base R list.
    #' @examples
    #' # convert array of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter (x) to a data frame
    #' \dontrun{
    #' library(purrr)
    #' library(tibble)
    #' df <- x |> map(\(y)y$toList()) |> map(as_tibble) |> list_rbind()
    #' df
    #' }
    toList = function() {
      return(self$toSimpleType())
    },

    #' @description
    #' Convert TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterObject <- list()
      if (!is.null(self$`values`)) {
        TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterObject[["values"]] <-
          self$`values`
      }
      return(TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameterObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`values`)) {
        self$`values` <- ApiClient$new()$deserializeObj(this_object$`values`, "array[character]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`values` <- ApiClient$new()$deserializeObj(this_object$`values`, "array[character]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
      invalid_fields
    },

    #' @description
    #' Print the object
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

