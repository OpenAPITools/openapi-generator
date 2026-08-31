#' Create a new Query
#'
#' @description
#' Query Class
#'
#' @docType class
#' @title Query
#' @description Query Class
#' @format An \code{R6Class} generator object
#' @field id Query integer [optional]
#' @field outcomes  list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Query <- R6::R6Class(
  "Query",
  public = list(
    `id` = NULL,
    `outcomes` = NULL,

    #' @description
    #' Initialize a new Query class.
    #'
    #' @param id Query
    #' @param outcomes outcomes. Default to ["SUCCESS","FAILURE"].
    #' @param ... Other optional arguments.
    initialize = function(`id` = NULL, `outcomes` = ["SUCCESS","FAILURE"], ...) {
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`outcomes`)) {
        stopifnot(is.vector(`outcomes`), length(`outcomes`) != 0)
        sapply(`outcomes`, function(x) stopifnot(is.character(x)))
        self$`outcomes` <- `outcomes`
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
    #' @return Query as a base R list.
    #' @examples
    #' # convert array of Query (x) to a data frame
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
    #' Convert Query to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      QueryObject <- list()
      if (!is.null(self$`id`)) {
        QueryObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`outcomes`)) {
        QueryObject[["outcomes"]] <-
          self$`outcomes`
      }
      return(QueryObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Query
    #'
    #' @param input_json the JSON input
    #' @return the instance of Query
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`outcomes`)) {
        self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Query in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Query
    #'
    #' @param input_json the JSON input
    #' @return the instance of Query
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      self
    },

    #' @description
    #' Validate JSON input with respect to Query and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Query
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
# Query$unlock()
#
## Below is an example to define the print function
# Query$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Query$lock()

