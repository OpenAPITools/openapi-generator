#' Create a new UpdatePetRequest
#'
#' @description
#' UpdatePetRequest Class
#'
#' @docType class
#' @title UpdatePetRequest
#' @description UpdatePetRequest Class
#' @format An \code{R6Class} generator object
#' @field jsonData  \link{Pet} [optional]
#' @field binaryDataN2Information  data.frame [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
UpdatePetRequest <- R6::R6Class(
  "UpdatePetRequest",
  public = list(
    `jsonData` = NULL,
    `binaryDataN2Information` = NULL,

    #' @description
    #' Initialize a new UpdatePetRequest class.
    #'
    #' @param jsonData jsonData
    #' @param binaryDataN2Information binaryDataN2Information
    #' @param ... Other optional arguments.
    initialize = function(`jsonData` = NULL, `binaryDataN2Information` = NULL, ...) {
      if (!is.null(`jsonData`)) {
        stopifnot(R6::is.R6(`jsonData`))
        self$`jsonData` <- `jsonData`
      }
      if (!is.null(`binaryDataN2Information`)) {
        self$`binaryDataN2Information` <- `binaryDataN2Information`
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
    #' @return UpdatePetRequest as a base R list.
    #' @examples
    #' # convert array of UpdatePetRequest (x) to a data frame
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
    #' Convert UpdatePetRequest to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      UpdatePetRequestObject <- list()
      if (!is.null(self$`jsonData`)) {
        UpdatePetRequestObject[["jsonData"]] <-
          self$`jsonData`$toSimpleType()
      }
      if (!is.null(self$`binaryDataN2Information`)) {
        UpdatePetRequestObject[["binaryDataN2Information"]] <-
          self$`binaryDataN2Information`
      }
      return(UpdatePetRequestObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of UpdatePetRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of UpdatePetRequest
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`jsonData`)) {
        `jsondata_object` <- Pet$new()
        `jsondata_object`$fromJSON(jsonlite::toJSON(this_object$`jsonData`, auto_unbox = TRUE, digits = NA))
        self$`jsonData` <- `jsondata_object`
      }
      if (!is.null(this_object$`binaryDataN2Information`)) {
        self$`binaryDataN2Information` <- this_object$`binaryDataN2Information`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return UpdatePetRequest in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of UpdatePetRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of UpdatePetRequest
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`jsonData` <- Pet$new()$fromJSON(jsonlite::toJSON(this_object$`jsonData`, auto_unbox = TRUE, digits = NA))
      self$`binaryDataN2Information` <- this_object$`binaryDataN2Information`
      self
    },

    #' @description
    #' Validate JSON input with respect to UpdatePetRequest and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of UpdatePetRequest
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
# UpdatePetRequest$unlock()
#
## Below is an example to define the print function
# UpdatePetRequest$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# UpdatePetRequest$lock()

