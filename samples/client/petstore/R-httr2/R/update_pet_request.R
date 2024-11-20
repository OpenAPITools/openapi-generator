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
    #' Convert to a list. This method was misnamed, it actually returns a list. Use `toList()` instead.
    toJSON = function() {
      .Deprecated(new = "toList", msg = "Use the '$toList()' method instead since that is more learly named. Use '$toJSONstring()' to get a JSON string")
      return(self$toList())
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
    #' df <- x |> purrr::map_dfr(\(y)y$toList())
    #' df
    #' }
    toList = function() {
      UpdatePetRequestObject <- list()
      if (!is.null(self$`jsonData`)) {
        UpdatePetRequestObject[["jsonData"]] <-
          self$`jsonData`$toList()
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
    #' @param minify Logical. If `TRUE` remove all indentation and white space
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return UpdatePetRequest in JSON format
    toJSONString = function(minify = TRUE, ...) {
      json_obj <- self$toList()
      

      json_string <- jsonlite::toJSON(json_obj, auto_unbox = TRUE, digits = NA, ...)
      if (minify) {
        json_string <- jsonlite::minify(json_string)
      }
      return(as.character(json_string))
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

