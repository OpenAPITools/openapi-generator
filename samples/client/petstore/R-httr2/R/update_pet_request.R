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
    #' Initialize a new UpdatePetRequest class.
    #'
    #' @description
    #' Initialize a new UpdatePetRequest class.
    #'
    #' @param jsonData jsonData
    #' @param binaryDataN2Information binaryDataN2Information
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`jsonData` = NULL, `binaryDataN2Information` = NULL, ...) {
      if (!is.null(`jsonData`)) {
        stopifnot(R6::is.R6(`jsonData`))
        self$`jsonData` <- `jsonData`
      }
      if (!is.null(`binaryDataN2Information`)) {
        self$`binaryDataN2Information` <- `binaryDataN2Information`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return UpdatePetRequest in JSON format
    #' @export
    toJSON = function() {
      UpdatePetRequestObject <- list()
      if (!is.null(self$`jsonData`)) {
        UpdatePetRequestObject[["jsonData"]] <-
          self$`jsonData`$toJSON()
      }
      if (!is.null(self$`binaryDataN2Information`)) {
        UpdatePetRequestObject[["binaryDataN2Information"]] <-
          self$`binaryDataN2Information`
      }
      UpdatePetRequestObject
    },
    #' Deserialize JSON string into an instance of UpdatePetRequest
    #'
    #' @description
    #' Deserialize JSON string into an instance of UpdatePetRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of UpdatePetRequest
    #' @export
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
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return UpdatePetRequest in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`jsonData`)) {
          sprintf(
          '"jsonData":
          %s
          ',
          jsonlite::toJSON(self$`jsonData`$toJSON(), auto_unbox = TRUE, digits = NA)
          )
        },
        if (!is.null(self$`binaryDataN2Information`)) {
          sprintf(
          '"binaryDataN2Information":
            "%s"
                    ',
          self$`binaryDataN2Information`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of UpdatePetRequest
    #'
    #' @description
    #' Deserialize JSON string into an instance of UpdatePetRequest
    #'
    #' @param input_json the JSON input
    #' @return the instance of UpdatePetRequest
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`jsonData` <- Pet$new()$fromJSON(jsonlite::toJSON(this_object$`jsonData`, auto_unbox = TRUE, digits = NA))
      self$`binaryDataN2Information` <- this_object$`binaryDataN2Information`
      self
    },
    #' Validate JSON input with respect to UpdatePetRequest
    #'
    #' @description
    #' Validate JSON input with respect to UpdatePetRequest and throw an exception if invalid
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
    #' @return String representation of UpdatePetRequest
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
# UpdatePetRequest$unlock()
#
## Below is an example to define the print function
# UpdatePetRequest$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# UpdatePetRequest$lock()

