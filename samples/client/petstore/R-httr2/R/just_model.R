#' Create a new JustModel
#'
#' @description
#' to test the model name mapping
#'
#' @docType class
#' @title JustModel
#' @description JustModel Class
#' @format An \code{R6Class} generator object
#' @field property  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
JustModel <- R6::R6Class(
  "JustModel",
  public = list(
    `property` = NULL,
    #' Initialize a new JustModel class.
    #'
    #' @description
    #' Initialize a new JustModel class.
    #'
    #' @param property property
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`property` = NULL, ...) {
      if (!is.null(`property`)) {
        if (!(is.character(`property`) && length(`property`) == 1)) {
          stop(paste("Error! Invalid data for `property`. Must be a string:", `property`))
        }
        self$`property` <- `property`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return JustModel in JSON format
    #' @export
    toJSON = function() {
      JustModelObject <- list()
      if (!is.null(self$`property`)) {
        JustModelObject[["property"]] <-
          self$`property`
      }
      JustModelObject
    },
    #' Deserialize JSON string into an instance of JustModel
    #'
    #' @description
    #' Deserialize JSON string into an instance of JustModel
    #'
    #' @param input_json the JSON input
    #' @return the instance of JustModel
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`property`)) {
        self$`property` <- this_object$`property`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return JustModel in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`property`)) {
          sprintf(
          '"property":
            "%s"
                    ',
          self$`property`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of JustModel
    #'
    #' @description
    #' Deserialize JSON string into an instance of JustModel
    #'
    #' @param input_json the JSON input
    #' @return the instance of JustModel
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`property` <- this_object$`property`
      self
    },
    #' Validate JSON input with respect to JustModel
    #'
    #' @description
    #' Validate JSON input with respect to JustModel and throw an exception if invalid
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
    #' @return String representation of JustModel
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
# JustModel$unlock()
#
## Below is an example to define the print function
# JustModel$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# JustModel$lock()

