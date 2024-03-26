#' Create a new TestFormObjectMultipartRequestMarker
#'
#' @description
#' TestFormObjectMultipartRequestMarker Class
#'
#' @docType class
#' @title TestFormObjectMultipartRequestMarker
#' @description TestFormObjectMultipartRequestMarker Class
#' @format An \code{R6Class} generator object
#' @field name  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
TestFormObjectMultipartRequestMarker <- R6::R6Class(
  "TestFormObjectMultipartRequestMarker",
  public = list(
    `name` = NULL,
    #' Initialize a new TestFormObjectMultipartRequestMarker class.
    #'
    #' @description
    #' Initialize a new TestFormObjectMultipartRequestMarker class.
    #'
    #' @param name name
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`name` = NULL, ...) {
      if (!is.null(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return TestFormObjectMultipartRequestMarker in JSON format
    #' @export
    toJSON = function() {
      TestFormObjectMultipartRequestMarkerObject <- list()
      if (!is.null(self$`name`)) {
        TestFormObjectMultipartRequestMarkerObject[["name"]] <-
          self$`name`
      }
      TestFormObjectMultipartRequestMarkerObject
    },
    #' Deserialize JSON string into an instance of TestFormObjectMultipartRequestMarker
    #'
    #' @description
    #' Deserialize JSON string into an instance of TestFormObjectMultipartRequestMarker
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestFormObjectMultipartRequestMarker
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return TestFormObjectMultipartRequestMarker in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of TestFormObjectMultipartRequestMarker
    #'
    #' @description
    #' Deserialize JSON string into an instance of TestFormObjectMultipartRequestMarker
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestFormObjectMultipartRequestMarker
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`name` <- this_object$`name`
      self
    },
    #' Validate JSON input with respect to TestFormObjectMultipartRequestMarker
    #'
    #' @description
    #' Validate JSON input with respect to TestFormObjectMultipartRequestMarker and throw an exception if invalid
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
    #' @return String representation of TestFormObjectMultipartRequestMarker
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
# TestFormObjectMultipartRequestMarker$unlock()
#
## Below is an example to define the print function
# TestFormObjectMultipartRequestMarker$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# TestFormObjectMultipartRequestMarker$lock()

