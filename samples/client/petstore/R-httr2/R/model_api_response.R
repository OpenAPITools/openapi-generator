#' Create a new ModelApiResponse
#'
#' @description
#' Describes the result of uploading an image resource
#'
#' @docType class
#' @title ModelApiResponse
#' @description ModelApiResponse Class
#' @format An \code{R6Class} generator object
#' @field code  integer [optional]
#' @field type  character [optional]
#' @field message  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
ModelApiResponse <- R6::R6Class(
  "ModelApiResponse",
  public = list(
    `code` = NULL,
    `type` = NULL,
    `message` = NULL,

    #' @description
    #' Initialize a new ModelApiResponse class.
    #'
    #' @param code code
    #' @param type type
    #' @param message message
    #' @param ... Other optional arguments.
    initialize = function(`code` = NULL, `type` = NULL, `message` = NULL, ...) {
      if (!is.null(`code`)) {
        if (!(is.numeric(`code`) && length(`code`) == 1)) {
          stop(paste("Error! Invalid data for `code`. Must be an integer:", `code`))
        }
        self$`code` <- `code`
      }
      if (!is.null(`type`)) {
        if (!(is.character(`type`) && length(`type`) == 1)) {
          stop(paste("Error! Invalid data for `type`. Must be a string:", `type`))
        }
        self$`type` <- `type`
      }
      if (!is.null(`message`)) {
        if (!(is.character(`message`) && length(`message`) == 1)) {
          stop(paste("Error! Invalid data for `message`. Must be a string:", `message`))
        }
        self$`message` <- `message`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return ModelApiResponse in JSON format
    toJSON = function() {
      ModelApiResponseObject <- list()
      if (!is.null(self$`code`)) {
        ModelApiResponseObject[["code"]] <-
          self$`code`
      }
      if (!is.null(self$`type`)) {
        ModelApiResponseObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`message`)) {
        ModelApiResponseObject[["message"]] <-
          self$`message`
      }
      ModelApiResponseObject
    },

    #' @description
    #' Deserialize JSON string into an instance of ModelApiResponse
    #'
    #' @param input_json the JSON input
    #' @return the instance of ModelApiResponse
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`code`)) {
        self$`code` <- this_object$`code`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`message`)) {
        self$`message` <- this_object$`message`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return ModelApiResponse in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`code`)) {
          sprintf(
          '"code":
            %d
                    ',
          self$`code`
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
            "%s"
                    ',
          self$`type`
          )
        },
        if (!is.null(self$`message`)) {
          sprintf(
          '"message":
            "%s"
                    ',
          self$`message`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of ModelApiResponse
    #'
    #' @param input_json the JSON input
    #' @return the instance of ModelApiResponse
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`code` <- this_object$`code`
      self$`type` <- this_object$`type`
      self$`message` <- this_object$`message`
      self
    },

    #' @description
    #' Validate JSON input with respect to ModelApiResponse and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of ModelApiResponse
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
# ModelApiResponse$unlock()
#
## Below is an example to define the print function
# ModelApiResponse$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# ModelApiResponse$lock()

