#' Create a new Bird
#'
#' @description
#' Bird Class
#'
#' @docType class
#' @title Bird
#' @description Bird Class
#' @format An \code{R6Class} generator object
#' @field size  character [optional]
#' @field color  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Bird <- R6::R6Class(
  "Bird",
  public = list(
    `size` = NULL,
    `color` = NULL,
    #' Initialize a new Bird class.
    #'
    #' @description
    #' Initialize a new Bird class.
    #'
    #' @param size size
    #' @param color color
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`size` = NULL, `color` = NULL, ...) {
      if (!is.null(`size`)) {
        if (!(is.character(`size`) && length(`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be a string:", `size`))
        }
        self$`size` <- `size`
      }
      if (!is.null(`color`)) {
        if (!(is.character(`color`) && length(`color`) == 1)) {
          stop(paste("Error! Invalid data for `color`. Must be a string:", `color`))
        }
        self$`color` <- `color`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Bird in JSON format
    #' @export
    toJSON = function() {
      BirdObject <- list()
      if (!is.null(self$`size`)) {
        BirdObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`color`)) {
        BirdObject[["color"]] <-
          self$`color`
      }
      BirdObject
    },
    #' Deserialize JSON string into an instance of Bird
    #'
    #' @description
    #' Deserialize JSON string into an instance of Bird
    #'
    #' @param input_json the JSON input
    #' @return the instance of Bird
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      if (!is.null(this_object$`color`)) {
        self$`color` <- this_object$`color`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Bird in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`size`)) {
          sprintf(
          '"size":
            "%s"
                    ',
          self$`size`
          )
        },
        if (!is.null(self$`color`)) {
          sprintf(
          '"color":
            "%s"
                    ',
          self$`color`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Bird
    #'
    #' @description
    #' Deserialize JSON string into an instance of Bird
    #'
    #' @param input_json the JSON input
    #' @return the instance of Bird
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`size` <- this_object$`size`
      self$`color` <- this_object$`color`
      self
    },
    #' Validate JSON input with respect to Bird
    #'
    #' @description
    #' Validate JSON input with respect to Bird and throw an exception if invalid
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
    #' @return String representation of Bird
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
# Bird$unlock()
#
## Below is an example to define the print function
# Bird$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Bird$lock()

