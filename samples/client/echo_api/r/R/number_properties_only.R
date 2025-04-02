#' Create a new NumberPropertiesOnly
#'
#' @description
#' NumberPropertiesOnly Class
#'
#' @docType class
#' @title NumberPropertiesOnly
#' @description NumberPropertiesOnly Class
#' @format An \code{R6Class} generator object
#' @field number  numeric [optional]
#' @field float  numeric [optional]
#' @field double  numeric [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
NumberPropertiesOnly <- R6::R6Class(
  "NumberPropertiesOnly",
  public = list(
    `number` = NULL,
    `float` = NULL,
    `double` = NULL,

    #' @description
    #' Initialize a new NumberPropertiesOnly class.
    #'
    #' @param number number
    #' @param float float
    #' @param double double
    #' @param ... Other optional arguments.
    initialize = function(`number` = NULL, `float` = NULL, `double` = NULL, ...) {
      if (!is.null(`number`)) {
        self$`number` <- `number`
      }
      if (!is.null(`float`)) {
        if (!(is.numeric(`float`) && length(`float`) == 1)) {
          stop(paste("Error! Invalid data for `float`. Must be a number:", `float`))
        }
        self$`float` <- `float`
      }
      if (!is.null(`double`)) {
        if (!(is.numeric(`double`) && length(`double`) == 1)) {
          stop(paste("Error! Invalid data for `double`. Must be a number:", `double`))
        }
        self$`double` <- `double`
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
    #' @return NumberPropertiesOnly as a base R list.
    #' @examples
    #' # convert array of NumberPropertiesOnly (x) to a data frame
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
    #' Convert NumberPropertiesOnly to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      NumberPropertiesOnlyObject <- list()
      if (!is.null(self$`number`)) {
        NumberPropertiesOnlyObject[["number"]] <-
          self$`number`
      }
      if (!is.null(self$`float`)) {
        NumberPropertiesOnlyObject[["float"]] <-
          self$`float`
      }
      if (!is.null(self$`double`)) {
        NumberPropertiesOnlyObject[["double"]] <-
          self$`double`
      }
      return(NumberPropertiesOnlyObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of NumberPropertiesOnly
    #'
    #' @param input_json the JSON input
    #' @return the instance of NumberPropertiesOnly
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`number`)) {
        self$`number` <- this_object$`number`
      }
      if (!is.null(this_object$`float`)) {
        self$`float` <- this_object$`float`
      }
      if (!is.null(this_object$`double`)) {
        self$`double` <- this_object$`double`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return NumberPropertiesOnly in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of NumberPropertiesOnly
    #'
    #' @param input_json the JSON input
    #' @return the instance of NumberPropertiesOnly
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`number` <- this_object$`number`
      self$`float` <- this_object$`float`
      self$`double` <- this_object$`double`
      self
    },

    #' @description
    #' Validate JSON input with respect to NumberPropertiesOnly and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of NumberPropertiesOnly
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      if (self$`double` > 50.2) {
        return(FALSE)
      }
      if (self$`double` < 0.8) {
        return(FALSE)
      }

      TRUE
    },

    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    getInvalidFields = function() {
      invalid_fields <- list()
      if (self$`double` > 50.2) {
        invalid_fields["double"] <- "Invalid value for `double`, must be smaller than or equal to 50.2."
      }
      if (self$`double` < 0.8) {
        invalid_fields["double"] <- "Invalid value for `double`, must be bigger than or equal to 0.8."
      }

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
# NumberPropertiesOnly$unlock()
#
## Below is an example to define the print function
# NumberPropertiesOnly$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# NumberPropertiesOnly$lock()

