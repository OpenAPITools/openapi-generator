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

    #' @description
    #' Initialize a new Bird class.
    #'
    #' @param size size
    #' @param color color
    #' @param ... Other optional arguments.
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
    #' @return Bird as a base R list.
    #' @examples
    #' # convert array of Bird (x) to a data frame
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
    #' Convert Bird to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      BirdObject <- list()
      if (!is.null(self$`size`)) {
        BirdObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`color`)) {
        BirdObject[["color"]] <-
          self$`color`
      }
      return(BirdObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Bird
    #'
    #' @param input_json the JSON input
    #' @return the instance of Bird
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

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Bird in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Bird
    #'
    #' @param input_json the JSON input
    #' @return the instance of Bird
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`size` <- this_object$`size`
      self$`color` <- this_object$`color`
      self
    },

    #' @description
    #' Validate JSON input with respect to Bird and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Bird
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
# Bird$unlock()
#
## Below is an example to define the print function
# Bird$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Bird$lock()

