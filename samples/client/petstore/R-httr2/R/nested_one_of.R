#' Create a new NestedOneOf
#'
#' @description
#' NestedOneOf Class
#'
#' @docType class
#' @title NestedOneOf
#' @description NestedOneOf Class
#' @format An \code{R6Class} generator object
#' @field size  integer [optional]
#' @field nested_pig  \link{Pig} [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
NestedOneOf <- R6::R6Class(
  "NestedOneOf",
  public = list(
    `size` = NULL,
    `nested_pig` = NULL,

    #' @description
    #' Initialize a new NestedOneOf class.
    #'
    #' @param size size
    #' @param nested_pig nested_pig
    #' @param ... Other optional arguments.
    initialize = function(`size` = NULL, `nested_pig` = NULL, ...) {
      if (!is.null(`size`)) {
        if (!(is.numeric(`size`) && length(`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be an integer:", `size`))
        }
        self$`size` <- `size`
      }
      if (!is.null(`nested_pig`)) {
        stopifnot(R6::is.R6(`nested_pig`))
        self$`nested_pig` <- `nested_pig`
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
    #' @return NestedOneOf as a base R list.
    #' @examples
    #' # convert array of NestedOneOf (x) to a data frame
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
    #' Convert NestedOneOf to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      NestedOneOfObject <- list()
      if (!is.null(self$`size`)) {
        NestedOneOfObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`nested_pig`)) {
        NestedOneOfObject[["nested_pig"]] <-
          self$`nested_pig`$toSimpleType()
      }
      return(NestedOneOfObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of NestedOneOf
    #'
    #' @param input_json the JSON input
    #' @return the instance of NestedOneOf
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      if (!is.null(this_object$`nested_pig`)) {
        `nested_pig_object` <- Pig$new()
        `nested_pig_object`$fromJSON(jsonlite::toJSON(this_object$`nested_pig`, auto_unbox = TRUE, digits = NA))
        self$`nested_pig` <- `nested_pig_object`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return NestedOneOf in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of NestedOneOf
    #'
    #' @param input_json the JSON input
    #' @return the instance of NestedOneOf
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`size` <- this_object$`size`
      self$`nested_pig` <- Pig$new()$fromJSON(jsonlite::toJSON(this_object$`nested_pig`, auto_unbox = TRUE, digits = NA))
      self
    },

    #' @description
    #' Validate JSON input with respect to NestedOneOf and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of NestedOneOf
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
# NestedOneOf$unlock()
#
## Below is an example to define the print function
# NestedOneOf$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# NestedOneOf$lock()

