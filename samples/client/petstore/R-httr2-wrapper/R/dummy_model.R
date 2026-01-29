#' Create a new DummyModel
#'
#' @description
#' to test the model name mapping
#'
#' @docType class
#' @title DummyModel
#' @description DummyModel Class
#' @format An \code{R6Class} generator object
#' @field property  character [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DummyModel <- R6::R6Class(
  "DummyModel",
  public = list(
    `property` = NULL,
    `_field_list` = c("property"),
    `additional_properties` = list(),

    #' @description
    #' Initialize a new DummyModel class.
    #'
    #' @param property property
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    initialize = function(`property` = NULL, additional_properties = NULL, ...) {
      if (!is.null(`property`)) {
        if (!(is.character(`property`) && length(`property`) == 1)) {
          stop(paste("Error! Invalid data for `property`. Must be a string:", `property`))
        }
        self$`property` <- `property`
      }
      if (!is.null(additional_properties)) {
        for (key in names(additional_properties)) {
          self$additional_properties[[key]] <- additional_properties[[key]]
        }
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
    #' @return DummyModel as a base R list.
    #' @examples
    #' # convert array of DummyModel (x) to a data frame
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
    #' Convert DummyModel to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      DummyModelObject <- list()
      if (!is.null(self$`property`)) {
        DummyModelObject[["property"]] <-
          self$`property`
      }
      for (key in names(self$additional_properties)) {
        DummyModelObject[[key]] <- self$additional_properties[[key]]
      }

      return(DummyModelObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of DummyModel
    #'
    #' @param input_json the JSON input
    #' @return the instance of DummyModel
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`property`)) {
        self$`property` <- this_object$`property`
      }
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return DummyModel in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      for (key in names(self$additional_properties)) {
        simple[[key]] <- self$additional_properties[[key]]
      }
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of DummyModel
    #'
    #' @param input_json the JSON input
    #' @return the instance of DummyModel
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`property` <- this_object$`property`
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },

    #' @description
    #' Validate JSON input with respect to DummyModel and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of DummyModel
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
# DummyModel$unlock()
#
## Below is an example to define the print function
# DummyModel$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# DummyModel$lock()

