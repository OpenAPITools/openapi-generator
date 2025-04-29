#' Create a new DataQuery
#'
#' @description
#' DataQuery Class
#'
#' @docType class
#' @title DataQuery
#' @description DataQuery Class
#' @format An \code{R6Class} generator object
#' @field id Query integer [optional]
#' @field outcomes  list(character) [optional]
#' @field suffix test suffix character [optional]
#' @field text Some text containing white spaces character [optional]
#' @field date A date character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DataQuery <- R6::R6Class(
  "DataQuery",
  inherit = Query,
  public = list(
    `id` = NULL,
    `outcomes` = NULL,
    `suffix` = NULL,
    `text` = NULL,
    `date` = NULL,

    #' @description
    #' Initialize a new DataQuery class.
    #'
    #' @param id Query
    #' @param outcomes outcomes. Default to [SUCCESS, FAILURE].
    #' @param suffix test suffix
    #' @param text Some text containing white spaces
    #' @param date A date
    #' @param ... Other optional arguments.
    initialize = function(`id` = NULL, `outcomes` = [SUCCESS, FAILURE], `suffix` = NULL, `text` = NULL, `date` = NULL, ...) {
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`outcomes`)) {
        stopifnot(is.vector(`outcomes`), length(`outcomes`) != 0)
        sapply(`outcomes`, function(x) stopifnot(is.character(x)))
        self$`outcomes` <- `outcomes`
      }
      if (!is.null(`suffix`)) {
        if (!(is.character(`suffix`) && length(`suffix`) == 1)) {
          stop(paste("Error! Invalid data for `suffix`. Must be a string:", `suffix`))
        }
        self$`suffix` <- `suffix`
      }
      if (!is.null(`text`)) {
        if (!(is.character(`text`) && length(`text`) == 1)) {
          stop(paste("Error! Invalid data for `text`. Must be a string:", `text`))
        }
        self$`text` <- `text`
      }
      if (!is.null(`date`)) {
        if (!is.character(`date`)) {
          stop(paste("Error! Invalid data for `date`. Must be a string:", `date`))
        }
        self$`date` <- `date`
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
    #' @return DataQuery as a base R list.
    #' @examples
    #' # convert array of DataQuery (x) to a data frame
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
    #' Convert DataQuery to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      DataQueryObject <- list()
      if (!is.null(self$`id`)) {
        DataQueryObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`outcomes`)) {
        DataQueryObject[["outcomes"]] <-
          self$`outcomes`
      }
      if (!is.null(self$`suffix`)) {
        DataQueryObject[["suffix"]] <-
          self$`suffix`
      }
      if (!is.null(self$`text`)) {
        DataQueryObject[["text"]] <-
          self$`text`
      }
      if (!is.null(self$`date`)) {
        DataQueryObject[["date"]] <-
          self$`date`
      }
      return(DataQueryObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of DataQuery
    #'
    #' @param input_json the JSON input
    #' @return the instance of DataQuery
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`outcomes`)) {
        self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`suffix`)) {
        self$`suffix` <- this_object$`suffix`
      }
      if (!is.null(this_object$`text`)) {
        self$`text` <- this_object$`text`
      }
      if (!is.null(this_object$`date`)) {
        self$`date` <- this_object$`date`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return DataQuery in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of DataQuery
    #'
    #' @param input_json the JSON input
    #' @return the instance of DataQuery
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      self$`suffix` <- this_object$`suffix`
      self$`text` <- this_object$`text`
      self$`date` <- this_object$`date`
      self
    },

    #' @description
    #' Validate JSON input with respect to DataQuery and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of DataQuery
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
# DataQuery$unlock()
#
## Below is an example to define the print function
# DataQuery$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# DataQuery$lock()

