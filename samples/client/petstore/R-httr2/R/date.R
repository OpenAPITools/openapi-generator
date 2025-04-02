#' Create a new Date
#'
#' @description
#' to test the model name `Date`
#'
#' @docType class
#' @title Date
#' @description Date Class
#' @format An \code{R6Class} generator object
#' @field className  character
#' @field percent using \% in the description character [optional]
#' @field url_property  character
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Date <- R6::R6Class(
  "Date",
  public = list(
    `className` = NULL,
    `percent` = NULL,
    `url_property` = NULL,

    #' @description
    #' Initialize a new Date class.
    #'
    #' @param className className
    #' @param url_property url_property
    #' @param percent using \% in the description
    #' @param ... Other optional arguments.
    initialize = function(`className`, `url_property`, `percent` = NULL, ...) {
      if (!missing(`className`)) {
        if (!(is.character(`className`) && length(`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", `className`))
        }
        self$`className` <- `className`
      }
      if (!missing(`url_property`)) {
        if (!(is.character(`url_property`) && length(`url_property`) == 1)) {
          stop(paste("Error! Invalid data for `url_property`. Must be a string:", `url_property`))
        }
        # to validate URL. ref: https://stackoverflow.com/questions/73952024/url-validation-in-r
        if (!stringr::str_detect(`url_property`, "(https?|ftp)://[^ /$.?#].[^\\s]*")) {
          stop(paste("Error! Invalid data for `url_property`. Must be a URL:", `url_property`))
        }
        self$`url_property` <- `url_property`
      }
      if (!is.null(`percent`)) {
        if (!(is.character(`percent`) && length(`percent`) == 1)) {
          stop(paste("Error! Invalid data for `percent`. Must be a string:", `percent`))
        }
        self$`percent` <- `percent`
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
    #' @return Date as a base R list.
    #' @examples
    #' # convert array of Date (x) to a data frame
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
    #' Convert Date to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      DateObject <- list()
      if (!is.null(self$`className`)) {
        DateObject[["className"]] <-
          self$`className`
      }
      if (!is.null(self$`percent`)) {
        DateObject[["percent_description"]] <-
          self$`percent`
      }
      if (!is.null(self$`url_property`)) {
        DateObject[["url_property"]] <-
          self$`url_property`
      }
      return(DateObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Date
    #'
    #' @param input_json the JSON input
    #' @return the instance of Date
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
      }
      if (!is.null(this_object$`percent_description`)) {
        self$`percent` <- this_object$`percent_description`
      }
      if (!is.null(this_object$`url_property`)) {
        # to validate URL. ref: https://stackoverflow.com/questions/73952024/url-validation-in-r
        if (!stringr::str_detect(this_object$`url_property`, "(https?|ftp)://[^ /$.?#].[^\\s]*")) {
          stop(paste("Error! Invalid data for `url_property`. Must be a URL:", this_object$`url_property`))
        }
        self$`url_property` <- this_object$`url_property`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Date in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Date
    #'
    #' @param input_json the JSON input
    #' @return the instance of Date
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`className` <- this_object$`className`
      self$`percent` <- this_object$`percent`
      # to validate URL. ref: https://stackoverflow.com/questions/73952024/url-validation-in-r
      if (!stringr::str_detect(this_object$`url_property`, "(https?|ftp)://[^ /$.?#].[^\\s]*")) {
        stop(paste("Error! Invalid data for `url_property`. Must be a URL:", this_object$`url_property`))
      }
      self$`url_property` <- this_object$`url_property`
      self
    },

    #' @description
    #' Validate JSON input with respect to Date and throw an exception if invalid
    #'
    #' @param input the JSON input
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `className`
      if (!is.null(input_json$`className`)) {
        if (!(is.character(input_json$`className`) && length(input_json$`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", input_json$`className`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Date: the required field `className` is missing."))
      }
      # check the required field `url_property`
      if (!is.null(input_json$`url_property`)) {
        if (!(is.character(input_json$`url_property`) && length(input_json$`url_property`) == 1)) {
          stop(paste("Error! Invalid data for `url_property`. Must be a string:", input_json$`url_property`))
        }
        # to validate URL. ref: https://stackoverflow.com/questions/73952024/url-validation-in-r
        if (!stringr::str_detect(input_json$`url_property`, "(https?|ftp)://[^ /$.?#].[^\\s]*")) {
          stop(paste("Error! Invalid data for `url_property`. Must be a URL:", input_json$`url_property`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for Date: the required field `url_property` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Date
    toString = function() {
      self$toJSONString()
    },

    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    isValid = function() {
      # check if the required `className` is null
      if (is.null(self$`className`)) {
        return(FALSE)
      }

      # check if the required `url_property` is null
      if (is.null(self$`url_property`)) {
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
      # check if the required `className` is null
      if (is.null(self$`className`)) {
        invalid_fields["className"] <- "Non-nullable required field `className` cannot be null."
      }

      # check if the required `url_property` is null
      if (is.null(self$`url_property`)) {
        invalid_fields["url_property"] <- "Non-nullable required field `url_property` cannot be null."
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
# Date$unlock()
#
## Below is an example to define the print function
# Date$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Date$lock()

