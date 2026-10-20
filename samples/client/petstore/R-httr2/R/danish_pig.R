#' Create a new DanishPig
#'
#' @description
#' DanishPig Class
#'
#' @docType class
#' @title DanishPig
#' @description DanishPig Class
#' @format An \code{R6Class} generator object
#' @field className  character
#' @field size  integer
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DanishPig <- R6::R6Class(
  "DanishPig",
  public = list(
    `className` = NULL,
    `size` = NULL,

    #' @description
    #' Initialize a new DanishPig class.
    #'
    #' @param className className
    #' @param size size
    #' @param ... Other optional arguments.
    initialize = function(`className`, `size`, ...) {
      if (!missing(`className`)) {
        if (!(is.character(`className`) && length(`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", `className`))
        }
        self$`className` <- `className`
      }
      if (!missing(`size`)) {
        if (!(is.numeric(`size`) && length(`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be an integer:", `size`))
        }
        self$`size` <- `size`
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
    #' @return DanishPig as a base R list.
    #' @examples
    #' # convert array of DanishPig (x) to a data frame
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
    #' Convert DanishPig to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      DanishPigObject <- list()
      if (!is.null(self$`className`)) {
        DanishPigObject[["className"]] <-
          self$`className`
      }
      if (!is.null(self$`size`)) {
        DanishPigObject[["size"]] <-
          self$`size`
      }
      return(DanishPigObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of DanishPig
    #'
    #' @param input_json the JSON input
    #' @return the instance of DanishPig
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
      }
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return DanishPig in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of DanishPig
    #'
    #' @param input_json the JSON input
    #' @return the instance of DanishPig
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`className` <- this_object$`className`
      self$`size` <- this_object$`size`
      self
    },

    #' @description
    #' Validate JSON input with respect to DanishPig and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for DanishPig: the required field `className` is missing."))
      }
      # check the required field `size`
      if (!is.null(input_json$`size`)) {
        if (!(is.numeric(input_json$`size`) && length(input_json$`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be an integer:", input_json$`size`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for DanishPig: the required field `size` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of DanishPig
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

      # check if the required `size` is null
      if (is.null(self$`size`)) {
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

      # check if the required `size` is null
      if (is.null(self$`size`)) {
        invalid_fields["size"] <- "Non-nullable required field `size` cannot be null."
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
# DanishPig$unlock()
#
## Below is an example to define the print function
# DanishPig$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# DanishPig$lock()

