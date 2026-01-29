#' Create a new Whale
#'
#' @description
#' Whale Class
#'
#' @docType class
#' @title Whale
#' @description Whale Class
#' @format An \code{R6Class} generator object
#' @field hasBaleen  character [optional]
#' @field hasTeeth  character [optional]
#' @field className  character
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Whale <- R6::R6Class(
  "Whale",
  public = list(
    `hasBaleen` = NULL,
    `hasTeeth` = NULL,
    `className` = NULL,
    `_field_list` = c("hasBaleen", "hasTeeth", "className"),
    `additional_properties` = list(),

    #' @description
    #' Initialize a new Whale class.
    #'
    #' @param className className
    #' @param hasBaleen hasBaleen
    #' @param hasTeeth hasTeeth
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    initialize = function(`className`, `hasBaleen` = NULL, `hasTeeth` = NULL, additional_properties = NULL, ...) {
      if (!missing(`className`)) {
        if (!(is.character(`className`) && length(`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", `className`))
        }
        self$`className` <- `className`
      }
      if (!is.null(`hasBaleen`)) {
        if (!(is.logical(`hasBaleen`) && length(`hasBaleen`) == 1)) {
          stop(paste("Error! Invalid data for `hasBaleen`. Must be a boolean:", `hasBaleen`))
        }
        self$`hasBaleen` <- `hasBaleen`
      }
      if (!is.null(`hasTeeth`)) {
        if (!(is.logical(`hasTeeth`) && length(`hasTeeth`) == 1)) {
          stop(paste("Error! Invalid data for `hasTeeth`. Must be a boolean:", `hasTeeth`))
        }
        self$`hasTeeth` <- `hasTeeth`
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
    #' @return Whale as a base R list.
    #' @examples
    #' # convert array of Whale (x) to a data frame
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
    #' Convert Whale to a base R type
    #'
    #' @return A base R type, e.g. a list or numeric/character array.
    toSimpleType = function() {
      WhaleObject <- list()
      if (!is.null(self$`hasBaleen`)) {
        WhaleObject[["hasBaleen"]] <-
          self$`hasBaleen`
      }
      if (!is.null(self$`hasTeeth`)) {
        WhaleObject[["hasTeeth"]] <-
          self$`hasTeeth`
      }
      if (!is.null(self$`className`)) {
        WhaleObject[["className"]] <-
          self$`className`
      }
      for (key in names(self$additional_properties)) {
        WhaleObject[[key]] <- self$additional_properties[[key]]
      }

      return(WhaleObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Whale
    #'
    #' @param input_json the JSON input
    #' @return the instance of Whale
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`hasBaleen`)) {
        self$`hasBaleen` <- this_object$`hasBaleen`
      }
      if (!is.null(this_object$`hasTeeth`)) {
        self$`hasTeeth` <- this_object$`hasTeeth`
      }
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
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
    #' @return Whale in JSON format
    toJSONString = function(...) {
      simple <- self$toSimpleType()
      for (key in names(self$additional_properties)) {
        simple[[key]] <- self$additional_properties[[key]]
      }
      json <- jsonlite::toJSON(simple, auto_unbox = TRUE, digits = NA, ...)
      return(as.character(jsonlite::minify(json)))
    },

    #' @description
    #' Deserialize JSON string into an instance of Whale
    #'
    #' @param input_json the JSON input
    #' @return the instance of Whale
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`hasBaleen` <- this_object$`hasBaleen`
      self$`hasTeeth` <- this_object$`hasTeeth`
      self$`className` <- this_object$`className`
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },

    #' @description
    #' Validate JSON input with respect to Whale and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for Whale: the required field `className` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Whale
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
# Whale$unlock()
#
## Below is an example to define the print function
# Whale$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Whale$lock()

