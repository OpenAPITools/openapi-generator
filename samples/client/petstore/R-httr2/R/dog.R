#' Create a new Dog
#'
#' @description
#' Dog Class
#'
#' @docType class
#' @title Dog
#' @description Dog Class
#' @format An \code{R6Class} generator object
#' @field className  character
#' @field color  character [optional]
#' @field breed  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Dog <- R6::R6Class(
  "Dog",
  inherit = Animal,
  public = list(
    `className` = NULL,
    `color` = NULL,
    `breed` = NULL,

    #' @description
    #' Initialize a new Dog class.
    #'
    #' @param className className
    #' @param color color. Default to "red".
    #' @param breed breed
    #' @param ... Other optional arguments.
    initialize = function(`className`, `color` = "red", `breed` = NULL, ...) {
      if (!missing(`className`)) {
        if (!(is.character(`className`) && length(`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", `className`))
        }
        self$`className` <- `className`
      }
      if (!is.null(`color`)) {
        if (!(is.character(`color`) && length(`color`) == 1)) {
          stop(paste("Error! Invalid data for `color`. Must be a string:", `color`))
        }
        self$`color` <- `color`
      }
      if (!is.null(`breed`)) {
        if (!(is.character(`breed`) && length(`breed`) == 1)) {
          stop(paste("Error! Invalid data for `breed`. Must be a string:", `breed`))
        }
        self$`breed` <- `breed`
      }
    },

    #' @description
    #' Convert to a list. This method was misnamed, it actually returns a list. Use `toList()` instead.
    toJSON = function() {
      .Deprecated(new = "toList", msg = "Use the '$toList()' method instead since that is more learly named. Use '$toJSONstring()' to get a JSON string")
      return(self$toList())
    },

    #' @description
    #' Convert to a List
    #'
    #' Convert the R6 object to a list to work more easily with other tooling.
    #'
    #' @return Dog as a base R list.
    #' @examples
    #' # convert array of Dog (x) to a data frame
    #' \dontrun{
    #' df <- x |> purrr::map_dfr(\(y)y$toList())
    #' df
    #' }
    toList = function() {
      DogObject <- list()
      if (!is.null(self$`className`)) {
        DogObject[["className"]] <-
          self$`className`
      }
      if (!is.null(self$`color`)) {
        DogObject[["color"]] <-
          self$`color`
      }
      if (!is.null(self$`breed`)) {
        DogObject[["breed"]] <-
          self$`breed`
      }
      return(DogObject)
    },

    #' @description
    #' Deserialize JSON string into an instance of Dog
    #'
    #' @param input_json the JSON input
    #' @return the instance of Dog
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
      }
      if (!is.null(this_object$`color`)) {
        self$`color` <- this_object$`color`
      }
      if (!is.null(this_object$`breed`)) {
        self$`breed` <- this_object$`breed`
      }
      self
    },

    #' @description
    #' To JSON String
    #' 
    #' @param minify Logical. If `TRUE` remove all indentation and white space
    #' @param ... Parameters passed to `jsonlite::toJSON`
    #' @return Dog in JSON format
    toJSONString = function(minify = TRUE, ...) {
      json_obj <- self$toList()
      

      json_string <- jsonlite::toJSON(json_obj, auto_unbox = TRUE, digits = NA, ...)
      if (minify) {
        json_string <- jsonlite::minify(json_string)
      }
      return(as.character(json_string))
    },

    #' @description
    #' Deserialize JSON string into an instance of Dog
    #'
    #' @param input_json the JSON input
    #' @return the instance of Dog
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`className` <- this_object$`className`
      self$`color` <- this_object$`color`
      self$`breed` <- this_object$`breed`
      self
    },

    #' @description
    #' Validate JSON input with respect to Dog and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for Dog: the required field `className` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Dog
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
# Dog$unlock()
#
## Below is an example to define the print function
# Dog$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Dog$lock()

