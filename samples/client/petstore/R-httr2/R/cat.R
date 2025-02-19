#' Create a new Cat
#'
#' @description
#' Cat Class
#'
#' @docType class
#' @title Cat
#' @description Cat Class
#' @format An \code{R6Class} generator object
#' @field className  character
#' @field color  character [optional]
#' @field declawed  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Cat <- R6::R6Class(
  "Cat",
  inherit = Animal,
  public = list(
    `className` = NULL,
    `color` = NULL,
    `declawed` = NULL,

    #' @description
    #' Initialize a new Cat class.
    #'
    #' @param className className
    #' @param color color. Default to "red".
    #' @param declawed declawed
    #' @param ... Other optional arguments.
    initialize = function(`className`, `color` = "red", `declawed` = NULL, ...) {
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
      if (!is.null(`declawed`)) {
        if (!(is.logical(`declawed`) && length(`declawed`) == 1)) {
          stop(paste("Error! Invalid data for `declawed`. Must be a boolean:", `declawed`))
        }
        self$`declawed` <- `declawed`
      }
    },

    #' @description
    #' To JSON String
    #'
    #' @return Cat in JSON format
    toJSON = function() {
      CatObject <- list()
      if (!is.null(self$`className`)) {
        CatObject[["className"]] <-
          self$`className`
      }
      if (!is.null(self$`color`)) {
        CatObject[["color"]] <-
          self$`color`
      }
      if (!is.null(self$`declawed`)) {
        CatObject[["declawed"]] <-
          self$`declawed`
      }
      CatObject
    },

    #' @description
    #' Deserialize JSON string into an instance of Cat
    #'
    #' @param input_json the JSON input
    #' @return the instance of Cat
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
      }
      if (!is.null(this_object$`color`)) {
        self$`color` <- this_object$`color`
      }
      if (!is.null(this_object$`declawed`)) {
        self$`declawed` <- this_object$`declawed`
      }
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return Cat in JSON format
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`className`)) {
          sprintf(
          '"className":
            "%s"
                    ',
          self$`className`
          )
        },
        if (!is.null(self$`color`)) {
          sprintf(
          '"color":
            "%s"
                    ',
          self$`color`
          )
        },
        if (!is.null(self$`declawed`)) {
          sprintf(
          '"declawed":
            %s
                    ',
          tolower(self$`declawed`)
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },

    #' @description
    #' Deserialize JSON string into an instance of Cat
    #'
    #' @param input_json the JSON input
    #' @return the instance of Cat
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`className` <- this_object$`className`
      self$`color` <- this_object$`color`
      self$`declawed` <- this_object$`declawed`
      self
    },

    #' @description
    #' Validate JSON input with respect to Cat and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for Cat: the required field `className` is missing."))
      }
    },

    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Cat
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
# Cat$unlock()
#
## Below is an example to define the print function
# Cat$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Cat$lock()

