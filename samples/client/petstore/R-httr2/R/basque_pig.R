#' Create a new BasquePig
#'
#' @description
#' BasquePig Class
#'
#' @docType class
#' @title BasquePig
#' @description BasquePig Class
#' @format An \code{R6Class} generator object
#' @field className  character
#' @field color  character
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
BasquePig <- R6::R6Class(
  "BasquePig",
  public = list(
    `className` = NULL,
    `color` = NULL,
    #' Initialize a new BasquePig class.
    #'
    #' @description
    #' Initialize a new BasquePig class.
    #'
    #' @param className className
    #' @param color color
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`className`, `color`, ...) {
      if (!missing(`className`)) {
        if (!(is.character(`className`) && length(`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", `className`))
        }
        self$`className` <- `className`
      }
      if (!missing(`color`)) {
        if (!(is.character(`color`) && length(`color`) == 1)) {
          stop(paste("Error! Invalid data for `color`. Must be a string:", `color`))
        }
        self$`color` <- `color`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BasquePig in JSON format
    #' @export
    toJSON = function() {
      BasquePigObject <- list()
      if (!is.null(self$`className`)) {
        BasquePigObject[["className"]] <-
          self$`className`
      }
      if (!is.null(self$`color`)) {
        BasquePigObject[["color"]] <-
          self$`color`
      }
      BasquePigObject
    },
    #' Deserialize JSON string into an instance of BasquePig
    #'
    #' @description
    #' Deserialize JSON string into an instance of BasquePig
    #'
    #' @param input_json the JSON input
    #' @return the instance of BasquePig
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`className`)) {
        self$`className` <- this_object$`className`
      }
      if (!is.null(this_object$`color`)) {
        self$`color` <- this_object$`color`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return BasquePig in JSON format
    #' @export
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of BasquePig
    #'
    #' @description
    #' Deserialize JSON string into an instance of BasquePig
    #'
    #' @param input_json the JSON input
    #' @return the instance of BasquePig
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`className` <- this_object$`className`
      self$`color` <- this_object$`color`
      self
    },
    #' Validate JSON input with respect to BasquePig
    #'
    #' @description
    #' Validate JSON input with respect to BasquePig and throw an exception if invalid
    #'
    #' @param input the JSON input
    #' @export
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
      # check the required field `className`
      if (!is.null(input_json$`className`)) {
        if (!(is.character(input_json$`className`) && length(input_json$`className`) == 1)) {
          stop(paste("Error! Invalid data for `className`. Must be a string:", input_json$`className`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for BasquePig: the required field `className` is missing."))
      }
      # check the required field `color`
      if (!is.null(input_json$`color`)) {
        if (!(is.character(input_json$`color`) && length(input_json$`color`) == 1)) {
          stop(paste("Error! Invalid data for `color`. Must be a string:", input_json$`color`))
        }
      } else {
        stop(paste("The JSON input `", input, "` is invalid for BasquePig: the required field `color` is missing."))
      }
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of BasquePig
    #' @export
    toString = function() {
      self$toJSONString()
    },
    #' Return true if the values in all fields are valid.
    #'
    #' @description
    #' Return true if the values in all fields are valid.
    #'
    #' @return true if the values in all fields are valid.
    #' @export
    isValid = function() {
      # check if the required `className` is null
      if (is.null(self$`className`)) {
        return(FALSE)
      }

      # check if the required `color` is null
      if (is.null(self$`color`)) {
        return(FALSE)
      }

      TRUE
    },
    #' Return a list of invalid fields (if any).
    #'
    #' @description
    #' Return a list of invalid fields (if any).
    #'
    #' @return A list of invalid fields (if any).
    #' @export
    getInvalidFields = function() {
      invalid_fields <- list()
      # check if the required `className` is null
      if (is.null(self$`className`)) {
        invalid_fields["className"] <- "Non-nullable required field `className` cannot be null."
      }

      # check if the required `color` is null
      if (is.null(self$`color`)) {
        invalid_fields["color"] <- "Non-nullable required field `color` cannot be null."
      }

      invalid_fields
    },
    #' Print the object
    #'
    #' @description
    #' Print the object
    #'
    #' @export
    print = function() {
      print(jsonlite::prettify(self$toJSONString()))
      invisible(self)
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# BasquePig$unlock()
#
## Below is an example to define the print function
# BasquePig$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# BasquePig$lock()

