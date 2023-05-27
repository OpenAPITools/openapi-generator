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
    #' Initialize a new Whale class.
    #'
    #' @description
    #' Initialize a new Whale class.
    #'
    #' @param className className
    #' @param hasBaleen hasBaleen
    #' @param hasTeeth hasTeeth
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    #' @export
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
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Whale in JSON format
    #' @export
    toJSON = function() {
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

      WhaleObject
    },
    #' Deserialize JSON string into an instance of Whale
    #'
    #' @description
    #' Deserialize JSON string into an instance of Whale
    #'
    #' @param input_json the JSON input
    #' @return the instance of Whale
    #' @export
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
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Whale in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`hasBaleen`)) {
          sprintf(
          '"hasBaleen":
            %s
                    ',
          tolower(self$`hasBaleen`)
          )
        },
        if (!is.null(self$`hasTeeth`)) {
          sprintf(
          '"hasTeeth":
            %s
                    ',
          tolower(self$`hasTeeth`)
          )
        },
        if (!is.null(self$`className`)) {
          sprintf(
          '"className":
            "%s"
                    ',
          self$`className`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
      json_obj <- jsonlite::fromJSON(json_string)
      for (key in names(self$additional_properties)) {
        json_obj[[key]] <- self$additional_properties[[key]]
      }
      json_string <- as.character(jsonlite::minify(jsonlite::toJSON(json_obj, auto_unbox = TRUE, digits = NA)))
    },
    #' Deserialize JSON string into an instance of Whale
    #'
    #' @description
    #' Deserialize JSON string into an instance of Whale
    #'
    #' @param input_json the JSON input
    #' @return the instance of Whale
    #' @export
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
    #' Validate JSON input with respect to Whale
    #'
    #' @description
    #' Validate JSON input with respect to Whale and throw an exception if invalid
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
        stop(paste("The JSON input `", input, "` is invalid for Whale: the required field `className` is missing."))
      }
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Whale
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
# Whale$unlock()
#
## Below is an example to define the print function
# Whale$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Whale$lock()

