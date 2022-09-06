#' Create a new Special
#'
#' @description
#' Describes the result of uploading an image resource
#'
#' @docType class
#' @title Special
#' @description Special Class
#' @format An \code{R6Class} generator object
#' @field item_self  integer [optional]
#' @field item_private  character [optional]
#' @field item_super  character [optional]
#' @field 123_number  character [optional]
#' @field array[test]  character [optional]
#' @field empty_string  character [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Special <- R6::R6Class(
  "Special",
  public = list(
    `item_self` = NULL,
    `item_private` = NULL,
    `item_super` = NULL,
    `123_number` = NULL,
    `array[test]` = NULL,
    `empty_string` = NULL,
    `_field_list` = c("item_self", "item_private", "item_super", "123_number", "array[test]", "empty_string"),
    `additional_properties` = list(),
    #' Initialize a new Special class.
    #'
    #' @description
    #' Initialize a new Special class.
    #'
    #' @param item_self item_self
    #' @param item_private item_private
    #' @param item_super item_super
    #' @param 123_number 123_number
    #' @param array[test] array[test]
    #' @param empty_string empty_string
    #' @param additional_properties additonal properties (optional)
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(
        `item_self` = NULL, `item_private` = NULL, `item_super` = NULL, `123_number` = NULL, `array[test]` = NULL, `empty_string` = NULL, additional_properties = NULL, ...
    ) {
      if (!is.null(`item_self`)) {
        stopifnot(is.numeric(`item_self`), length(`item_self`) == 1)
        self$`item_self` <- `item_self`
      }
      if (!is.null(`item_private`)) {
        stopifnot(is.character(`item_private`), length(`item_private`) == 1)
        self$`item_private` <- `item_private`
      }
      if (!is.null(`item_super`)) {
        stopifnot(is.character(`item_super`), length(`item_super`) == 1)
        self$`item_super` <- `item_super`
      }
      if (!is.null(`123_number`)) {
        stopifnot(is.character(`123_number`), length(`123_number`) == 1)
        self$`123_number` <- `123_number`
      }
      if (!is.null(`array[test]`)) {
        stopifnot(is.character(`array[test]`), length(`array[test]`) == 1)
        self$`array[test]` <- `array[test]`
      }
      if (!is.null(`empty_string`)) {
        stopifnot(is.character(`empty_string`), length(`empty_string`) == 1)
        self$`empty_string` <- `empty_string`
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
    #' @return Special in JSON format
    #' @export
    toJSON = function() {
      SpecialObject <- list()
      if (!is.null(self$`item_self`)) {
        SpecialObject[["self"]] <-
          self$`item_self`
      }
      if (!is.null(self$`item_private`)) {
        SpecialObject[["private"]] <-
          self$`item_private`
      }
      if (!is.null(self$`item_super`)) {
        SpecialObject[["super"]] <-
          self$`item_super`
      }
      if (!is.null(self$`123_number`)) {
        SpecialObject[["123_number"]] <-
          self$`123_number`
      }
      if (!is.null(self$`array[test]`)) {
        SpecialObject[["array[test]"]] <-
          self$`array[test]`
      }
      if (!is.null(self$`empty_string`)) {
        SpecialObject[["empty_string"]] <-
          self$`empty_string`
      }
      for (key in names(self$additional_properties)) {
        SpecialObject[[key]] <- self$additional_properties[[key]]
      }

      SpecialObject
    },
    #' Deserialize JSON string into an instance of Special
    #'
    #' @description
    #' Deserialize JSON string into an instance of Special
    #'
    #' @param input_json the JSON input
    #' @return the instance of Special
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`self`)) {
        self$`item_self` <- this_object$`self`
      }
      if (!is.null(this_object$`private`)) {
        self$`item_private` <- this_object$`private`
      }
      if (!is.null(this_object$`super`)) {
        self$`item_super` <- this_object$`super`
      }
      if (!is.null(this_object$`123_number`)) {
        self$`123_number` <- this_object$`123_number`
      }
      if (!is.null(this_object$`array[test]`)) {
        self$`array[test]` <- this_object$`array[test]`
      }
      if (!is.null(this_object$`empty_string`)) {
        self$`empty_string` <- this_object$`empty_string`
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
    #' @return Special in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`item_self`)) {
          sprintf(
          '"self":
            %d
                    ',
          self$`item_self`
          )
        },
        if (!is.null(self$`item_private`)) {
          sprintf(
          '"private":
            "%s"
                    ',
          self$`item_private`
          )
        },
        if (!is.null(self$`item_super`)) {
          sprintf(
          '"super":
            "%s"
                    ',
          self$`item_super`
          )
        },
        if (!is.null(self$`123_number`)) {
          sprintf(
          '"123_number":
            "%s"
                    ',
          self$`123_number`
          )
        },
        if (!is.null(self$`array[test]`)) {
          sprintf(
          '"array[test]":
            "%s"
                    ',
          self$`array[test]`
          )
        },
        if (!is.null(self$`empty_string`)) {
          sprintf(
          '"empty_string":
            "%s"
                    ',
          self$`empty_string`
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
    #' Deserialize JSON string into an instance of Special
    #'
    #' @description
    #' Deserialize JSON string into an instance of Special
    #'
    #' @param input_json the JSON input
    #' @return the instance of Special
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`item_self` <- this_object$`item_self`
      self$`item_private` <- this_object$`item_private`
      self$`item_super` <- this_object$`item_super`
      self$`123_number` <- this_object$`123_number`
      self$`array[test]` <- this_object$`array[test]`
      self$`empty_string` <- this_object$`empty_string`
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },
    #' Validate JSON input with respect to Special
    #'
    #' @description
    #' Validate JSON input with respect to Special and throw an exception if invalid
    #'
    #' @param input the JSON input
    #' @export
    validateJSON = function(input) {
      input_json <- jsonlite::fromJSON(input)
    },
    #' To string (JSON format)
    #'
    #' @description
    #' To string (JSON format)
    #'
    #' @return String representation of Special
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
    }),
    # Lock the class to prevent modifications to the method or field
    lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
#Special$unlock()
#
## Below is an example to define the print fnuction
#Special$set("public", "print", function(...) {
#  print(jsonlite::prettify(self$toJSONString()))
#  invisible(self)
#})
## Uncomment below to lock the class to prevent modifications to the method or field
#Special$lock()

