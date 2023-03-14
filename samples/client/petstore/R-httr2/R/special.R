#' Create a new Special
#'
#' @description
#' Describes the result of uploading an image resource
#'
#' @docType class
#' @title Special
#' @description Special Class
#' @format An \code{R6Class} generator object
#' @field set_test  list(character) [optional]
#' @field item_self  integer [optional]
#' @field item_private  character [optional]
#' @field item_super  character [optional]
#' @field 123_number  character [optional]
#' @field array[test]  character [optional]
#' @field empty_string  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Special <- R6::R6Class(
  "Special",
  public = list(
    `set_test` = NULL,
    `item_self` = NULL,
    `item_private` = NULL,
    `item_super` = NULL,
    `123_number` = NULL,
    `array[test]` = NULL,
    `empty_string` = NULL,
    #' Initialize a new Special class.
    #'
    #' @description
    #' Initialize a new Special class.
    #'
    #' @param set_test set_test
    #' @param item_self item_self
    #' @param item_private item_private
    #' @param item_super item_super
    #' @param 123_number 123_number
    #' @param array[test] array[test]
    #' @param empty_string empty_string
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`set_test` = NULL, `item_self` = NULL, `item_private` = NULL, `item_super` = NULL, `123_number` = NULL, `array[test]` = NULL, `empty_string` = NULL, ...) {
      if (!is.null(`set_test`)) {
        stopifnot(is.vector(`set_test`), length(`set_test`) != 0)
        sapply(`set_test`, function(x) stopifnot(is.character(x)))
        if (!identical(`set_test`, unique(`set_test`))) {
          stop("Error! Items in `set_test` are not unique.")
        }
        self$`set_test` <- `set_test`
      }
      if (!is.null(`item_self`)) {
        if (!(is.numeric(`item_self`) && length(`item_self`) == 1)) {
          stop(paste("Error! Invalid data for `item_self`. Must be an integer:", `item_self`))
        }
        self$`item_self` <- `item_self`
      }
      if (!is.null(`item_private`)) {
        if (!(is.character(`item_private`) && length(`item_private`) == 1)) {
          stop(paste("Error! Invalid data for `item_private`. Must be a string:", `item_private`))
        }
        self$`item_private` <- `item_private`
      }
      if (!is.null(`item_super`)) {
        if (!(is.character(`item_super`) && length(`item_super`) == 1)) {
          stop(paste("Error! Invalid data for `item_super`. Must be a string:", `item_super`))
        }
        self$`item_super` <- `item_super`
      }
      if (!is.null(`123_number`)) {
        if (!(is.character(`123_number`) && length(`123_number`) == 1)) {
          stop(paste("Error! Invalid data for `123_number`. Must be a string:", `123_number`))
        }
        self$`123_number` <- `123_number`
      }
      if (!is.null(`array[test]`)) {
        if (!(is.character(`array[test]`) && length(`array[test]`) == 1)) {
          stop(paste("Error! Invalid data for `array[test]`. Must be a string:", `array[test]`))
        }
        self$`array[test]` <- `array[test]`
      }
      if (!is.null(`empty_string`)) {
        if (!(is.character(`empty_string`) && length(`empty_string`) == 1)) {
          stop(paste("Error! Invalid data for `empty_string`. Must be a string:", `empty_string`))
        }
        self$`empty_string` <- `empty_string`
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
      if (!is.null(self$`set_test`)) {
        SpecialObject[["set_test"]] <-
          self$`set_test`
      }
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
      if (!is.null(this_object$`set_test`)) {
        self$`set_test` <- ApiClient$new()$deserializeObj(this_object$`set_test`, "set[character]", loadNamespace("petstore"))
        if (!identical(self$`set_test`, unique(self$`set_test`))) {
          stop("Error! Items in `set_test` are not unique.")
        }
      }
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
        if (!is.null(self$`set_test`)) {
          sprintf(
          '"set_test":
             [%s]
          ',
          paste(unlist(lapply(self$`set_test`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
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
      self$`set_test` <- ApiClient$new()$deserializeObj(this_object$`set_test`, "set[character]", loadNamespace("petstore"))
      if (!identical(self$`set_test`, unique(self$`set_test`))) {
        stop("Error! Items in `set_test` are not unique.")
      }
      self$`item_self` <- this_object$`item_self`
      self$`item_private` <- this_object$`item_private`
      self$`item_super` <- this_object$`item_super`
      self$`123_number` <- this_object$`123_number`
      self$`array[test]` <- this_object$`array[test]`
      self$`empty_string` <- this_object$`empty_string`
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
    }
  ),
  # Lock the class to prevent modifications to the method or field
  lock_class = TRUE
)
## Uncomment below to unlock the class to allow modifications of the method or field
# Special$unlock()
#
## Below is an example to define the print function
# Special$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Special$lock()

