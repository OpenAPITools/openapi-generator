#' Create a new NestedOneOf
#'
#' @description
#' NestedOneOf Class
#'
#' @docType class
#' @title NestedOneOf
#' @description NestedOneOf Class
#' @format An \code{R6Class} generator object
#' @field size  integer [optional]
#' @field nested_pig  \link{Pig} [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
NestedOneOf <- R6::R6Class(
  "NestedOneOf",
  public = list(
    `size` = NULL,
    `nested_pig` = NULL,
    `_field_list` = c("size", "nested_pig"),
    `additional_properties` = list(),
    #' Initialize a new NestedOneOf class.
    #'
    #' @description
    #' Initialize a new NestedOneOf class.
    #'
    #' @param size size
    #' @param nested_pig nested_pig
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`size` = NULL, `nested_pig` = NULL, additional_properties = NULL, ...) {
      if (!is.null(`size`)) {
        if (!(is.numeric(`size`) && length(`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be an integer:", `size`))
        }
        self$`size` <- `size`
      }
      if (!is.null(`nested_pig`)) {
        stopifnot(R6::is.R6(`nested_pig`))
        self$`nested_pig` <- `nested_pig`
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
    #' @return NestedOneOf in JSON format
    #' @export
    toJSON = function() {
      NestedOneOfObject <- list()
      if (!is.null(self$`size`)) {
        NestedOneOfObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`nested_pig`)) {
        NestedOneOfObject[["nested_pig"]] <-
          self$`nested_pig`$toJSON()
      }
      for (key in names(self$additional_properties)) {
        NestedOneOfObject[[key]] <- self$additional_properties[[key]]
      }

      NestedOneOfObject
    },
    #' Deserialize JSON string into an instance of NestedOneOf
    #'
    #' @description
    #' Deserialize JSON string into an instance of NestedOneOf
    #'
    #' @param input_json the JSON input
    #' @return the instance of NestedOneOf
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      if (!is.null(this_object$`nested_pig`)) {
        `nested_pig_object` <- Pig$new()
        `nested_pig_object`$fromJSON(jsonlite::toJSON(this_object$`nested_pig`, auto_unbox = TRUE, digits = NA))
        self$`nested_pig` <- `nested_pig_object`
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
    #' @return NestedOneOf in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`size`)) {
          sprintf(
          '"size":
            %d
                    ',
          self$`size`
          )
        },
        if (!is.null(self$`nested_pig`)) {
          sprintf(
          '"nested_pig":
          %s
          ',
          jsonlite::toJSON(self$`nested_pig`$toJSON(), auto_unbox = TRUE, digits = NA)
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
    #' Deserialize JSON string into an instance of NestedOneOf
    #'
    #' @description
    #' Deserialize JSON string into an instance of NestedOneOf
    #'
    #' @param input_json the JSON input
    #' @return the instance of NestedOneOf
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`size` <- this_object$`size`
      self$`nested_pig` <- Pig$new()$fromJSON(jsonlite::toJSON(this_object$`nested_pig`, auto_unbox = TRUE, digits = NA))
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },
    #' Validate JSON input with respect to NestedOneOf
    #'
    #' @description
    #' Validate JSON input with respect to NestedOneOf and throw an exception if invalid
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
    #' @return String representation of NestedOneOf
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
# NestedOneOf$unlock()
#
## Below is an example to define the print function
# NestedOneOf$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# NestedOneOf$lock()

