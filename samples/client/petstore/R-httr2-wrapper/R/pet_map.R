#' Create a new PetMap
#'
#' @description
#' A mock map of a pet and some properties
#'
#' @docType class
#' @title PetMap
#' @description PetMap Class
#' @format An \code{R6Class} generator object
#' @field pet  named list(character) [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
PetMap <- R6::R6Class(
  "PetMap",
  public = list(
    `pet` = NULL,
    `_field_list` = c("pet"),
    `additional_properties` = list(),
    #' Initialize a new PetMap class.
    #'
    #' @description
    #' Initialize a new PetMap class.
    #'
    #' @param pet pet
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`pet` = NULL, additional_properties = NULL, ...) {
      if (!is.null(`pet`)) {
        stopifnot(is.vector(`pet`), length(`pet`) != 0)
        sapply(`pet`, function(x) stopifnot(is.character(x)))
        self$`pet` <- `pet`
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
    #' @return PetMap in JSON format
    #' @export
    toJSON = function() {
      PetMapObject <- list()
      if (!is.null(self$`pet`)) {
        PetMapObject[["pet"]] <-
          self$`pet`
      }
      for (key in names(self$additional_properties)) {
        PetMapObject[[key]] <- self$additional_properties[[key]]
      }

      PetMapObject
    },
    #' Deserialize JSON string into an instance of PetMap
    #'
    #' @description
    #' Deserialize JSON string into an instance of PetMap
    #'
    #' @param input_json the JSON input
    #' @return the instance of PetMap
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`pet`)) {
        self$`pet` <- ApiClient$new()$deserializeObj(this_object$`pet`, "map(character)", loadNamespace("petstore"))
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
    #' @return PetMap in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`pet`)) {
          sprintf(
          '"pet":
            %s
          ',
          jsonlite::toJSON(lapply(self$`pet`, function(x){ x }), auto_unbox = TRUE, digits = NA)
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
    #' Deserialize JSON string into an instance of PetMap
    #'
    #' @description
    #' Deserialize JSON string into an instance of PetMap
    #'
    #' @param input_json the JSON input
    #' @return the instance of PetMap
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`pet` <- ApiClient$new()$deserializeObj(this_object$`pet`, "map(character)", loadNamespace("petstore"))
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },
    #' Validate JSON input with respect to PetMap
    #'
    #' @description
    #' Validate JSON input with respect to PetMap and throw an exception if invalid
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
    #' @return String representation of PetMap
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
# PetMap$unlock()
#
## Below is an example to define the print function
# PetMap$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# PetMap$lock()

