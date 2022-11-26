#' Create a new Category
#'
#' @description
#' A category for a pet
#'
#' @docType class
#' @title Category
#' @description Category Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field name  character [optional]
#' @field _field_list a list of fields list(character)
#' @field additional_properties additional properties list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Category <- R6::R6Class(
  "Category",
  public = list(
    `id` = NULL,
    `name` = NULL,
    `_field_list` = c("id", "name"),
    `additional_properties` = list(),
    #' Initialize a new Category class.
    #'
    #' @description
    #' Initialize a new Category class.
    #'
    #' @param id id
    #' @param name name
    #' @param additional_properties additional properties (optional)
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`id` = NULL, `name` = NULL, additional_properties = NULL, ...) {
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`name`)) {
        if (!(is.character(`name`) && length(`name`) == 1)) {
          stop(paste("Error! Invalid data for `name`. Must be a string:", `name`))
        }
        self$`name` <- `name`
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
    #' @return Category in JSON format
    #' @export
    toJSON = function() {
      CategoryObject <- list()
      if (!is.null(self$`id`)) {
        CategoryObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`name`)) {
        CategoryObject[["name"]] <-
          self$`name`
      }
      for (key in names(self$additional_properties)) {
        CategoryObject[[key]] <- self$additional_properties[[key]]
      }

      CategoryObject
    },
    #' Deserialize JSON string into an instance of Category
    #'
    #' @description
    #' Deserialize JSON string into an instance of Category
    #'
    #' @param input_json the JSON input
    #' @return the instance of Category
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
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
    #' @return Category in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`id`)) {
          sprintf(
          '"id":
            %d
                    ',
          self$`id`
          )
        },
        if (!is.null(self$`name`)) {
          sprintf(
          '"name":
            "%s"
                    ',
          self$`name`
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
    #' Deserialize JSON string into an instance of Category
    #'
    #' @description
    #' Deserialize JSON string into an instance of Category
    #'
    #' @param input_json the JSON input
    #' @return the instance of Category
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`name` <- this_object$`name`
      # process additional properties/fields in the payload
      for (key in names(this_object)) {
        if (!(key %in% self$`_field_list`)) { # json key not in list of fields
          self$additional_properties[[key]] <- this_object[[key]]
        }
      }

      self
    },
    #' Validate JSON input with respect to Category
    #'
    #' @description
    #' Validate JSON input with respect to Category and throw an exception if invalid
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
    #' @return String representation of Category
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
      if (!str_detect(self$`name`, "^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")) {
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
      if (!str_detect(self$`name`, "^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$")) {
        invalid_fields["name"] <- "Invalid value for `name`, must conform to the pattern ^[a-zA-Z0-9]+[a-zA-Z0-9\\.\\-_]*[a-zA-Z0-9]+$."
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
# Category$unlock()
#
## Below is an example to define the print function
# Category$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Category$lock()

