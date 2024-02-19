#' Create a new TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
#'
#' @description
#' TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter Class
#'
#' @docType class
#' @title TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
#' @description TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter Class
#' @format An \code{R6Class} generator object
#' @field size  character [optional]
#' @field color  character [optional]
#' @field id  integer [optional]
#' @field name  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter <- R6::R6Class(
  "TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter",
  public = list(
    `size` = NULL,
    `color` = NULL,
    `id` = NULL,
    `name` = NULL,
    #' Initialize a new TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter class.
    #'
    #' @description
    #' Initialize a new TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter class.
    #'
    #' @param size size
    #' @param color color
    #' @param id id
    #' @param name name
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`size` = NULL, `color` = NULL, `id` = NULL, `name` = NULL, ...) {
      if (!is.null(`size`)) {
        if (!(is.character(`size`) && length(`size`) == 1)) {
          stop(paste("Error! Invalid data for `size`. Must be a string:", `size`))
        }
        self$`size` <- `size`
      }
      if (!is.null(`color`)) {
        if (!(is.character(`color`) && length(`color`) == 1)) {
          stop(paste("Error! Invalid data for `color`. Must be a string:", `color`))
        }
        self$`color` <- `color`
      }
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
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter in JSON format
    #' @export
    toJSON = function() {
      TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameterObject <- list()
      if (!is.null(self$`size`)) {
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameterObject[["size"]] <-
          self$`size`
      }
      if (!is.null(self$`color`)) {
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameterObject[["color"]] <-
          self$`color`
      }
      if (!is.null(self$`id`)) {
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameterObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`name`)) {
        TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameterObject[["name"]] <-
          self$`name`
      }
      TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameterObject
    },
    #' Deserialize JSON string into an instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #'
    #' @description
    #' Deserialize JSON string into an instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`size`)) {
        self$`size` <- this_object$`size`
      }
      if (!is.null(this_object$`color`)) {
        self$`color` <- this_object$`color`
      }
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter in JSON format
    #' @export
    toJSONString = function() {
      jsoncontent <- c(
        if (!is.null(self$`size`)) {
          sprintf(
          '"size":
            "%s"
                    ',
          self$`size`
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
    },
    #' Deserialize JSON string into an instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #'
    #' @description
    #' Deserialize JSON string into an instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #'
    #' @param input_json the JSON input
    #' @return the instance of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`size` <- this_object$`size`
      self$`color` <- this_object$`color`
      self$`id` <- this_object$`id`
      self$`name` <- this_object$`name`
      self
    },
    #' Validate JSON input with respect to TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
    #'
    #' @description
    #' Validate JSON input with respect to TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter and throw an exception if invalid
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
    #' @return String representation of TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
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
# TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter$unlock()
#
## Below is an example to define the print function
# TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter$lock()

