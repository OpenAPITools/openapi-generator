#' Create a new AllofTagApiResponse
#'
#' @description
#' AllofTagApiResponse Class
#'
#' @docType class
#' @title AllofTagApiResponse
#' @description AllofTagApiResponse Class
#' @format An \code{R6Class} generator object
#' @field id  integer [optional]
#' @field name  character [optional]
#' @field code  integer [optional]
#' @field type  character [optional]
#' @field message  character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
AllofTagApiResponse <- R6::R6Class(
  "AllofTagApiResponse",
  public = list(
    `id` = NULL,
    `name` = NULL,
    `code` = NULL,
    `type` = NULL,
    `message` = NULL,
    #' Initialize a new AllofTagApiResponse class.
    #'
    #' @description
    #' Initialize a new AllofTagApiResponse class.
    #'
    #' @param id id
    #' @param name name
    #' @param code code
    #' @param type type
    #' @param message message
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`id` = NULL, `name` = NULL, `code` = NULL, `type` = NULL, `message` = NULL, ...) {
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
      if (!is.null(`code`)) {
        if (!(is.numeric(`code`) && length(`code`) == 1)) {
          stop(paste("Error! Invalid data for `code`. Must be an integer:", `code`))
        }
        self$`code` <- `code`
      }
      if (!is.null(`type`)) {
        if (!(is.character(`type`) && length(`type`) == 1)) {
          stop(paste("Error! Invalid data for `type`. Must be a string:", `type`))
        }
        self$`type` <- `type`
      }
      if (!is.null(`message`)) {
        if (!(is.character(`message`) && length(`message`) == 1)) {
          stop(paste("Error! Invalid data for `message`. Must be a string:", `message`))
        }
        self$`message` <- `message`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AllofTagApiResponse in JSON format
    #' @export
    toJSON = function() {
      AllofTagApiResponseObject <- list()
      if (!is.null(self$`id`)) {
        AllofTagApiResponseObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`name`)) {
        AllofTagApiResponseObject[["name"]] <-
          self$`name`
      }
      if (!is.null(self$`code`)) {
        AllofTagApiResponseObject[["code"]] <-
          self$`code`
      }
      if (!is.null(self$`type`)) {
        AllofTagApiResponseObject[["type"]] <-
          self$`type`
      }
      if (!is.null(self$`message`)) {
        AllofTagApiResponseObject[["message"]] <-
          self$`message`
      }
      AllofTagApiResponseObject
    },
    #' Deserialize JSON string into an instance of AllofTagApiResponse
    #'
    #' @description
    #' Deserialize JSON string into an instance of AllofTagApiResponse
    #'
    #' @param input_json the JSON input
    #' @return the instance of AllofTagApiResponse
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`name`)) {
        self$`name` <- this_object$`name`
      }
      if (!is.null(this_object$`code`)) {
        self$`code` <- this_object$`code`
      }
      if (!is.null(this_object$`type`)) {
        self$`type` <- this_object$`type`
      }
      if (!is.null(this_object$`message`)) {
        self$`message` <- this_object$`message`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return AllofTagApiResponse in JSON format
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
        },
        if (!is.null(self$`code`)) {
          sprintf(
          '"code":
            %d
                    ',
          self$`code`
          )
        },
        if (!is.null(self$`type`)) {
          sprintf(
          '"type":
            "%s"
                    ',
          self$`type`
          )
        },
        if (!is.null(self$`message`)) {
          sprintf(
          '"message":
            "%s"
                    ',
          self$`message`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of AllofTagApiResponse
    #'
    #' @description
    #' Deserialize JSON string into an instance of AllofTagApiResponse
    #'
    #' @param input_json the JSON input
    #' @return the instance of AllofTagApiResponse
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`name` <- this_object$`name`
      self$`code` <- this_object$`code`
      self$`type` <- this_object$`type`
      self$`message` <- this_object$`message`
      self
    },
    #' Validate JSON input with respect to AllofTagApiResponse
    #'
    #' @description
    #' Validate JSON input with respect to AllofTagApiResponse and throw an exception if invalid
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
    #' @return String representation of AllofTagApiResponse
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
# AllofTagApiResponse$unlock()
#
## Below is an example to define the print function
# AllofTagApiResponse$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# AllofTagApiResponse$lock()

