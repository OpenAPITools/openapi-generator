#' Create a new DataQuery
#'
#' @description
#' DataQuery Class
#'
#' @docType class
#' @title DataQuery
#' @description DataQuery Class
#' @format An \code{R6Class} generator object
#' @field id Query integer [optional]
#' @field outcomes  list(character) [optional]
#' @field suffix test suffix character [optional]
#' @field text Some text containing white spaces character [optional]
#' @field date A date character [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
DataQuery <- R6::R6Class(
  "DataQuery",
  inherit = Query,
  public = list(
    `id` = NULL,
    `outcomes` = NULL,
    `suffix` = NULL,
    `text` = NULL,
    `date` = NULL,
    #' Initialize a new DataQuery class.
    #'
    #' @description
    #' Initialize a new DataQuery class.
    #'
    #' @param id Query
    #' @param outcomes outcomes. Default to [SUCCESS, FAILURE].
    #' @param suffix test suffix
    #' @param text Some text containing white spaces
    #' @param date A date
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`id` = NULL, `outcomes` = [SUCCESS, FAILURE], `suffix` = NULL, `text` = NULL, `date` = NULL, ...) {
      if (!is.null(`id`)) {
        if (!(is.numeric(`id`) && length(`id`) == 1)) {
          stop(paste("Error! Invalid data for `id`. Must be an integer:", `id`))
        }
        self$`id` <- `id`
      }
      if (!is.null(`outcomes`)) {
        stopifnot(is.vector(`outcomes`), length(`outcomes`) != 0)
        sapply(`outcomes`, function(x) stopifnot(is.character(x)))
        self$`outcomes` <- `outcomes`
      }
      if (!is.null(`suffix`)) {
        if (!(is.character(`suffix`) && length(`suffix`) == 1)) {
          stop(paste("Error! Invalid data for `suffix`. Must be a string:", `suffix`))
        }
        self$`suffix` <- `suffix`
      }
      if (!is.null(`text`)) {
        if (!(is.character(`text`) && length(`text`) == 1)) {
          stop(paste("Error! Invalid data for `text`. Must be a string:", `text`))
        }
        self$`text` <- `text`
      }
      if (!is.null(`date`)) {
        if (!is.character(`date`)) {
          stop(paste("Error! Invalid data for `date`. Must be a string:", `date`))
        }
        self$`date` <- `date`
      }
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DataQuery in JSON format
    #' @export
    toJSON = function() {
      DataQueryObject <- list()
      if (!is.null(self$`id`)) {
        DataQueryObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`outcomes`)) {
        DataQueryObject[["outcomes"]] <-
          self$`outcomes`
      }
      if (!is.null(self$`suffix`)) {
        DataQueryObject[["suffix"]] <-
          self$`suffix`
      }
      if (!is.null(self$`text`)) {
        DataQueryObject[["text"]] <-
          self$`text`
      }
      if (!is.null(self$`date`)) {
        DataQueryObject[["date"]] <-
          self$`date`
      }
      DataQueryObject
    },
    #' Deserialize JSON string into an instance of DataQuery
    #'
    #' @description
    #' Deserialize JSON string into an instance of DataQuery
    #'
    #' @param input_json the JSON input
    #' @return the instance of DataQuery
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`outcomes`)) {
        self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      }
      if (!is.null(this_object$`suffix`)) {
        self$`suffix` <- this_object$`suffix`
      }
      if (!is.null(this_object$`text`)) {
        self$`text` <- this_object$`text`
      }
      if (!is.null(this_object$`date`)) {
        self$`date` <- this_object$`date`
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return DataQuery in JSON format
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
        if (!is.null(self$`outcomes`)) {
          sprintf(
          '"outcomes":
             [%s]
          ',
          paste(unlist(lapply(self$`outcomes`, function(x) paste0('"', x, '"'))), collapse = ",")
          )
        },
        if (!is.null(self$`suffix`)) {
          sprintf(
          '"suffix":
            "%s"
                    ',
          self$`suffix`
          )
        },
        if (!is.null(self$`text`)) {
          sprintf(
          '"text":
            "%s"
                    ',
          self$`text`
          )
        },
        if (!is.null(self$`date`)) {
          sprintf(
          '"date":
            "%s"
                    ',
          self$`date`
          )
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of DataQuery
    #'
    #' @description
    #' Deserialize JSON string into an instance of DataQuery
    #'
    #' @param input_json the JSON input
    #' @return the instance of DataQuery
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      self$`suffix` <- this_object$`suffix`
      self$`text` <- this_object$`text`
      self$`date` <- this_object$`date`
      self
    },
    #' Validate JSON input with respect to DataQuery
    #'
    #' @description
    #' Validate JSON input with respect to DataQuery and throw an exception if invalid
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
    #' @return String representation of DataQuery
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
# DataQuery$unlock()
#
## Below is an example to define the print function
# DataQuery$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# DataQuery$lock()

