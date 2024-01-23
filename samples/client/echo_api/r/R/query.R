#' Create a new Query
#'
#' @description
#' Query Class
#'
#' @docType class
#' @title Query
#' @description Query Class
#' @format An \code{R6Class} generator object
#' @field id Query integer [optional]
#' @field outcomes  list(character) [optional]
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
Query <- R6::R6Class(
  "Query",
  public = list(
    `id` = NULL,
    `outcomes` = NULL,
    #' Initialize a new Query class.
    #'
    #' @description
    #' Initialize a new Query class.
    #'
    #' @param id Query
    #' @param outcomes outcomes. Default to ["SUCCESS","FAILURE"].
    #' @param ... Other optional arguments.
    #' @export
    initialize = function(`id` = NULL, `outcomes` = ["SUCCESS","FAILURE"], ...) {
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
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Query in JSON format
    #' @export
    toJSON = function() {
      QueryObject <- list()
      if (!is.null(self$`id`)) {
        QueryObject[["id"]] <-
          self$`id`
      }
      if (!is.null(self$`outcomes`)) {
        QueryObject[["outcomes"]] <-
          self$`outcomes`
      }
      QueryObject
    },
    #' Deserialize JSON string into an instance of Query
    #'
    #' @description
    #' Deserialize JSON string into an instance of Query
    #'
    #' @param input_json the JSON input
    #' @return the instance of Query
    #' @export
    fromJSON = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      if (!is.null(this_object$`id`)) {
        self$`id` <- this_object$`id`
      }
      if (!is.null(this_object$`outcomes`)) {
        self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      }
      self
    },
    #' To JSON string
    #'
    #' @description
    #' To JSON String
    #'
    #' @return Query in JSON format
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
        }
      )
      jsoncontent <- paste(jsoncontent, collapse = ",")
      json_string <- as.character(jsonlite::minify(paste("{", jsoncontent, "}", sep = "")))
    },
    #' Deserialize JSON string into an instance of Query
    #'
    #' @description
    #' Deserialize JSON string into an instance of Query
    #'
    #' @param input_json the JSON input
    #' @return the instance of Query
    #' @export
    fromJSONString = function(input_json) {
      this_object <- jsonlite::fromJSON(input_json)
      self$`id` <- this_object$`id`
      self$`outcomes` <- ApiClient$new()$deserializeObj(this_object$`outcomes`, "array[character]", loadNamespace("openapi"))
      self
    },
    #' Validate JSON input with respect to Query
    #'
    #' @description
    #' Validate JSON input with respect to Query and throw an exception if invalid
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
    #' @return String representation of Query
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
# Query$unlock()
#
## Below is an example to define the print function
# Query$set("public", "print", function(...) {
#   print(jsonlite::prettify(self$toJSONString()))
#   invisible(self)
# })
## Uncomment below to lock the class to prevent modifications to the method or field
# Query$lock()

