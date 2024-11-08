#' @docType class
#' @title StringEnumRef
#' @description StringEnumRef Class
#' @format An \code{R6Class} generator object
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON toJSON
#' @export
StringEnumRef <- R6::R6Class(
  "StringEnumRef",
  public = list(

    #' @description
    #' Initialize a new StringEnumRef class.
    #'
    #' @param ... Optional arguments.
    initialize = function(...) {
      local.optional.var <- list(...)
      val <- unlist(local.optional.var)
      enumvec <- .parse_StringEnumRef()

      if (length(val) == 0L) {
        val = "DUMMY_ENUM"
      } else {
        stopifnot(length(val) == 1L)
      }

      if (!val %in% enumvec) {
        if (!(val=="DUMMY_ENUM")) {
          stop("Use one of the valid values: ",
            paste0(enumvec, collapse = ", "))
        }
        warning("Initializing StringEnumRef with DUMMY_ENUM. Use one of the valid values: ",
          paste0(enumvec, collapse = ", "),
          ". If you did not manually initialize StringEnumRef, this may already be overwritten by an enum loaded from a JSON config.")
      }
      private$value <- val
    },

    #' @description
    #' To JSON String
    #'
    #' @return StringEnumRef in JSON format
    toJSON = function() {
        jsonlite::toJSON(private$value, auto_unbox = TRUE)
    },

    #' @description
    #' Deserialize JSON string into an instance of StringEnumRef
    #'
    #' @param input_json the JSON input
    #'
    #' @return the instance of StringEnumRef
    fromJSON = function(input_json) {
      private$value <- jsonlite::fromJSON(input_json,
          simplifyVector = FALSE)
      self
    },

    #' @description
    #' To JSON String
    #'
    #' @return StringEnumRef in JSON format
    toJSONString = function() {
      as.character(jsonlite::toJSON(private$value,
          auto_unbox = TRUE))
    },

    #' @description
    #' Deserialize JSON string into an instance of StringEnumRef
    #'
    #' @param input_json the JSON input
    #'
    #' @return the instance of StringEnumRef
    fromJSONString = function(input_json) {
      private$value <- jsonlite::fromJSON(input_json,
          simplifyVector = FALSE)
      self
    }
  ),
  private = list(
    value = NULL
  )
)

# add to utils.R
.parse_StringEnumRef <- function(vals) {
  res <- gsub("^\\[|\\]$", "", "[success, failure, unclassified]")
  unlist(strsplit(res, ", "))
}

