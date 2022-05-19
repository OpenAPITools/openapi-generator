#' @docType class
#' @title ApiException
#' @description ApiException Class
#' @format An \code{R6Class} generator object
#' @field status Status of the ApiException
#' @field reason Reason of the ApiException
#' @field body Body of the http response
#' @field headers Headers of the http response
#' @field errorObjectType error object type
#' @export
ApiException <- R6::R6Class(
  "ApiException",
  public = list(
    status = NULL,
    reason = NULL,
    body = NULL,
    headers = NULL,
    errorObject = NULL,

    initialize = function(status = NULL, reason = NULL, http_response = NULL) {
      if (!is.null(http_response)) {
        self$status <- http_response$status_code
        errorMsg <- toString(content(http_response))
        if(errorMsg == ""){
          errorMsg <- "Api exception encountered."
        }
        self$body <- errorMsg
        self$headers <- http_response$headers
        self$reason <- httr::http_status(http_response)$reason
        self$errorObject <- ApiResponse$fromJsonString(errorMsg)
      } else {
        self$status <- status
        self$reason <- reason
        self$body <- NULL
        self$headers <- NULL
        self$errorObject <- NULL
      }
    },

    # returns the string format of ApiException
    toString = function() {
      errorMsg <- ""
      errorMsg <- paste("status : ", self$status, "\n", sep = "")
      errorMsg <- paste(errorMsg, "Reason : ", self$reason, "\n", sep = "")
      if (!is.null(self$headers)) {
        errorMsg <- paste(errorMsg, "Headers : ", "\n", sep = "")
        for (name in names(self$headers)) {
          errorMsg <- paste(errorMsg, name, " : ", self$headers[[name]], "\n", sep = " ")
        }
      }
      if (!is.null(self$body)) {
        errorMsg <- paste(errorMsg, "Body : ", "\n", sep = "")
        errorMsg <- paste(errorMsg, self$body,"\n")
      }
      if (!is.null(self$errorObject)) {
        errorMsg <- paste(errorMsg, "Error object : ", "\n", sep = "")
        errorMsg <- paste(errorMsg, self$errorObject,"\n")
      }
      errorMsg
    }
  )
)
