#' ApiResponse Class
#'
#' ApiResponse Class
#' @export
ApiResponse  <- R6::R6Class(
  'ApiResponse',
  public = list(
    content = NULL,
    response = NULL,
    initialize = function(content, response){
      self$content <- content
      self$response <- response
    }
  )
)
