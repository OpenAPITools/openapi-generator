#' Response Class
#'
#' Response Class
#' @export
Response  <- R6::R6Class(
  'Response',
  public = list(
    content = NULL,
    response = NULL,
    initialize = function(content, response){
      self$content <- content
      self$response <- response
    }
  )
)