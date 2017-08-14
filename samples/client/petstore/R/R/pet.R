#' Pet Class
#'
#' Pet Class
#' @export
Pet  <- R6::R6Class(
  'Pet',
  public = list(
    id = NULL,
    category = NULL,
    name = NULL,
    photoUrls = NULL,
    tags = NULL,
    status = NULL,
    initialize = function(id,category,name,photoUrls,tags,status){
      if (!missing(id)) {
        stopifnot(is.numeric(id), length(id) == 1)
        self$id <- id
      }
      if (!missing(category)) {
        stopifnot("Element" %in% class(category), !is.list(category))
        self$category <- category
      }
      if (!missing(name)) {
        stopifnot(is.character(name), length(name) == 1)
        self$name <- name
      }
      if (!missing(photoUrls)) {
        stopifnot(is.list(photoUrls), length(photoUrls) != 0)
        lapply(photoUrls, function(x) stopifnot(is.character(x)))
        self$photoUrls <- photoUrls
      }
      if (!missing(tags)) {
        stopifnot(is.list(tags), length(tags) != 0)
        lapply(tags, function(x) stopifnot("Element" %in% class(x), !is.list(x)))
        self$tags <- tags
      }
      if (!missing(status)) {
        stopifnot(is.character(status), length(status) == 1)
        self$status <- status
      }
    },
    toJson = function() { 
       sprintf(
        '{
           "id": %d,
           "category": {
              "id": %d,
              "name": "%s"
           },
           "name": "%s",
           "photoUrls": [%s],
          "tags": [%s],
          "status": "%s"
        }',
        self$id,
        self$category$id,
        self$category$name,
        self$name,
        lapply(self$photoUrls, function(x) paste(paste0('"', x, '"'), sep=",")),
        lapply(self$tags, function(x) paste(x$toJson(), sep=",")),
        self$status)
    }
  )
)

#' Element Class
#'
#' Element Class
#' @export
Element  <- R6::R6Class(
  'Element',
  public = list(
    id = NULL,
    name = NULL,
    initialize = function(id,name){
      if (!missing(id)) {
        stopifnot(is.numeric(id), length(id) == 1)
        self$id <- id
      }
      if (!missing(name)) {
        stopifnot(is.character(name), length(name) == 1)
        self$name <- name
      }
    },
    toJson = function() { 
       sprintf('{"id":%d,"name":"%s"}', self$id, self$name)
    }
  )
)

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