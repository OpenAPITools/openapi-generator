#' Pet Store Client Class
#'
#' Pet Store Client Class
#' @export
PetStoreClient <- R6::R6Class(
  'PetStoreClient',
  public = list(
    host = NULL,
    basePath = NULL,
    scheme = NULL,
    url = NULL,
    initialize = function(host, basePath, scheme){
      self$host <- host
      self$basePath <- basePath
      self$scheme <- scheme
      self$url <- sprintf("%s://%s/%s/pet/", scheme, host, basePath)
    },
    getPetById = function(petId){
      resp <- httr::GET(paste0(self$url, petId), httr::add_headers("accept" = "application/json"))
      if (httr::http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }

      if (httr::status_code(resp) == 200) 
      {
        parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                                 simplifyVector = FALSE)
        pet <- Pet$new(parsed$id, 
                       Element$new(parsed$category$id, parsed$category$name),
                       parsed$name,
                       parsed$photoUrls,
                       lapply(parsed$tags, function(x) Element$new(x$id, x$name)),
                       parsed$status)
        Response$new(pet, resp)     
      } 
      else if(httr::status_code(resp) == 400)
      {
        Response$new("Invalid ID supplied", resp)
      }
      else if(httr::status_code(resp) == 404)
      {
        Response$new("Pet not found", resp)
      }
      else 
      {
        Response$new("Unexpected response status code", resp)
      }
    },
    updatePetWithForm = function(petId, name, status){
      resp <- httr::POST(paste0(self$url, petId), 
                         httr::add_headers("accept" = "application/json",
                                           "content-type" = "application/x-www-form-urlencoded"),
                         body = list(name = name,
                                     status = status))
      if (httr::http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }
  
      if (httr::status_code(resp) == 200) 
      {
        Response$new("Pet updated", resp)
      } 
      else if(httr::status_code(resp) == 405)
      {
        Response$new("Invalid input", resp)
      }
      else 
      {
        Response$new("Unexpected response status code", resp)
      }
    },
    addPet = function(pet){
      resp <- httr::POST(self$url, 
                         httr::add_headers("accept" = "application/json",
                                           "content-type" = "application/json"),
                         body = pet$toJson())
      if (httr::http_type(resp) != "application/json") {
        stop("API did not return json", call. = FALSE)
      }
  
      if (httr::status_code(resp) == 200) 
      {
        Response$new("Pet added", resp)
      } 
      else if(httr::status_code(resp) == 405)
      {
        Response$new("Invalid input", resp)
      }
      else 
      {
        Response$new("Unexpected response status code", resp)
      }
    }
  )
)