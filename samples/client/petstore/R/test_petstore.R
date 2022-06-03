library(petstore)

var_pet_id <- 56 # integer | ID of pet to return

#Find pet by ID
api_instance <- PetApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$GetPetById(var_pet_id),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if(!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # deserialized response object
  response.object <- result$content
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}

