
install.packages("petstore_1.0.0.tar.gz",repos=NULL, type="source")
library(petstore)

#errorMsg <- "{\"code\":1,\"type\":\"error\",\"message\":\"Pet not found\"}"
##errorMsg <- '{"code": 404, "message": "Not found"}'
#a <- ModelApiResponse$new()$fromJSONString(errorMsg)
#dput(a)
#
#var_pet_id <- 1231256 # integer | ID of pet to return
#
##Find pet by ID
#api_instance <- PetApi$new()
## Configure API key authorization: api_key
#api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
#result <- tryCatch(
#             api_instance$GetPetById(var_pet_id),
#             ApiException = function(ex) ex
#          )
## In case of error, print the error object
#if(!is.null(result$ApiException)) {
#  cat(result$ApiException$toString())
#} else {
#  # deserialized response object
#  response.object <- result$content
#  # response headers
#  response.headers <- result$response$headers
#  # response status code
#  response.status.code <- result$response$status_code
#}

json2 <-
'{"name": "pet", "photoUrls" : ["http://a.com", "http://b.com"]}'

jsonlite::minify(json2)

pet_api <- PetApi$new()
pet_id <- 123321
pet <- Pet$new("name_test",
  photoUrls = list("photo_test", "second test"),
  category = Category$new(id = 450, name = "test_cat"),
  id = pet_id,
  tags = list(
    Tag$new(id = 123, name = "tag_test"), Tag$new(id = 456, name = "unknown")
  ),
  status = "available"
)

#jsonlite::minify(pet$toJSONString())
#cat(pet$toJSONString())
toString(pet$toString())

#json <-
#'[
#  {"Name" : "Mario", "Age" : 32, "Occupation" : "Plumber"}, 
#  {"Name" : "Peach", "Age" : 21, "Occupation" : "Princess"},
#  {},
#  {"Name" : "Bowser", "Occupation" : "Koopa"}
#]'
#
#
##Pet$public_methods
##Pet$public_methods$fromJSON(json)
##Pet$public_methods$toJson()
##Pet$public_methods$validateJSON(json2)
##Pet$public_methods$validateJson(json)
##Pet$my_static_method <- function(x) { x + 2}
##Pet$public_methods$my_static_method(1)
#
  basque_pig_json <-
  '{"className2": "BasquePig", "color": "red"}'
# 
#  danish_pig_json <-
#  '{"className2": "DanishPig", "size": 7}'
# 
#  wrong_json <- 
#  '[
#    {"Name" : "Tom", "Age" : 32, "Occupation" : "Consultant"}, 
#    {},
#    {"Name" : "Ada", "Occupation" : "Engineer"}
#  ]'
#
#  print("==========") 
  pig <- Pig$new()
  basque_pig <- pig$fromJSON(basque_pig_json)
#  #print(basque_pig$actual_instance$color)
#  #expect_equal(basque_pig$actual_type, "BasquePig")
#  pig$fromJSON(danish_pig_json)
#  #pig$fromJSON(wrong_json)
#  pig$toJSON()
#
#  #d <- DanishPig$new()
#  #dp <- d$validateJSON(danish_pig_json)
#
#

# test nested oneOf
nested_oneof <- NestedOneOf$new()
nested_oneof$nested_pig <- pig
nested_oneof$size <- 15

cat(nested_oneof$toJSONString())

