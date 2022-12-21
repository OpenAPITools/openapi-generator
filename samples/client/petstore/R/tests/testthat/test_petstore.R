context("basic functionality")

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

# no need to set uasername, password and there should be no error
# since the endpoint can support multi auth schema
#pet_api$api_client$username <- ""
#pet_api$api_client$password <- ""
result <- pet_api$AddPet(pet)

test_that("Test discriminator and mapping", {
  d <- '{"breed": "bulldog","color":"white","className":"Dog"}'
  dog <- ApiClient$new()$deserialize(d, "Animal", loadNamespace("petstore"))
  expect_equal(class(dog)[1], "Dog")
  expect_equal(dog$breed, "bulldog")
  expect_equal(dog$color, "white")
  expect_equal(dog$className, "Dog")
})

test_that("Test toJSONString", {
  expect_equal(pet_id, 123321)
  expect_equal(pet$toJSONString(), '{"id":123321,"category":{"id":450,"name":"test_cat"},"name":"name_test","photoUrls":["photo_test","second test"],"tags":[{"id":123,"name":"tag_test"},{"id":456,"name":"unknown"}],"status":"available"}')
})

test_that("Test FindPetByStatus", {
  pet_api$api_client$oauth_client_id <- "client_id_test"
  pet_api$api_client$oauth_secret <- "secret_test"
  
  result <- pet_api$FindPetsByStatus("available")
  expect_equal(result[[1]]$status, "available")
})

test_that("Test toJSON toJSONString fromJSON fromJSONString", {
  pet0 <- Pet$new()
  jsonpet <- pet0$toJSON()
  pet2 <- pet0$fromJSON(
    jsonlite::toJSON(jsonpet, auto_unbox = TRUE)
  )
  expect_equal(pet0, pet2)
  jsonpet <- pet0$toJSONString()
  pet2 <- pet0$fromJSON(
    jsonpet
  )
  expect_equal(pet0, pet2)

  jsonpet <- pet0$toJSONString()
  pet2 <- pet0$fromJSONString(
    jsonpet
  )
  expect_equal(pet0, pet2)

  pet1 <- Pet$new("name_test",
    list("photo_test", "second test"),
    category = Category$new(id = 450, name = "test_cat"),
    id = pet_id,
    tags = list(
      Tag$new(id = 123, name = "tag_test"), Tag$new(id = 456, name = "unknown")
    ),
    status = "available"
  )
  jsonpet <- pet1$toJSON()
  pet2 <- pet1$fromJSON(
    jsonlite::toJSON(jsonpet, auto_unbox = TRUE)
  )
  expect_equal(pet1, pet2)

  jsonpet <- pet1$toJSONString()
  pet2 <- pet1$fromJSON(
    jsonpet
  )
  expect_equal(pet1, pet2)

  jsonpet <- pet1$toJSONString()
  pet2 <- pet1$fromJSONString(
    jsonpet
  )
  expect_equal(pet1, pet2)
})

test_that("Test Category", {
  c1 <- Category$new(id = 450, name = "test_cat")
  c2 <- Category$new()
  c2$fromJSON(jsonlite::toJSON(c1$toJSON(), auto_unbox = TRUE))
  expect_equal(c1, c2)
  c2$fromJSONString(c1$toJSONString())
  expect_equal(c1, c2)
})

test_that("GetPetById", {
  response <- pet_api$GetPetById(pet_id)
  expect_equal(response$id, pet_id)
  expect_equal(response$name, "name_test")
  expect_equal(
    response$photoUrls,
    list("photo_test", "second test")
  )
  expect_equal(response$status, "available")
  expect_equal(response$category, Category$new(id = 450, name = "test_cat"))

  expect_equal(pet$tags, response$tags)
  expect_equal(
    response$tags,
    list(Tag$new(id = 123, name = "tag_test"), Tag$new(id = 456, name = "unknown"))
  )
})

test_that("GetPetByIdStreaming", {
  result <- tryCatch(
               pet_api$GetPetByIdStreaming(pet_id, stream_callback = function(x) { print(x) }),
               ApiException = function(ex) ex
            )
})

test_that("Test header parameters", {
  # test exception 
  result <- tryCatch(pet_api$TestHeader(45345), 
          ApiException = function(ex) ex
  )

  expect_true(!is.null(result))
  expect_true(!is.null(result$ApiException))
  expect_equal(result$ApiException$status, 404)
  # test error object `ApiResponse`
  #expect_equal(result$ApiException$error_object$toString(), "{\"code\":404,\"type\":\"unknown\",\"message\":\"null for uri: http://pet\n  x[1]: store.swagger.io/v2/pet_header_test\"}")
  expect_equal(result$ApiException$error_object$code, 404)
})

test_that("Test GetPetById exception", {
  # test exception 
  result <- tryCatch(pet_api$GetPetById(98765), # petId not exist
          ApiException = function(ex) ex
  )

  expect_true(!is.null(result))
  expect_true(!is.null(result$ApiException))
  expect_equal(result$ApiException$status, 404)
  # test error object `ApiResponse`
  expect_equal(result$ApiException$error_object$toString(), "{\"code\":1,\"type\":\"error\",\"message\":\"Pet not found\"}")
  expect_equal(result$ApiException$error_object$code, 1)
})

test_that("GetPetById with data_file", {
  # test to ensure json is saved to the file `get_pet_by_id.json`
  pet_response <- pet_api$GetPetById(pet_id, data_file = "get_pet_by_id.json")
  response <- read_json("get_pet_by_id.json")
  expect_true(!is.null(response))
  expect_equal(response$id, pet_id)
  expect_equal(response$name, "name_test")
})

test_that("Tests allOf", {
  # test allOf without discriminator
  a1 <- AllofTagApiResponse$new(id = 450, name = "test_cat", code = 200, type = "test_type", message = "test_message")
  
  expect_true(!is.null(a1))
  expect_equal(a1$id, 450)
  expect_equal(a1$name, "test_cat")
})

test_that("Tests allOf with discriminator", {
  # test allOf without discriminator
  c1 <- Cat$new(className = "cat", color = "red", declawed = TRUE)
  
  expect_true(!is.null(c1))
  expect_equal(c1$className, "cat")
  expect_equal(c1$color, "red")
  expect_true(c1$declawed)
})

test_that("Tests validateJSON", {
  json <-
  '{"name": "pet", "photoUrls" : ["http://a.com", "http://b.com"]}'
  
  json2 <-
  '[
    {"Name" : "Tom", "Age" : 32, "Occupation" : "Consultant"}, 
    {},
    {"Name" : "Ada", "Occupation" : "Engineer"}
  ]'

  # validate `json` and no error throw
  Pet$public_methods$validateJSON(json)

  # validate `json2` and should throw an error due to missing required fields
  #expect_error(Pet$public_methods$validateJSON(json2), 'The JSON input ` [\n    {\"Name\" : \"Tom\", \"Age\" : 32, \"Occupation\" : \"Consultant\"}, \n    {},\n    {\"Name\" : \"Ada\", \"Occupation\" : \"Engineer\"}\n  ] ` is invalid for Pet: the required field `name` is missing.')
  
})

# test object with special item names: self, private, super
test_that("Tests special item names", {
  special_json <-
  '{"self": 123, "private": "red", "super": "something"}'

  # test fromJSON
  special <- Special$new()$fromJSON(special_json)
  expect_equal(special$item_self, 123)
  expect_equal(special$item_private, "red")
  expect_equal(special$item_super, "something")

  # test toJSONString 
  expect_true(grepl('"private"', special$toJSONString()))
  expect_true(grepl('"self"', special$toJSONString()))
  expect_true(grepl('"super"', special$toJSONString()))
  expect_equal('{"self":123,"private":"red","super":"something"}', special$toJSONString())

  # round trip test
  s1 <- Special$new()$fromJSONString(special_json)
  s2 <- Special$new()$fromJSONString(s1$toJSONString())
  expect_equal(s1, s2)

})

test_that("Tests oneOf", {
  basque_pig_json <-
  '{"className": "BasquePig", "color": "red"}'

  danish_pig_json <-
  '{"className": "DanishPig", "size": 7}'

  wrong_json <- 
  '[
    {"Name" : "Tom", "Age" : 32, "Occupation" : "Consultant"}, 
    {},
    {"Name" : "Ada", "Occupation" : "Engineer"}
  ]'

  original_danish_pig <- DanishPig$new()$fromJSON(danish_pig_json)
  original_basque_pig <- BasquePig$new()$fromJSON(basque_pig_json)

  # test fromJSON, actual_tpye, actual_instance
  pig <- Pig$new()
  danish_pig <- pig$fromJSON(danish_pig_json)
  pig$validateJSON(basque_pig_json) # validate JSON to ensure its actual_instance, actual_type are not updated
  expect_equal(danish_pig$actual_type, "DanishPig")
  expect_equal(danish_pig$actual_instance$size, 7)
  expect_equal(danish_pig$actual_instance$className, "DanishPig")

  expect_equal(pig$actual_type, "DanishPig")
  expect_equal(pig$actual_instance$size, 7)
  expect_equal(pig$actual_instance$className, "DanishPig")

  # test toJSON
  expect_equal(danish_pig$toJSONString(), original_danish_pig$toJSONString())

  basque_pig <- pig$fromJSON(basque_pig_json)
  expect_equal(basque_pig$actual_type, "BasquePig")
  expect_equal(basque_pig$actual_instance$color, "red")
  expect_equal(basque_pig$actual_instance$className, "BasquePig")
  expect_equal(basque_pig$toJSONString(), original_basque_pig$toJSONString())

  # test exception when no matche found
  expect_error(pig$fromJSON('{}'), 'No match found when deserializing the input into Pig with oneOf schemas BasquePig, DanishPig. Details: >> The JSON input ` \\{\\} ` is invalid for BasquePig: the required field `className` is missing\\. >> The JSON input ` \\{\\} ` is invalid for DanishPig: the required field `className` is missing\\.')
  expect_error(pig$validateJSON('{}'), 'No match found when deserializing the input into Pig with oneOf schemas BasquePig, DanishPig. Details: >> The JSON input ` \\{\\} ` is invalid for BasquePig: the required field `className` is missing\\. >> The JSON input ` \\{\\} ` is invalid for DanishPig: the required field `className` is missing\\.')

  # class name test
  expect_equal(get(class(basque_pig$actual_instance)[[1]], pos = -1)$classname, "BasquePig")

  # test constructors
  pig2 <- Pig$new(instance = basque_pig$actual_instance)
  expect_equal(pig2$actual_type, "BasquePig")
  expect_equal(pig2$actual_instance$color, "red")
  expect_equal(pig2$actual_instance$className, "BasquePig")
  expect_equal(pig2$toJSONString(), original_basque_pig$toJSONString())

  expect_error(Pig$new(instance = basque_pig), 'Failed to initialize Pig with oneOf schemas BasquePig, DanishPig. Provided class name:  Pig')

  # test nested oneOf toJSONString
  nested_oneof <- NestedOneOf$new()
  nested_oneof$nested_pig <- pig
  nested_oneof$size <- 15
  expect_equal(nested_oneof$toJSONString(), '{"size":15,"nested_pig":{"className":"BasquePig","color":"red"}}')

  # test fromJSONString with nested oneOf
  nested_json_str <- '{"size":15,"nested_pig":{"className":"BasquePig","color":"red"}}'
  nested_oneof2 <- NestedOneOf$new()$fromJSONString(nested_json_str)
  expect_equal(nested_oneof2$toJSONString(), '{"size":15,"nested_pig":{"className":"BasquePig","color":"red"}}')

  # test toString
  expect_equal(as.character(jsonlite::minify(pig$toString())), "{\"actual_instance\":{\"className\":\"BasquePig\",\"color\":\"red\"},\"actual_type\":\"BasquePig\",\"one_of\":\"BasquePig, DanishPig\"}")
  expect_equal(as.character(jsonlite::minify(Pig$new()$toString())), "{\"one_of\":\"BasquePig, DanishPig\"}")
})

test_that("Tests anyOf", {
  basque_pig_json <-
  '{"className": "BasquePig", "color": "red"}'

  danish_pig_json <-
  '{"className": "DanishPig", "size": 7}'

  wrong_json <- 
  '[
    {"Name" : "Tom", "Age" : 32, "Occupation" : "Consultant"}, 
    {},
    {"Name" : "Ada", "Occupation" : "Engineer"}
  ]'

  original_danish_pig <- DanishPig$new()$fromJSON(danish_pig_json)
  original_basque_pig <- BasquePig$new()$fromJSON(basque_pig_json)

  # test fromJSON, actual_tpye, actual_instance
  pig <- AnyOfPig$new()
  danish_pig <- pig$fromJSON(danish_pig_json)
  expect_equal(danish_pig$actual_type, "DanishPig")
  expect_equal(danish_pig$actual_instance$size, 7)
  expect_equal(danish_pig$actual_instance$className, "DanishPig")

  expect_equal(pig$actual_type, "DanishPig")
  expect_equal(pig$actual_instance$size, 7)
  expect_equal(pig$actual_instance$className, "DanishPig")

  # test toJSONString
  expect_equal(danish_pig$toJSONString(), original_danish_pig$toJSONString())

  basque_pig <- pig$fromJSON(basque_pig_json)
  expect_equal(basque_pig$actual_type, "BasquePig")
  expect_equal(basque_pig$actual_instance$color, "red")
  expect_equal(basque_pig$actual_instance$className, "BasquePig")
  expect_equal(basque_pig$toJSONString(), original_basque_pig$toJSONString())

  # test exception when no matche found
  expect_error(pig$fromJSON('{}'), 'No match found when deserializing the input into AnyOfPig with anyOf schemas BasquePig, DanishPig. Details: >> The JSON input ` \\{\\} ` is invalid for BasquePig: the required field `className` is missing\\. >> The JSON input ` \\{\\} ` is invalid for DanishPig: the required field `className` is missing\\.')
  expect_error(pig$validateJSON('{}'), 'No match found when deserializing the input into AnyOfPig with anyOf schemas BasquePig, DanishPig. Details: >> The JSON input ` \\{\\} ` is invalid for BasquePig: the required field `className` is missing\\. >> The JSON input ` \\{\\} ` is invalid for DanishPig: the required field `className` is missing\\.')

})

#test_that("GetPetById", {
#  pet.id <- pet.id
#  pet <- Pet$new(pet.id, NULL, "name_test2",
#                 list("photo_test2", "second test2"),
#                 NULL, NULL)
#  result <-pet_api$AddPet(pet)
#
#  response <- pet_api$GetPetById(pet.id)
#
#  expect_equal(response$id, pet.id)
#  expect_equal(response$name, "name_test2")
#  #expect_equal(response$category, Category$new(450,"test_cat"))
#  expect_equal(response$photoUrls, list("photo_test2", "second test2"))
#  expect_equal(response$status, NULL)
#  #expect_equal(response$tags, list(Tag$new(123, "tag_test"), Tag$new(456, "unknown")))
#})

