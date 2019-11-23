context("basic functionality")
petApi <- PetApi$new()
petId <- 123321
test_that("AddPet", {
  pet <- Pet$new("name_test", list("photo_test", "second test"),
                 petId, Category$new(id=450, name="test_cat"),
                 list(Tag$new(id=123, name="tag_test"), Tag$new(id=456, name="unknown")), 
                 "available")
  #pet <- Pet$new("name_test", 
  #               list("photo_test", "second test"),
  #               category=Category$new(id=450, name="test_cat"),
  #               id=petId,
  #               tags=list(Tag$new(id=123, name="tag_test"), Tag$new(id=456, name="unknown")),
  #               status="available")
  result <-petApi$AddPet(pet)
  expect_equal(petId, 123321)
  expect_equal(result, NULL)
})

test_that("Test Pet", {
  pet <- Pet$new("name_test", list("photo_test", "second test"),
                 petId, Category$new(id=450, name="test_cat"),
                 list(Tag$new(id=123, name="tag_test"), Tag$new(id=456, name="unknown")), 
                 "available")

  pet2 <- Pet$new()
  #pet2$fromJSON(jsonlite::toJSON(pet$toJSON(), auto_unbox=TRUE))
  #expect_equal(pet, pet2)
})

test_that("Test Category", {
  c1 <- Category$new(id=450, name="test_cat")
  c2 <- Category$new()
  c2$fromJSON(jsonlite::toJSON(c1$toJSON(), auto_unbox=TRUE))
  expect_equal(c1, c2)
})

test_that("GetPetById", {
  response <- petApi$GetPetById(petId)
  expect_equal(response$id, petId)
  expect_equal(response$name, "name_test")
  #expect_equal(response$category, Category$new(id=450, name="test_cat"))
  expect_equal(response$photoUrls, list("photo_test", "second test"))
  expect_equal(response$status, "available")
  expect_equal(response$tags, list(Tag$new(id=123, name="tag_test"), Tag$new(id=456, name="unknown")))
})

#test_that("GetPetById", {
#  pet.id <- pet.id
#  pet <- Pet$new(pet.id, NULL, "name_test2",
#                 list("photo_test2", "second test2"),
#                 NULL, NULL)
#  result <-petApi$AddPet(pet)
#
#  response <- petApi$GetPetById(pet.id)
#
#  expect_equal(response$id, pet.id)
#  expect_equal(response$name, "name_test2")
#  #expect_equal(response$category, Category$new(450,"test_cat"))
#  expect_equal(response$photoUrls, list("photo_test2", "second test2"))
#  expect_equal(response$status, NULL)
#  #expect_equal(response$tags, list(Tag$new(123, "tag_test"), Tag$new(456, "unknown")))
#})

#test_that("updatePetWithForm", {
#  response <- petApi$updatePetWithForm(petId, "test", "sold")
#  expect_equal(response, "Pet updated")
#})
