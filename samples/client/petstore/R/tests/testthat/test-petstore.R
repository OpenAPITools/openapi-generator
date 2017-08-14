context("basic functionality")
petStoreClient <- PetStoreClient$new("petstore.swagger.io", "v2", "http")
petId <- sample(1:1000, 1)
test_that("addPet", {
  pet <- Pet$new(petId, 
                 Element$new(0,"test"),
                 "test",
                 list("test"),
                 list(Element$new(0, "test")),
                 "available")
  response <- petStoreClient$addPet(pet)
  expect_equal(response$content, "Pet added")
})

test_that("getPetById", {
  response <- petStoreClient$getPetById(petId)
  expect_equal(response$content$id, petId)
})

test_that("updatePetWithForm", {
  response <- petStoreClient$updatePetWithForm(petId, "test", "sold")
  expect_equal(response$content, "Pet updated")
})