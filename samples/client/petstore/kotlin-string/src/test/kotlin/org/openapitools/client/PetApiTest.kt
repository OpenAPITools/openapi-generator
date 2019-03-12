package org.openapitools.client

import io.kotlintest.shouldBe
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.PetApi
import org.openapitools.client.models.Category
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Tag

class PetApiTest : ShouldSpec() {
    init {
        should("add a new pet (id 10006) to ensure the pet object is available for all the tests") {
            val petId:Long = 10006
            val pet = Pet(
                    id = petId,
                    name = "kotlin client test",
                    photoUrls = arrayOf("http://test_kotlin_unit_test.com"),
                    category = Category(petId, "test kotlin category"),
                    tags = arrayOf(Tag(petId, "test kotlin tag"))
            )

            val api = PetApi()
            api.addPet(pet)

        }


        should("get pet by id") {
            val petId: Long = 10006
            val api = PetApi()
            val result = api.getPetById(petId)

            result.name shouldBe ("kotlin client test")
        }

    }

}