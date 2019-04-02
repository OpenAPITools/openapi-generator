package org.openapitools.client

import io.kotlintest.shouldBe
import io.kotlintest.matchers.numerics.shouldBeGreaterThan
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.PetApi
import org.openapitools.client.models.Category
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Tag

class PetApiTest : ShouldSpec() {
    init {

        should("add a pet") {
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

            result.id shouldBe (petId)
            result.name shouldBe ("kotlin client test")
            result.photoUrls[0] shouldBe ("http://test_kotlin_unit_test.com")
            result.category!!.id shouldBe (petId)
            result.category!!.name shouldBe ("test kotlin category")
            result.tags!![0].id shouldBe (petId)
            result.tags!![0].name shouldBe ("test kotlin tag")

        }

        should("find pet by status") {
            val api = PetApi()
            val result = api.findPetsByStatus(arrayOf("available"))

            result.size.shouldBeGreaterThan(0)

            for(onePet in result) {
                onePet.status.shouldBe(Pet.Status.available)
            }

            val result2 = api.findPetsByStatus(arrayOf("unknown_and_incorrect_status"))

            result2.size.shouldBe(0)

        }

        should("update a pet") {
            val petId:Long = 10007
            val pet = Pet(
                    id = petId,
                    name = "kotlin client updatePet",
                    status = Pet.Status.pending,
                    photoUrls = arrayOf("http://test_kotlin_unit_test.com")
            )

            val api = PetApi()
            api.updatePet(pet)

            // verify updated Pet
            val result = api.getPetById(petId)
            result.id shouldBe (petId)
            result.name shouldBe ("kotlin client updatePet")
            result.status shouldBe (Pet.Status.pending)

        }

        //TODO the test fail cause client doesn't support other JSON contentType/Accept
        /*
        should("update a pet with form") {
            val petId:Long = 10007
            val name = "kotlin client updatePet with Form"
            val status = "pending"

            val api = PetApi()
            api.updatePetWithForm(petId, name, status)

            // verify updated Pet
            val result = api.getPetById(petId)
            result.id shouldBe (petId)
            result.name shouldBe ("kotlin client updatePet with Form")
            result.status shouldBe (Pet.Status.pending)

        }
        */

    }

}
