package org.openapitools.client

import io.kotlintest.shouldBe
import io.kotlintest.matchers.numerics.shouldBeGreaterThan
import io.kotlintest.matchers.string.shouldContain
import io.kotlintest.shouldThrow
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.ClientException
import org.openapitools.client.models.Category
import org.openapitools.client.models.Pet
import org.openapitools.client.models.Tag

class PetApiTest : ShouldSpec() {
    init {

        val petId:Long = 10006
        val api = PetApi()

        should("add a pet") {

            val pet = Pet(
                    id = petId,
                    name = "kotlin client test",
                    photoUrls = listOf("http://test_kotlin_unit_test.com"),
                    category = Category(petId, "test kotlin category"),
                    tags = listOf(Tag(petId, "test kotlin tag"))
            )
            api.addPet(pet)

        }

        should("get pet by id") {
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
            val result = api.findPetsByStatus(listOf("available"))

            result.size.shouldBeGreaterThan(0)

            for(onePet in result) {
                onePet.status.shouldBe(Pet.Status.available)
            }

            val result2 = api.findPetsByStatus(listOf("unknown_and_incorrect_status"))

            result2.size.shouldBe(0)

        }

        should("update a pet") {
            val pet = Pet(
                    id = petId,
                    name = "kotlin client updatePet",
                    status = Pet.Status.pending,
                    photoUrls = listOf("http://test_kotlin_unit_test.com")
            )
            api.updatePet(pet)

            // verify updated Pet
            val result = api.getPetById(petId)
            result.id shouldBe (petId)
            result.name shouldBe ("kotlin client updatePet")
            result.status shouldBe (Pet.Status.pending)

        }

        should("update a pet with form") {
            val name = "kotlin client updatePet with Form"
            val status = "pending"

            api.updatePetWithForm(petId, name, status)

            // verify updated Pet
            val result = api.getPetById(petId)
            result.id shouldBe (petId)
            result.name shouldBe ("kotlin client updatePet with Form")
            result.status shouldBe (Pet.Status.pending)

        }

        should("delete a pet") {
            api.deletePet(petId, "apiKey")

            // verify updated Pet
            val exception = shouldThrow<ClientException> {
                api.getPetById(petId)
            }
            exception.message?.shouldContain("Pet not found")
        }

    }

}
