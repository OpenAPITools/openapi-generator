package io.swagger.client.functional

import io.kotlintest.matchers.should
import io.kotlintest.matchers.beGreaterThan
import io.kotlintest.specs.ShouldSpec
import io.swagger.client.apis.PetApi

class EvaluateTest : ShouldSpec() {
    init {
        should("query against pet statuses") {
            val api = PetApi()
            val results = api.findPetsByStatus(listOf("available", "pending"))

            results.size should beGreaterThan(1)
        }

// TODO: Handle default (200) response
/*
        should("post data (new pet)") {
            val api = PetApi()
            val pet = Pet(
                    id = 0,
                    name = "kotlin client test",
                    category = Category(0, "string"),
                    tags = listOf(Tag(0, "string"))
            )
            val result = api.addPet(pet)

            result.name shouldBe(pet.name)
            result.category shouldBe(pet.category)
            result.tags shouldBe(pet.tags)
        }
*/
    }
}