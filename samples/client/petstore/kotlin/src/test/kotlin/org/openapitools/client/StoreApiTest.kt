package org.openapitools.client

import io.kotlintest.shouldBe
import io.kotlintest.specs.ShouldSpec
import org.openapitools.client.apis.PetApi
import org.openapitools.client.apis.StoreApi
import org.openapitools.client.models.Order
import org.openapitools.client.models.Pet
import java.time.OffsetDateTime.now

class StoreApiTest : ShouldSpec() {
    init {

        val petId:Long = 10006
        val petApi = PetApi()
        val storeApi = StoreApi()

        val pet = Pet(
                id = petId,
                name = "kotlin client test",
                photoUrls = listOf("http://test_kotlin_unit_test.com")
        )
        petApi.addPet(pet)

        should("add an order") {

            val order = Order(
                    id = 12,
                    petId = petId,
                    quantity = 2,
                    shipDate = now(),
                    complete = true
            )
            storeApi.placeOrder(order);

        }

        should("get order by id") {
            val result = storeApi.getOrderById(12)

            // verify order
            result.petId shouldBe (petId)
            result.quantity shouldBe (2)
        }

    }

}
