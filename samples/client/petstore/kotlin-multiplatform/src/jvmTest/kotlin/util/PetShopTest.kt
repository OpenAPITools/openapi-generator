package util

import org.junit.Test
import org.openapitools.client.apis.PetApi
import org.openapitools.client.models.Pet

class PetStoreTest {
    @Test
    fun addPet() = runTest {
        val mainApi = PetApi()
        println(mainApi.addPet(Pet("TestPet", emptyList())).body())
    }
}