package org.openapitools.server.api.api

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet
import jakarta.inject.Inject
import misk.testing.MiskTest
import okhttp3.Headers

import org.junit.jupiter.api.Test

@MiskTest(startService = true)
internal class PetApiTest {

    @Inject private lateinit var PetApi : PetApiController

    /**
     * To test PetApiController.addPet
     */
    @Test
    fun `should handle addPet`() {
        val pet: Pet = TODO()
        val response: Pet = PetApi.addPet(pet)
    }

    /**
     * To test PetApiController.deletePet
     */
    @Test
    fun `should handle deletePet`() {
        val petId: kotlin.Long = TODO()
        val apiKey: kotlin.String? = TODO()
        val response = PetApi.deletePet(petId, apiKey)
    }

    /**
     * To test PetApiController.findPetsByStatus
     */
    @Test
    fun `should handle findPetsByStatus`() {
        val status: kotlin.Array<kotlin.String> = TODO()
        val response: kotlin.Array<Pet> = PetApi.findPetsByStatus(status)
    }

    /**
     * To test PetApiController.findPetsByTags
     */
    @Test
    fun `should handle findPetsByTags`() {
        val tags: kotlin.Array<kotlin.String> = TODO()
        val response: kotlin.Array<Pet> = PetApi.findPetsByTags(tags)
    }

    /**
     * To test PetApiController.getPetById
     */
    @Test
    fun `should handle getPetById`() {
        val petId: kotlin.Long = TODO()
        val response: Pet = PetApi.getPetById(petId)
    }

    /**
     * To test PetApiController.updatePet
     */
    @Test
    fun `should handle updatePet`() {
        val pet: Pet = TODO()
        val response: Pet = PetApi.updatePet(pet)
    }

    /**
     * To test PetApiController.updatePetWithForm
     */
    @Test
    fun `should handle updatePetWithForm`() {
        val petId: kotlin.Long = TODO()
        val name: kotlin.String? = TODO()
        val status: kotlin.String? = TODO()
        val response = PetApi.updatePetWithForm(petId, name, status)
    }

    /**
     * To test PetApiController.uploadFile
     */
    @Test
    fun `should handle uploadFile`() {
        val petId: kotlin.Long = TODO()
        val additionalMetadata: kotlin.String? = TODO()
        val file: Headers = TODO()
        val response: ModelApiResponse = PetApi.uploadFile(petId, additionalMetadata, file)
    }

}
