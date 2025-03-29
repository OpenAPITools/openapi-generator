package org.openapitools.server.api.api

import jakarta.inject.Inject
import misk.testing.MiskTest
import org.junit.jupiter.api.Test

import misk.web.HttpCall
import misk.web.PathParam
import misk.web.QueryParam
import misk.web.RequestBody
import misk.web.RequestHeader

import org.openapitools.server.api.model.ModelApiResponse
import org.openapitools.server.api.model.Pet

@MiskTest(startService = true)
internal class PetApiTest {

    @Inject private lateinit var petApi: PetApi

    /**
     * To test PetApiController.addPet
     */
    @Test
    fun `should handle addPet`() {
        val pet = TODO()
        val response: Pet = petApi.addPet(pet)
    }

    /**
     * To test PetApiController.deletePet
     */
    @Test
    fun `should handle deletePet`() {
        val petId = TODO()
        val apiKey = TODO()
        val response = petApi.deletePet(petId, apiKey)
    }

    /**
     * To test PetApiController.findPetsByStatus
     */
    @Test
    fun `should handle findPetsByStatus`() {
        val status = TODO()
        val response: kotlin.Array<Pet> = petApi.findPetsByStatus(status)
    }

    /**
     * To test PetApiController.findPetsByTags
     */
    @Test
    fun `should handle findPetsByTags`() {
        val tags = TODO()
        val response: kotlin.Array<Pet> = petApi.findPetsByTags(tags)
    }

    /**
     * To test PetApiController.getPetById
     */
    @Test
    fun `should handle getPetById`() {
        val petId = TODO()
        val response: Pet = petApi.getPetById(petId)
    }

    /**
     * To test PetApiController.updatePet
     */
    @Test
    fun `should handle updatePet`() {
        val pet = TODO()
        val response: Pet = petApi.updatePet(pet)
    }

    /**
     * To test PetApiController.updatePetWithForm
     */
    @Test
    fun `should handle updatePetWithForm`() {
        val petId = TODO()
        val name = TODO()
        val status = TODO()
        val response = petApi.updatePetWithForm(petId, name, status)
    }

    /**
     * To test PetApiController.uploadFile
     */
    @Test
    fun `should handle uploadFile`() {
        val petId = TODO()
        val additionalMetadata = TODO()
        val file = TODO()
        val response: ModelApiResponse = petApi.uploadFile(petId, additionalMetadata, file)
    }

}
