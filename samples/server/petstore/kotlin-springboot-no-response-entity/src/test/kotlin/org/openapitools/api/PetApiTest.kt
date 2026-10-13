package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Disabled
import org.springframework.http.HttpStatus

class PetApiTest {

    private val service: PetApiService = PetApiServiceImpl()
    private val api: PetApiController = PetApiController(service)

    /**
     * To test PetApiController.addPet
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun addPetTest() {
        val body: Pet = TODO()
        
        val response: Unit = api.addPet(body)

        // TODO: test validations
    }

    /**
     * To test PetApiController.deletePet
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun deletePetTest() {
        val petId: kotlin.Long = TODO()
        val apiKey: kotlin.String? = TODO()
        
        val response: Unit = api.deletePet(petId, apiKey)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsByStatus
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun findPetsByStatusTest() {
        val status: kotlin.collections.List<kotlin.String> = TODO()
        
        val response: List<Pet> = api.findPetsByStatus(status)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsByTags
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun findPetsByTagsTest() {
        val tags: kotlin.collections.List<kotlin.String> = TODO()
        
        val response: List<Pet> = api.findPetsByTags(tags)

        // TODO: test validations
    }

    /**
     * To test PetApiController.getPetById
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun getPetByIdTest() {
        val petId: kotlin.Long = TODO()
        
        val response: Pet = api.getPetById(petId)

        // TODO: test validations
    }

    /**
     * To test PetApiController.updatePet
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun updatePetTest() {
        val body: Pet = TODO()
        
        val response: Unit = api.updatePet(body)

        // TODO: test validations
    }

    /**
     * To test PetApiController.updatePetWithForm
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun updatePetWithFormTest() {
        val petId: kotlin.Long = TODO()
        val name: kotlin.String? = TODO()
        val status: kotlin.String? = TODO()
        
        val response: Unit = api.updatePetWithForm(petId, name, status)

        // TODO: test validations
    }

    /**
     * To test PetApiController.uploadFile
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    @Disabled("Provide test inputs and assertions before enabling this generated placeholder")
    fun uploadFileTest() {
        val petId: kotlin.Long = TODO()
        val additionalMetadata: kotlin.String? = TODO()
        val file: org.springframework.web.multipart.MultipartFile = TODO()
        
        val response: ModelApiResponse = api.uploadFile(petId, additionalMetadata, file)

        // TODO: test validations
    }
}
