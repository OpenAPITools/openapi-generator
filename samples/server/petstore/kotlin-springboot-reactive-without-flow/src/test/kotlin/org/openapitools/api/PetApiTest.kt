package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Disabled
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.test.runBlockingTest
import org.springframework.http.ResponseEntity

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
    fun addPetTest() = runBlockingTest {
        val pet: Pet = TODO()
        val response: ResponseEntity<Pet> = api.addPet(pet)

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
    fun deletePetTest() = runBlockingTest {
        val petId: kotlin.Long = TODO()
        val apiKey: kotlin.String? = TODO()
        val response: ResponseEntity<Unit> = api.deletePet(petId, apiKey)

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
    fun findPetsByStatusTest() = runBlockingTest {
        val status: kotlin.collections.List<kotlin.String> = TODO()
        val response: ResponseEntity<List<Pet>> = api.findPetsByStatus(status)

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
    fun findPetsByTagsTest() = runBlockingTest {
        val tags: kotlin.collections.List<kotlin.String> = TODO()
        val response: ResponseEntity<List<Pet>> = api.findPetsByTags(tags)

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
    fun getPetByIdTest() = runBlockingTest {
        val petId: kotlin.Long = TODO()
        val response: ResponseEntity<Pet> = api.getPetById(petId)

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
    fun updatePetTest() = runBlockingTest {
        val pet: Pet = TODO()
        val response: ResponseEntity<Pet> = api.updatePet(pet)

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
    fun updatePetWithFormTest() = runBlockingTest {
        val petId: kotlin.Long = TODO()
        val name: kotlin.String? = TODO()
        val status: kotlin.String? = TODO()
        val response: ResponseEntity<Unit> = api.updatePetWithForm(petId, name, status)

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
    fun uploadFileTest() = runBlockingTest {
        val petId: kotlin.Long = TODO()
        val additionalMetadata: kotlin.String? = TODO()
        val file: org.springframework.http.codec.multipart.Part? = TODO()
        val response: ResponseEntity<ModelApiResponse> = api.uploadFile(petId, additionalMetadata, file)

        // TODO: test validations
    }
}
