package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.junit.jupiter.api.Test
import kotlinx.coroutines.flow.Flow;
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
    fun addPetTest() = runBlockingTest {
        val body:Pet = TODO()
        val response: ResponseEntity<Unit> = api.addPet(body)

        // TODO: test validations
    }

    /**
     * To test PetApiController.deletePet
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun deletePetTest() = runBlockingTest {
        val petId:kotlin.Long = TODO()
        val apiKey:kotlin.String? = TODO()
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
    fun findPetsByStatusTest() = runBlockingTest {
        val status:kotlin.collections.List<kotlin.String> = TODO()
        val response: ResponseEntity<Flow<Pet>> = api.findPetsByStatus(status)

        // TODO: test validations
    }

    /**
     * To test PetApiController.findPetsByTags
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun findPetsByTagsTest() = runBlockingTest {
        val tags:kotlin.collections.List<kotlin.String> = TODO()
        val response: ResponseEntity<Flow<Pet>> = api.findPetsByTags(tags)

        // TODO: test validations
    }

    /**
     * To test PetApiController.getPetById
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun getPetByIdTest() = runBlockingTest {
        val petId:kotlin.Long = TODO()
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
    fun updatePetTest() = runBlockingTest {
        val body:Pet = TODO()
        val response: ResponseEntity<Unit> = api.updatePet(body)

        // TODO: test validations
    }

    /**
     * To test PetApiController.updatePetWithForm
     *
     * @throws ApiException
     *          if the Api call fails
     */
    @Test
    fun updatePetWithFormTest() = runBlockingTest {
        val petId:kotlin.Long = TODO()
        val name:kotlin.String? = TODO()
        val status:kotlin.String? = TODO()
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
    fun uploadFileTest() = runBlockingTest {
        val petId:kotlin.Long = TODO()
        val additionalMetadata:kotlin.String? = TODO()
        val file:org.springframework.core.io.Resource? = TODO()
        val response: ResponseEntity<ModelApiResponse> = api.uploadFile(petId, additionalMetadata, file)

        // TODO: test validations
    }

}
