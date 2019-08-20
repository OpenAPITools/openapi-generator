package org.openapitools.api

import org.openapitools.model.ModelApiResponse
import org.openapitools.model.Pet
import org.junit.jupiter.api.Test

import org.springframework.http.ResponseEntity

class PetApiTest {

    private val service: PetApiService = PetApiServiceImpl()
    private val api: PetApiController = PetApiController(service)

    
    /**
    * Add a new pet to the store
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun addPetTest() {
        val body:Pet? = null
        val response: ResponseEntity<Unit> = api.addPet(body!!)

        // TODO: test validations
    }
    
    /**
    * Deletes a pet
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun deletePetTest() {
        val petId:kotlin.Long? = null
        val apiKey:kotlin.String? = null
        val response: ResponseEntity<Unit> = api.deletePet(petId!!, apiKey!!)

        // TODO: test validations
    }
    
    /**
    * Finds Pets by status
    *
    * Multiple status values can be provided with comma separated strings
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun findPetsByStatusTest() {
        val status:kotlin.collections.List<kotlin.String>? = null
        val response: ResponseEntity<List<Pet>> = api.findPetsByStatus(status!!)

        // TODO: test validations
    }
    
    /**
    * Finds Pets by tags
    *
    * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun findPetsByTagsTest() {
        val tags:kotlin.collections.List<kotlin.String>? = null
        val response: ResponseEntity<List<Pet>> = api.findPetsByTags(tags!!)

        // TODO: test validations
    }
    
    /**
    * Find pet by ID
    *
    * Returns a single pet
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun getPetByIdTest() {
        val petId:kotlin.Long? = null
        val response: ResponseEntity<Pet> = api.getPetById(petId!!)

        // TODO: test validations
    }
    
    /**
    * Update an existing pet
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun updatePetTest() {
        val body:Pet? = null
        val response: ResponseEntity<Unit> = api.updatePet(body!!)

        // TODO: test validations
    }
    
    /**
    * Updates a pet in the store with form data
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun updatePetWithFormTest() {
        val petId:kotlin.Long? = null
        val name:kotlin.String? = null
        val status:kotlin.String? = null
        val response: ResponseEntity<Unit> = api.updatePetWithForm(petId!!, name!!, status!!)

        // TODO: test validations
    }
    
    /**
    * uploads an image
    *
    * 
    *
    * @throws ApiException
    *          if the Api call fails
    */
    @Test
    fun uploadFileTest() {
        val petId:kotlin.Long? = null
        val additionalMetadata:kotlin.String? = null
        val file:org.springframework.core.io.Resource? = null
        val response: ResponseEntity<ModelApiResponse> = api.uploadFile(petId!!, additionalMetadata!!, file!!)

        // TODO: test validations
    }
    
}
