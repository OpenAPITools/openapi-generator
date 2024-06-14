package org.openapitools.server.apis

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet
import io.javalin.http.Context

interface PetApiService {

    /**
     * POST /pet : Add a new pet to the store
     * 
     *
     * @param pet Pet object that needs to be added to the store (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid input (status code 405)
     * @see PetApi#addPet
     */
    fun addPet(pet: Pet, ctx: Context): Pet

    /**
     * DELETE /pet/{petId} : Deletes a pet
     * 
     *
     * @param petId Pet id to delete (required)
     * @param apiKey  (optional)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return Invalid pet value (status code 400)
     * @see PetApi#deletePet
     */
    fun deletePet(petId: kotlin.Long, apiKey: kotlin.String?, ctx: Context): Unit

    /**
     * GET /pet/findByStatus : Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     *
     * @param status Status values that need to be considered for filter (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid status value (status code 400)
     * @see PetApi#findPetsByStatus
     */
    fun findPetsByStatus(status: kotlin.collections.List<kotlin.String>, ctx: Context): List<Pet>

    /**
     * GET /pet/findByTags : Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * @param tags Tags to filter by (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid tag value (status code 400)
     * @deprecated
     * @see PetApi#findPetsByTags
     */
    fun findPetsByTags(tags: kotlin.collections.List<kotlin.String>, ctx: Context): List<Pet>

    /**
     * GET /pet/{petId} : Find pet by ID
     * Returns a single pet
     *
     * @param petId ID of pet to return (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Pet not found (status code 404)
     * @see PetApi#getPetById
     */
    fun getPetById(petId: kotlin.Long, ctx: Context): Pet

    /**
     * PUT /pet : Update an existing pet
     * 
     *
     * @param pet Pet object that needs to be added to the store (required)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Pet not found (status code 404)
     *         or Validation exception (status code 405)
     * API documentation for the updatePet operation
     * @see <a href="http://petstore.swagger.io/v2/doc/updatePet">Update an existing pet Documentation</a>
     * @see PetApi#updatePet
     */
    fun updatePet(pet: Pet, ctx: Context): Pet

    /**
     * POST /pet/{petId} : Updates a pet in the store with form data
     * 
     *
     * @param petId ID of pet that needs to be updated (required)
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return Invalid input (status code 405)
     * @see PetApi#updatePetWithForm
     */
    fun updatePetWithForm(petId: kotlin.Long, name: kotlin.String?, status: kotlin.String?, ctx: Context): Unit

    /**
     * POST /pet/{petId}/uploadImage : uploads an image
     * 
     *
     * @param petId ID of pet to update (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     * @param ctx The Javalin context. Especially handy if you need to access things like authentication headers in your service. (required)
     * @return successful operation (status code 200)
     * @see PetApi#uploadFile
     */
    fun uploadFile(petId: kotlin.Long, additionalMetadata: kotlin.String?, file: io.javalin.http.UploadedFile?, ctx: Context): ModelApiResponse
}
