package org.openapitools.server.apis

import io.javalin.http.Context
import io.javalin.http.bodyAsClass
import io.javalin.http.pathParamAsClass
import io.javalin.http.queryParamAsClass

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

class PetApi(private val service: PetApiService) {
    /**
     * Add a new pet to the store
     * 
     * @param pet Pet object that needs to be added to the store 
     */
    fun addPet(ctx: Context) {
        ctx.status(200).json(service.addPet(ctx.bodyAsClass<Pet>()))
    }

    /**
     * Deletes a pet
     * 
     * @param petId Pet id to delete 
     * @param apiKey  (optional)
     */
    fun deletePet(ctx: Context) {
        ctx.status(200).json(service.deletePet(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx.header("api_key")))
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * @param status Status values that need to be considered for filter 
     */
    fun findPetsByStatus(ctx: Context) {
        ctx.status(200).json(service.findPetsByStatus(ctx.queryParams("status")))
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by 
     */
    fun findPetsByTags(ctx: Context) {
        ctx.status(200).json(service.findPetsByTags(ctx.queryParams("tags")))
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * @param petId ID of pet to return 
     */
    fun getPetById(ctx: Context) {
        ctx.status(200).json(service.getPetById(ctx.pathParamAsClass<kotlin.Long>("petId").get()))
    }

    /**
     * Update an existing pet
     * 
     * @param pet Pet object that needs to be added to the store 
     */
    fun updatePet(ctx: Context) {
        ctx.status(200).json(service.updatePet(ctx.bodyAsClass<Pet>()))
    }

    /**
     * Updates a pet in the store with form data
     * 
     * @param petId ID of pet that needs to be updated 
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     */
    fun updatePetWithForm(ctx: Context) {
        ctx.status(200).json(service.updatePetWithForm(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx.formParam("name"), ctx.formParam("status")))
    }

    /**
     * uploads an image
     * 
     * @param petId ID of pet to update 
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     */
    fun uploadFile(ctx: Context) {
        ctx.status(200).json(service.uploadFile(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx.formParam("additionalMetadata"), ctx.uploadedFile("file")))
    }

}
