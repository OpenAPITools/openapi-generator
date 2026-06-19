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
        val result = service.addPet(ctx.bodyAsClass<Pet>(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Deletes a pet
     * 
     * @param petId Pet id to delete 
     * @param apiKey  (optional)
     */
    fun deletePet(ctx: Context) {
        val result = service.deletePet(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx.header("api_key"), ctx)
        ctx.status(400).json(result)
    }

    /**
     * Finds Pets by status
     * Multiple status values can be provided with comma separated strings
     * @param status Status values that need to be considered for filter 
     */
    fun findPetsByStatus(ctx: Context) {
        val result = service.findPetsByStatus(ctx.queryParams("status"), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Finds Pets by tags
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by 
     */
    fun findPetsByTags(ctx: Context) {
        val result = service.findPetsByTags(ctx.queryParams("tags"), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Find pet by ID
     * Returns a single pet
     * @param petId ID of pet to return 
     */
    fun getPetById(ctx: Context) {
        val result = service.getPetById(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Update an existing pet
     * 
     * @param pet Pet object that needs to be added to the store 
     */
    fun updatePet(ctx: Context) {
        val result = service.updatePet(ctx.bodyAsClass<Pet>(), ctx)
        ctx.status(200).json(result)
    }

    /**
     * Updates a pet in the store with form data
     * 
     * @param petId ID of pet that needs to be updated 
     * @param name Updated name of the pet (optional)
     * @param status Updated status of the pet (optional)
     */
    fun updatePetWithForm(ctx: Context) {
        val result = service.updatePetWithForm(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx.formParam("name"), ctx.formParam("status"), ctx)
        ctx.status(405).json(result)
    }

    /**
     * uploads an image
     * 
     * @param petId ID of pet to update 
     * @param additionalMetadata Additional data to pass to server (optional)
     * @param file file to upload (optional)
     */
    fun uploadFile(ctx: Context) {
        val result = service.uploadFile(ctx.pathParamAsClass<kotlin.Long>("petId").get(), ctx.formParam("additionalMetadata"), ctx.uploadedFile("file"), ctx)
        ctx.status(200).json(result)
    }

}
