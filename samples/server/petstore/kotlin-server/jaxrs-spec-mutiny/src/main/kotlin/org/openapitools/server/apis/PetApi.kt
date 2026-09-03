package org.openapitools.server.apis;

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/pet")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.26.0-SNAPSHOT")
interface PetApi {

    /**
     * Add a new pet to the store
     * @param body Pet object that needs to be added to the store
     * @return Invalid input (status code 405)
     */
    @POST
    @Consumes("application/json", "application/xml")
    fun addPet( body: Pet): io.smallrye.mutiny.Uni<Response>

    /**
     * Deletes a pet
     * @param petId Pet id to delete
     * @return Invalid pet value (status code 400)
     */
    @DELETE
    @Path("/{petId}")
    fun deletePet(@PathParam("petId") petId: kotlin.Long,@HeaderParam("api_key")  apiKey: kotlin.String?): io.smallrye.mutiny.Uni<Response>

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     * @param status Status values that need to be considered for filter
     * @return successful operation (status code 200)
     *         or Invalid status value (status code 400)
     */
    @GET
    @Path("/findByStatus")
    @Produces("application/xml", "application/json")
    fun findPetsByStatus(@QueryParam("status") status: kotlin.collections.List<kotlin.String>): io.smallrye.mutiny.Uni<Response>

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * @param tags Tags to filter by
     * @return successful operation (status code 200)
     *         or Invalid tag value (status code 400)
     * @deprecated
     */
    @GET
    @Path("/findByTags")
    @Produces("application/xml", "application/json")
    fun findPetsByTags(@QueryParam("tags") tags: kotlin.collections.List<kotlin.String>): io.smallrye.mutiny.Uni<Response>

    /**
     * Find pet by ID
     *
     * Returns a single pet
     * @param petId ID of pet to return
     * @return successful operation (status code 200)
     *         or Invalid ID supplied (status code 400)
     *         or Pet not found (status code 404)
     */
    @GET
    @Path("/{petId}")
    @Produces("application/xml", "application/json")
    fun getPetById(@PathParam("petId") petId: kotlin.Long): io.smallrye.mutiny.Uni<Response>

    /**
     * Update an existing pet
     * @param body Pet object that needs to be added to the store
     * @return Invalid ID supplied (status code 400)
     *         or Pet not found (status code 404)
     *         or Validation exception (status code 405)
     */
    @PUT
    @Consumes("application/json", "application/xml")
    fun updatePet( body: Pet): io.smallrye.mutiny.Uni<Response>

    /**
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @return Invalid input (status code 405)
     */
    @POST
    @Path("/{petId}")
    @Consumes("application/x-www-form-urlencoded")
    fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long,@FormParam(value = "name") name: kotlin.String?,@FormParam(value = "status") status: kotlin.String?): io.smallrye.mutiny.Uni<Response>

    /**
     * uploads an image
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param fileInputStream file to upload
     * @return successful operation (status code 200)
     */
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes("multipart/form-data")
    @Produces("application/json")
    fun uploadFile(@PathParam("petId") petId: kotlin.Long,@FormParam(value = "additionalMetadata") additionalMetadata: kotlin.String?, @FormParam(value = "file") fileInputStream: InputStream?): io.smallrye.mutiny.Uni<Response>
}
