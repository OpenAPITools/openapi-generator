package org.openapitools.server.apis;

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))
interface PetApi {

    @POST
    @Path("/pet")
    @Consumes("application/json", "application/xml")
    fun addPet( body: Pet): io.smallrye.mutiny.Uni<Response>

    @DELETE
    @Path("/pet/{petId}")
    fun deletePet(@PathParam("petId") petId: kotlin.Long,@HeaderParam("api_key")   apiKey: kotlin.String?): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/pet/findByStatus")
    @Produces("application/xml", "application/json")
    fun findPetsByStatus(@QueryParam("status")   status: kotlin.collections.List<kotlin.String>): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/pet/findByTags")
    @Produces("application/xml", "application/json")
    fun findPetsByTags(@QueryParam("tags")   tags: kotlin.collections.List<kotlin.String>): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/pet/{petId}")
    @Produces("application/xml", "application/json")
    fun getPetById(@PathParam("petId") petId: kotlin.Long): io.smallrye.mutiny.Uni<Response>

    @PUT
    @Path("/pet")
    @Consumes("application/json", "application/xml")
    fun updatePet( body: Pet): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/pet/{petId}")
    @Consumes("application/x-www-form-urlencoded")
    fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long,@FormParam(value = "name") name: kotlin.String?,@FormParam(value = "status") status: kotlin.String?): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes("multipart/form-data")
    @Produces("application/json")
    fun uploadFile(@PathParam("petId") petId: kotlin.Long,@FormParam(value = "additionalMetadata") additionalMetadata: kotlin.String?, @FormParam(value = "file") fileInputStream: InputStream?): io.smallrye.mutiny.Uni<Response>
}
