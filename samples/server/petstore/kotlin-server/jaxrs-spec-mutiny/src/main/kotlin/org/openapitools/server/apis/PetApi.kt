package org.openapitools.server.apis;

import java.io.File
import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.10.0-SNAPSHOT")
interface PetApi {

    @POST
    @Path("/pet")
    @Consumes("application/json", "application/xml")
    fun addPet( body: Pet): io.smallrye.mutiny.Uni<Response>

    @DELETE
    @Path("/pet/{petId}")
    fun deletePet(@PathParam("petId") petId: Long,@HeaderParam("api_key")   apiKey: String?): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/pet/findByStatus")
    @Produces("application/xml", "application/json")
    fun findPetsByStatus(@QueryParam("status")   status: List<String>): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/pet/findByTags")
    @Produces("application/xml", "application/json")
    fun findPetsByTags(@QueryParam("tags")   tags: List<String>): io.smallrye.mutiny.Uni<Response>

    @GET
    @Path("/pet/{petId}")
    @Produces("application/xml", "application/json")
    fun getPetById(@PathParam("petId") petId: Long): io.smallrye.mutiny.Uni<Response>

    @PUT
    @Path("/pet")
    @Consumes("application/json", "application/xml")
    fun updatePet( body: Pet): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/pet/{petId}")
    @Consumes("application/x-www-form-urlencoded")
    fun updatePetWithForm(@PathParam("petId") petId: Long,@FormParam(value = "name") name: String?,@FormParam(value = "status") status: String?): io.smallrye.mutiny.Uni<Response>

    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes("multipart/form-data")
    @Produces("application/json")
    fun uploadFile(@PathParam("petId") petId: Long,@FormParam(value = "additionalMetadata") additionalMetadata: String?, @FormParam(value = "file") fileInputStream: InputStream?): io.smallrye.mutiny.Uni<Response>
}
