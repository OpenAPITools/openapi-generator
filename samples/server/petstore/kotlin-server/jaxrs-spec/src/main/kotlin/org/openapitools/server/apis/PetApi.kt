package org.openapitools.server.apis;

import java.io.File
import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.10.0-SNAPSHOT")
class PetApi {

    @POST
    @Consumes("application/json", "application/xml")
    suspend fun addPet( body: Pet): Response {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    suspend fun deletePet(@PathParam("petId") petId: Long,@HeaderParam("api_key")   apiKey: String?): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun findPetsByStatus(@QueryParam("status")   status: List<String>): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun findPetsByTags(@QueryParam("tags")   tags: List<String>): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun getPetById(@PathParam("petId") petId: Long): Response {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Consumes("application/json", "application/xml")
    suspend fun updatePet( body: Pet): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes("application/x-www-form-urlencoded")
    suspend fun updatePetWithForm(@PathParam("petId") petId: Long,@FormParam(value = "name") name: String?,@FormParam(value = "status") status: String?): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes("multipart/form-data")
    @Produces("application/json")
    suspend fun uploadFile(@PathParam("petId") petId: Long,@FormParam(value = "additionalMetadata") additionalMetadata: String?, @FormParam(value = "file") fileInputStream: InputStream?): Response {
        return Response.ok().entity("magic!").build();
    }
}
