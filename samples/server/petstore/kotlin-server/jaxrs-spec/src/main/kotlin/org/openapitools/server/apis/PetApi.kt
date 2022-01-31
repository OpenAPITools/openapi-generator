package org.openapitools.server.apis;

import org.openapitools.server.models.ModelApiResponse
import org.openapitools.server.models.Pet

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream
import java.util.Map
import java.util.List



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))class PetApi {

    @POST
    @Consumes("application/json", "application/xml")
    suspend fun addPet( body: Pet): Response {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    suspend fun deletePet(@PathParam("petId") petId: kotlin.Long,@HeaderParam("api_key")   apiKey: kotlin.String?): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun findPetsByStatus(@QueryParam("status")   status: kotlin.collections.List<kotlin.String>): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun findPetsByTags(@QueryParam("tags")   tags: kotlin.collections.List<kotlin.String>): Response {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Produces("application/xml", "application/json")
    suspend fun getPetById(@PathParam("petId") petId: kotlin.Long): Response {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Consumes("application/json", "application/xml")
    suspend fun updatePet( body: Pet): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes("application/x-www-form-urlencoded")
    suspend fun updatePetWithForm(@PathParam("petId") petId: kotlin.Long,@FormParam(value = "name") name: kotlin.String?,@FormParam(value = "status") status: kotlin.String?): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes("multipart/form-data")
    @Produces("application/json")
    suspend fun uploadFile(@PathParam("petId") petId: kotlin.Long,@FormParam(value = "additionalMetadata") additionalMetadata: kotlin.String?, @FormParam(value = "file") fileInputStream: InputStream?): Response {
        return Response.ok().entity("magic!").build();
    }
}
