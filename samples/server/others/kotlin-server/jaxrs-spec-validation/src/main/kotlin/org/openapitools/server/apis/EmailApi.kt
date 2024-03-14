package org.openapitools.server.apis;

import org.openapitools.server.models.EmailDefinition

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream
import javax.validation.constraints.*
import javax.validation.Valid


@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.5.0-SNAPSHOT")
class EmailApi {

    @GET
    @Produces("application/json")
    fun getEmails(@PathParam("from") @Email

 from: kotlin.String): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes("application/json")
    fun sendEmail(@Valid  emailDefinition: EmailDefinition?): Response {
        return Response.ok().entity("magic!").build();
    }
}
