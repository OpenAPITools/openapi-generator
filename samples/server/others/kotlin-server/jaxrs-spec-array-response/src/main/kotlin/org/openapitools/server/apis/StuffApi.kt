package org.openapitools.server.apis;

import org.openapitools.server.models.Stuff

import jakarta.ws.rs.*
import jakarta.ws.rs.core.Response


import java.io.InputStream



@Path("")
@jakarta.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.26.0-SNAPSHOT")
interface StuffApi {

    /**
     * Finds stuff
     *
     * Finds stuff
     * @return successful operation (status code 200)
     *         or Invalid status value (status code 400)
     */
    @GET
    @Path("/stuff")
    @Produces("application/json")
    fun findStuff(): kotlin.collections.List<Stuff>

    /**
     * Finds unique stuff
     *
     * Finds unique stuff
     * @return successful operation (status code 200)
     *         or Invalid status value (status code 400)
     */
    @GET
    @Path("/uniquestuff")
    @Produces("application/json")
    fun findUniqueStuff(): kotlin.collections.Set<Stuff>
}
