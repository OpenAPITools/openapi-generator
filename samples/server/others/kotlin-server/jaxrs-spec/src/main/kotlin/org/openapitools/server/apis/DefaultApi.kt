package org.openapitools.server.apis;


import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))
class DefaultApi {

    @GET
    suspend fun findPetsByStatus(@PathParam("path_default") pathDefault: kotlin.String,@PathParam("path_nullable") pathNullable: kotlin.String,@QueryParam("query_default") @DefaultValue("available")   queryDefault: kotlin.String,@HeaderParam("header_default")  @DefaultValue("available")  headerDefault: kotlin.String,@CookieParam("cookie_default") @DefaultValue("available")  cookieDefault: kotlin.String,@QueryParam("query_nullable")   queryNullable: kotlin.String?,@HeaderParam("header_nullable")   headerNullable: kotlin.String?,@CookieParam("cookie_nullable")  cookieNullable: kotlin.String?,@QueryParam("\$query-\$dollar-sign")   dollarQueryDollarDollarSign: kotlin.String?): Response {
        return Response.ok().entity("magic!").build();
    }
}
