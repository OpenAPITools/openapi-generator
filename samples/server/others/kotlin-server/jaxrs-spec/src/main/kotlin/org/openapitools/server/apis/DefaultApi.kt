package org.openapitools.server.apis;

import java.math.BigDecimal

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.10.0-SNAPSHOT")
class DefaultApi {

    @GET
    suspend fun findPetsByStatus(@PathParam("path_default") pathDefault: String,@PathParam("path_nullable") pathNullable: String,@QueryParam("query_default") @DefaultValue("available")   queryDefault: String,@QueryParam("query_default_enum") @DefaultValue("B")   queryDefaultEnum: String,@QueryParam("query_default_int") @DefaultValue("3")   queryDefaultInt: BigDecimal,@HeaderParam("header_default")  @DefaultValue("available")  headerDefault: String,@HeaderParam("header_default_enum")  @DefaultValue("B")  headerDefaultEnum: String,@HeaderParam("header_default_int")  @DefaultValue("3")  headerDefaultInt: BigDecimal,@CookieParam("cookie_default") @DefaultValue("available")  cookieDefault: String,@CookieParam("cookie_default_enum") @DefaultValue("B")  cookieDefaultEnum: String,@CookieParam("cookie_default_int") @DefaultValue("3")  cookieDefaultInt: BigDecimal,@QueryParam("query_nullable")   queryNullable: String?,@HeaderParam("header_nullable")   headerNullable: String?,@CookieParam("cookie_nullable")  cookieNullable: String?,@QueryParam("\$query-\$dollar-sign")   dollarQueryDollarDollarSign: String?): Response {
        return Response.ok().entity("magic!").build();
    }
}
