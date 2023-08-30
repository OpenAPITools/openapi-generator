package org.openapitools.server.apis;


import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"))
class DefaultApi {

    @GET
    suspend fun findPetsByStatus(@PathParam("") pathDefault: kotlin.String,@PathParam("") pathNullable: kotlin.String,@QueryParam("") @DefaultValue("available")   queryDefault: kotlin.String,@QueryParam("") @DefaultValue(B)   queryDefaultEnum: kotlin.String,@QueryParam("") @DefaultValue(3)   queryDefaultInt: java.math.BigDecimal,@HeaderParam("header_default")  @DefaultValue("available")  headerDefault: kotlin.String,@HeaderParam("header_default_enum")  @DefaultValue(B)  headerDefaultEnum: kotlin.String,@HeaderParam("header_default_int")  @DefaultValue(3)  headerDefaultInt: java.math.BigDecimal,@CookieParam("") @DefaultValue("available")  cookieDefault: kotlin.String,@CookieParam("") @DefaultValue(B)  cookieDefaultEnum: kotlin.String,@CookieParam("") @DefaultValue(3)  cookieDefaultInt: java.math.BigDecimal,@QueryParam("")   queryNullable: kotlin.String?,@HeaderParam("header_nullable")   headerNullable: kotlin.String?,@CookieParam("")  cookieNullable: kotlin.String?,@QueryParam("")   dollarQueryDollarDollarSign: kotlin.String?): Response {
        return Response.ok().entity("magic!").build();
    }
}
