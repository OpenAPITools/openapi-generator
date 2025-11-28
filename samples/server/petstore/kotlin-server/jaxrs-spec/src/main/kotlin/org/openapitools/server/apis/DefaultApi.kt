package org.openapitools.server.apis;

import org.openapitools.server.models.ItemWithDollarAttributesAndExamples
import org.openapitools.server.models.ItemsItemIdSomethingItemSubIdGet200Response

import javax.ws.rs.*
import javax.ws.rs.core.Response


import java.io.InputStream



@Path("/")
@javax.annotation.Generated(value = arrayOf("org.openapitools.codegen.languages.KotlinServerCodegen"), comments = "Generator version: 7.18.0-SNAPSHOT")
class DefaultApi {

    @GET
    @Produces("application/json")
    suspend fun itemsItemIdSomethingItemSubIdGet(@PathParam("item\$Id") itemDollarId: kotlin.String,@PathParam("item\$SubId") itemDollarSubId: kotlin.String,@QueryParam("filter\$Type") @DefaultValue("SQ = \"; SBS = \\; DBS = \\\\; SD = $some")   filterDollarType: kotlin.String,@QueryParam("filter\$SubType") @DefaultValue("SQ = \"; SBS = \\; DBS = \\\\; SD = $some")   filterDollarSubType: kotlin.String,@HeaderParam("X-Custom_Header")   xCustomHeader: kotlin.String?,@HeaderParam("X-Custom_Header_two")   xCustomHeaderTwo: kotlin.String?): Response {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Consumes("application/x-www-form-urlencoded")
    @Produces("application/json")
    suspend fun itemsPost(@HeaderParam("X-Post_Header")   xPostHeader: kotlin.String?,@FormParam(value = "form$Name") formDollarName: kotlin.String?,@FormParam(value = "form$Value") formDollarValue: kotlin.String?): Response {
        return Response.ok().entity("magic!").build();
    }
}
