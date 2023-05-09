package org.openapitools.api;

import java.util.Map;
import org.openapitools.model.Order;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import io.swagger.annotations.*;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
   info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        title = "store", version="1.0.0", description="Access to Petstore orders",
        license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
   ),
   tags = @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="store", description="Access to Petstore orders")
)
@org.eclipse.microprofile.openapi.annotations.tags.Tag(name="store", description="Access to Petstore orders")
@org.eclipse.microprofile.openapi.annotations.security.SecuritySchemes(value = {
    @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "petstore_auth",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.OAUTH2,
         description = "",
         flows = @org.eclipse.microprofile.openapi.annotations.security.OAuthFlows(
            implicit = @org.eclipse.microprofile.openapi.annotations.security.OAuthFlow(authorizationUrl = "http://petstore.swagger.io/api/oauth/dialog",
            tokenUrl = "",
            refreshUrl = "",
            scopes = {
                @org.eclipse.microprofile.openapi.annotations.security.OAuthScope(name = "write:pets", description = "modify pets in your account"),
                @org.eclipse.microprofile.openapi.annotations.security.OAuthScope(name = "read:pets", description = "read your pets")
                 })) 
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "api_key",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.APIKEY,
         description = "",
         apiKeyName = "api_key",
         in = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeIn.HEADER
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "api_key_query",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.APIKEY,
         description = "",
         apiKeyName = "api_key_query",
         in = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeIn.QUERY
    ), @org.eclipse.microprofile.openapi.annotations.security.SecurityScheme(
         securitySchemeName = "http_basic_test",
         type = org.eclipse.microprofile.openapi.annotations.enums.SecuritySchemeType.HTTP,
         description = "",
         scheme = "basic"
    )
})
@Api(description = "the store API")
@Path("/store")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class StoreApi {

    @DELETE
    @Path("/order/{order_id}")
    @ApiOperation(value = "Delete purchase order by ID", notes = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors", response = Void.class, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "deleteOrder", summary = "Delete purchase order by ID", description = "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="store")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid ID supplied",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "Order not found",  content = {
                
            })
        })
    public Response deleteOrder(@PathParam("order_id") @ApiParam("ID of the order that needs to be deleted") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of the order that needs to be deleted") String orderId) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/inventory")
    @Produces({ "application/json" })
    @ApiOperation(value = "Returns pet inventories by status", notes = "Returns a map of status codes to quantities", response = Integer.class, responseContainer = "Map", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Map.class, responseContainer = "Map")
    })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "api_key")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "getInventory", summary = "Returns pet inventories by status", description = "Returns a map of status codes to quantities")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="store")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Map.class))
            })
        })
    public Response getInventory() {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/order/{order_id}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find purchase order by ID", notes = "For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions", response = Order.class, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Order not found", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "getOrderById", summary = "Find purchase order by ID", description = "For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="store")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Order.class)),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Order.class))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid ID supplied",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "Order not found",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response getOrderById(@PathParam("order_id") @Min(1L) @Max(5L) @ApiParam("ID of pet that needs to be fetched") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of pet that needs to be fetched") Long orderId) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/order")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Place an order for a pet", notes = "", response = Order.class, tags={ "store" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Order.class),
        @ApiResponse(code = 400, message = "Invalid Order", response = Void.class)
    })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "placeOrder", summary = "Place an order for a pet", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="store")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Order.class)),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Order.class))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid Order",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response placeOrder(@Valid @NotNull Order body) {
        return Response.ok().entity("magic!").build();
    }
}
