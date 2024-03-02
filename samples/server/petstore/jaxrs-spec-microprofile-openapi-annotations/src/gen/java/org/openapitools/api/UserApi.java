package org.openapitools.api;

import java.time.LocalDateTime;
import org.openapitools.model.User;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;




import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
   info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        title = "user", version="1.0.0", description="Operations about user",
        license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
   ),
   tags = @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user", description="Operations about user")
)
@org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user", description="Operations about user")
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
@Path("/user")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class UserApi {

    @POST
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "createUser", summary = "Create user", description = "This can only be done by the logged in user.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response createUser(@Valid @NotNull User body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithArray")
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "createUsersWithArrayInput", summary = "Creates list of users with given input array", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response createUsersWithArrayInput(@Valid @NotNull List<User> body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithList")
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "createUsersWithListInput", summary = "Creates list of users with given input array", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response createUsersWithListInput(@Valid @NotNull List<User> body) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "deleteUser", summary = "Delete user", description = "This can only be done by the logged in user.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid username supplied",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "User not found",  content = {
                
            })
        })
    public Response deleteUser(@PathParam("username") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="The name that needs to be deleted") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "getUserByName", summary = "Get user by user name", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = User.class)),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = User.class))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid username supplied",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "User not found",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response getUserByName(@PathParam("username") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="The name that needs to be fetched. Use user1 for testing.") String username) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "loginUser", summary = "Logs user into the system", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation", headers = { 
                @org.eclipse.microprofile.openapi.annotations.headers.Header(name = "X-Rate-Limit", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.INTEGER), description = "calls per hour allowed by the user"),
                @org.eclipse.microprofile.openapi.annotations.headers.Header(name = "X-Expires-After", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.STRING), description = "date in UTC when token expires")
            }, content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = String.class)),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = String.class))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid username/password supplied",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response loginUser(@QueryParam("username") @NotNull  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="The user name for login")  String username,@QueryParam("password") @NotNull  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="The password for login in clear text")  String password) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "logoutUser", summary = "Logs out current logged in user session", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            })
        })
    public Response logoutUser() {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/{username}")
    
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "updateUser", summary = "Updated user", description = "This can only be done by the logged in user.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="user")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid user supplied",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "User not found",  content = {
                
            })
        })
    public Response updateUser(@PathParam("username") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="name that need to be deleted") String username,@Valid @NotNull User body) {
        return Response.ok().entity("magic!").build();
    }
}
