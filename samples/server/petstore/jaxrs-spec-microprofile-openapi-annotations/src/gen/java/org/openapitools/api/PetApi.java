package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;




import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@org.eclipse.microprofile.openapi.annotations.OpenAPIDefinition(
   info = @org.eclipse.microprofile.openapi.annotations.info.Info(
        title = "pet", version="1.0.0", description="Everything about your Pets",
        license = @org.eclipse.microprofile.openapi.annotations.info.License(name = "Apache-2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
   ),
   tags = @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet", description="Everything about your Pets")
)
@org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet", description="Everything about your Pets")
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
@Path("/pet")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")
public class PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "addPet", summary = "Add a new pet to the store", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "405", description = "Invalid input",  content = {
                
            })
        })
    public Response addPet(@Valid @NotNull Pet body) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{petId}")
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "deletePet", summary = "Deletes a pet", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid pet value",  content = {
                
            })
        })
    public Response deletePet(@PathParam("petId") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Pet id to delete") Long petId) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "findPetsByStatus", summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Pet.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY )),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Pet.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY ))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid status value",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response findPetsByStatus(@QueryParam("status") @NotNull  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Status values that need to be considered for filter")  List<String> status) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "findPetsByTags", summary = "Finds Pets by tags", description = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Pet.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY , uniqueItems = true )),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Pet.class, type = org.eclipse.microprofile.openapi.annotations.enums.SchemaType.ARRAY , uniqueItems = true ))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid tag value",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response findPetsByTags(@QueryParam("tags") @NotNull  @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="Tags to filter by")  Set<String> tags) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
             @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "api_key")
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "getPetById", summary = "Find pet by ID", description = "Returns a single pet")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Pet.class)),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = Pet.class))
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid ID supplied",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "Pet not found",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/xml"),
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json")
            })
        })
    public Response getPetById(@PathParam("petId") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of pet to return") Long petId) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Consumes({ "application/json", "application/xml" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "updatePet", summary = "Update an existing pet", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "400", description = "Invalid ID supplied",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "404", description = "Pet not found",  content = {
                
            }),
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "405", description = "Validation exception",  content = {
                
            })
        })
    public Response updatePet(@Valid @NotNull Pet body) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "updatePetWithForm", summary = "Updates a pet in the store with form data", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "405", description = "Invalid input",  content = {
                
            })
        })
    public Response updatePetWithForm(@PathParam("petId") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of pet that needs to be updated") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirements(value={
        @org.eclipse.microprofile.openapi.annotations.security.SecurityRequirement(name = "petstore_auth", scopes = {  "write:pets",  "read:pets"  })
    })
    @org.eclipse.microprofile.openapi.annotations.Operation(operationId = "uploadFile", summary = "uploads an image", description = "")
    @org.eclipse.microprofile.openapi.annotations.tags.Tag(name="pet")
    @org.eclipse.microprofile.openapi.annotations.responses.APIResponses(value = { 
            @org.eclipse.microprofile.openapi.annotations.responses.APIResponse(responseCode = "200", description = "successful operation",  content = { 
                @org.eclipse.microprofile.openapi.annotations.media.Content(mediaType="application/json", schema = @org.eclipse.microprofile.openapi.annotations.media.Schema(implementation = ModelApiResponse.class))
            })
        })
    public Response uploadFile(@PathParam("petId") @org.eclipse.microprofile.openapi.annotations.parameters.Parameter(description="ID of pet to update") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream _fileInputStream) {
        return Response.ok().entity("magic!").build();
    }
}
