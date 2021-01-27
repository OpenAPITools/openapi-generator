package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.PetApiService;
import org.openapitools.api.factories.PetApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.multipart.FormDataParam;
import com.sun.jersey.multipart.FormDataBodyPart;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/pet")


@io.swagger.annotations.Api(description = "the pet API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class PetApi  {
   private final PetApiService delegate = PetApiServiceFactory.getPetApi();

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response addPet(
        @ApiParam(value = "Pet object that needs to be added to the store", required = true) @NotNull @Valid  Pet body,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPet(body,securityContext);
    }
    @DELETE
    @Path("/{petId}")
    
    
    @io.swagger.annotations.ApiOperation(value = "Deletes a pet", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid pet value", response = Void.class) })
    public Response deletePet(
        @ApiParam(value = "Pet id to delete", required = true) @PathParam("petId") @NotNull  Long petId,
        @ApiParam(value = "" )@HeaderParam("api_key") String apiKey,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deletePet(petId,apiKey,securityContext);
    }
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid status value", response = Void.class) })
    public Response findPetsByStatus(
        @ApiParam(value = "Status values that need to be considered for filter", required = true) @QueryParam("status") @NotNull @Valid  List<String> status,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByStatus(status,securityContext);
    }
    @GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "Set", authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "Set"),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid tag value", response = Void.class) })
    public Response findPetsByTags(
        @ApiParam(value = "Tags to filter by", required = true) @QueryParam("tags") @NotNull @Valid  Set<String> tags,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByTags(tags,securityContext);
    }
    @GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "api_key")
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    public Response getPetById(
        @ApiParam(value = "ID of pet to return", required = true) @PathParam("petId") @NotNull  Long petId,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetById(petId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @io.swagger.annotations.ApiOperation(value = "Update an existing pet", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 405, message = "Validation exception", response = Void.class) })
    public Response updatePet(
        @ApiParam(value = "Pet object that needs to be added to the store", required = true) @NotNull @Valid  Pet body,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePet(body,securityContext);
    }
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @io.swagger.annotations.ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    public Response updatePetWithForm(
        @ApiParam(value = "ID of pet that needs to be updated", required = true) @PathParam("petId") @NotNull  Long petId,
        @ApiParam(value = "Updated name of the pet")  @FormParam("name")  String name,
        @ApiParam(value = "Updated status of the pet")  @FormParam("status")  String status,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePetWithForm(petId,name,status,securityContext);
    }
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @io.swagger.annotations.ApiOperation(value = "uploads an image", notes = "", response = ModelApiResponse.class, authorizations = {
        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @io.swagger.annotations.ApiResponses(value = { 
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    public Response uploadFile(
        @ApiParam(value = "ID of pet to update", required = true) @PathParam("petId") @NotNull  Long petId,
        @FormDataParam("additionalMetadata")  String additionalMetadata,
        @FormDataParam("file") FormDataBodyPart fileBodypart,
        @Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFile(petId,additionalMetadata,fileBodypart,securityContext);
    }
}
