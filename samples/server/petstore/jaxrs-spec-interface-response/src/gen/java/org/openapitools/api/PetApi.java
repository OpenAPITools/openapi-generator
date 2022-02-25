package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/pet")
@Api(description = "the pet API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @ApiOperation(value = "Add a new pet to the store", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    Response addPet(@Valid @NotNull Pet body);

    @DELETE
    @Path("/{petId}")
    @ApiOperation(value = "Deletes a pet", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @ApiResponse(code = 400, message = "Invalid pet value", response = Void.class) })
    Response deletePet(@PathParam("petId") @ApiParam("Pet id to delete") Long petId,@HeaderParam("api_key")   String apiKey);

    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", response = Void.class, responseContainer = "List") })
    Response findPetsByStatus(@QueryParam("status") @NotNull  @ApiParam("Status values that need to be considered for filter")  List<String> status);

    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "Set"),
        @ApiResponse(code = 400, message = "Invalid tag value", response = Void.class, responseContainer = "Set") })
    Response findPetsByTags(@QueryParam("tags") @NotNull  @ApiParam("Tags to filter by")  Set<String> tags);

    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    Response getPetById(@PathParam("petId") @ApiParam("ID of pet to return") Long petId);

    @PUT
    @Consumes({ "application/json", "application/xml" })
    @ApiOperation(value = "Update an existing pet", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class),
        @ApiResponse(code = 405, message = "Validation exception", response = Void.class) })
    Response updatePet(@Valid @NotNull Pet body);

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    Response updatePetWithForm(@PathParam("petId") @ApiParam("ID of pet that needs to be updated") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    Response uploadFile(@PathParam("petId") @ApiParam("ID of pet to update") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream _fileInputStream);
}
