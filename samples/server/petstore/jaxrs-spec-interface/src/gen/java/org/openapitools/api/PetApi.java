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

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/pet")
@Api(description = "the pet API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.17.0-SNAPSHOT")
public interface PetApi {

    /**
     * 
     *
     * @param body Pet object that needs to be added to the store
     * @return successful operation
     * @return Invalid input
     */
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
    void addPet(@Valid @NotNull Pet body);


    /**
     * 
     *
     * @param petId Pet id to delete
     * @return successful operation
     * @return Invalid pet value
     */
    @DELETE
    @Path("/{petId}")
    @ApiOperation(value = "Deletes a pet", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @io.swagger.annotations.ApiImplicitParams({
        @io.swagger.annotations.ApiImplicitParam(name = "api_key", value = "",  dataType = "String", paramType = "header")
    })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @ApiResponse(code = 400, message = "Invalid pet value", response = Void.class) })
    void deletePet(@PathParam("petId") @ApiParam("Pet id to delete") Long petId);


    /**
     * Multiple status values can be provided with comma separated strings
     *
     * @param status Status values that need to be considered for filter
     * @return successful operation
     * @return Invalid status value
     */
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
    List<Pet> findPetsByStatus(@QueryParam("status") @NotNull  @ApiParam("Status values that need to be considered for filter")  List<String> status);


    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     * @param tags Tags to filter by
     * @return successful operation
     * @return Invalid tag value
     */
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
    Set<Pet> findPetsByTags(@QueryParam("tags") @NotNull  @ApiParam("Tags to filter by")  Set<String> tags);


    /**
     * Returns a single pet
     *
     * @param petId ID of pet to return
     * @return successful operation
     * @return Invalid ID supplied
     * @return Pet not found
     */
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
    Pet getPetById(@PathParam("petId") @ApiParam("ID of pet to return") Long petId);


    /**
     * 
     *
     * @param body Pet object that needs to be added to the store
     * @return successful operation
     * @return Invalid ID supplied
     * @return Pet not found
     * @return Validation exception
     */
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
    void updatePet(@Valid @NotNull Pet body);


    /**
     * 
     *
     * @param petId ID of pet that needs to be updated
     * @param name Updated name of the pet
     * @param status Updated status of the pet
     * @return Invalid input
     */
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
    void updatePetWithForm(@PathParam("petId") @ApiParam("ID of pet that needs to be updated") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);


    /**
     * 
     *
     * @param petId ID of pet to update
     * @param additionalMetadata Additional data to pass to server
     * @param _file file to upload
     * @return successful operation
     */
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
    ModelApiResponse uploadFile(@PathParam("petId") @ApiParam("ID of pet to update") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream _fileInputStream);

}
