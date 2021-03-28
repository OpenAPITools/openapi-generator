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

@Path("")
@Api(description = "the Pet API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface PetApi {

    @POST
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @ApiOperation(value = "Add a new pet to the store", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    AddPetResponse addPet(@Valid @NotNull Pet body);

    public static class AddPetResponse extends org.openapitools.api.support.ResponseWrapper {
        private AddPetResponse(Response delegate) {
            super(delegate);
        }
        public static AddPetResponse with200() {
            return new AddPetResponse(Response.status(200).build());
        }
        public static AddPetResponse with405() {
            return new AddPetResponse(Response.status(405).build());
        }
        public static AddPetResponse withCustomResponse(Response response) {
            return new AddPetResponse(response);
        }
    }

    @DELETE
    @Path("/pet/{petId}")
    @ApiOperation(value = "Deletes a pet", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Void.class),
        @ApiResponse(code = 400, message = "Invalid pet value", response = Void.class) })
    DeletePetResponse deletePet(@PathParam("petId") @ApiParam("Pet id to delete") Long petId,@HeaderParam("api_key")    String apiKey);

    public static class DeletePetResponse extends org.openapitools.api.support.ResponseWrapper {
        private DeletePetResponse(Response delegate) {
            super(delegate);
        }
        public static DeletePetResponse with200() {
            return new DeletePetResponse(Response.status(200).build());
        }
        public static DeletePetResponse with400() {
            return new DeletePetResponse(Response.status(400).build());
        }
        public static DeletePetResponse withCustomResponse(Response response) {
            return new DeletePetResponse(response);
        }
    }

    @GET
    @Path("/pet/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", response = Void.class, responseContainer = "List") })
    FindPetsByStatusResponse findPetsByStatus(@QueryParam("status") @NotNull   @ApiParam("Status values that need to be considered for filter")  List<String> status);

    public static class FindPetsByStatusResponse extends org.openapitools.api.support.ResponseWrapper {
        private FindPetsByStatusResponse(Response delegate) {
            super(delegate);
        }
        public static FindPetsByStatusResponse with200ApplicationXml(List<Pet> entity) {
            return new FindPetsByStatusResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static FindPetsByStatusResponse with200ApplicationJson(List<Pet> entity) {
            return new FindPetsByStatusResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static FindPetsByStatusResponse with400() {
            return new FindPetsByStatusResponse(Response.status(400).build());
        }
        public static FindPetsByStatusResponse withCustomResponse(Response response) {
            return new FindPetsByStatusResponse(response);
        }
    }

    @GET
    @Path("/pet/findByTags")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "Set"),
        @ApiResponse(code = 400, message = "Invalid tag value", response = Void.class, responseContainer = "Set") })
    FindPetsByTagsResponse findPetsByTags(@QueryParam("tags") @NotNull   @ApiParam("Tags to filter by")  Set<String> tags);

    public static class FindPetsByTagsResponse extends org.openapitools.api.support.ResponseWrapper {
        private FindPetsByTagsResponse(Response delegate) {
            super(delegate);
        }
        public static FindPetsByTagsResponse with200ApplicationXml(Set<Pet> entity) {
            return new FindPetsByTagsResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static FindPetsByTagsResponse with200ApplicationJson(Set<Pet> entity) {
            return new FindPetsByTagsResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static FindPetsByTagsResponse with400() {
            return new FindPetsByTagsResponse(Response.status(400).build());
        }
        public static FindPetsByTagsResponse withCustomResponse(Response response) {
            return new FindPetsByTagsResponse(response);
        }
    }

    @GET
    @Path("/pet/{petId}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", authorizations = {
        
        @Authorization(value = "api_key")
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Void.class) })
    GetPetByIdResponse getPetById(@PathParam("petId") @ApiParam("ID of pet to return") Long petId);

    public static class GetPetByIdResponse extends org.openapitools.api.support.ResponseWrapper {
        private GetPetByIdResponse(Response delegate) {
            super(delegate);
        }
        public static GetPetByIdResponse with200ApplicationXml(Pet entity) {
            return new GetPetByIdResponse(Response.status(200).header("Content-Type", "application/xml").entity(entity).build());
        }
        public static GetPetByIdResponse with200ApplicationJson(Pet entity) {
            return new GetPetByIdResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static GetPetByIdResponse with400() {
            return new GetPetByIdResponse(Response.status(400).build());
        }
        public static GetPetByIdResponse with404() {
            return new GetPetByIdResponse(Response.status(404).build());
        }
        public static GetPetByIdResponse withCustomResponse(Response response) {
            return new GetPetByIdResponse(response);
        }
    }

    @PUT
    @Path("/pet")
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
    UpdatePetResponse updatePet(@Valid @NotNull Pet body);

    public static class UpdatePetResponse extends org.openapitools.api.support.ResponseWrapper {
        private UpdatePetResponse(Response delegate) {
            super(delegate);
        }
        public static UpdatePetResponse with200() {
            return new UpdatePetResponse(Response.status(200).build());
        }
        public static UpdatePetResponse with400() {
            return new UpdatePetResponse(Response.status(400).build());
        }
        public static UpdatePetResponse with404() {
            return new UpdatePetResponse(Response.status(404).build());
        }
        public static UpdatePetResponse with405() {
            return new UpdatePetResponse(Response.status(405).build());
        }
        public static UpdatePetResponse withCustomResponse(Response response) {
            return new UpdatePetResponse(response);
        }
    }

    @POST
    @Path("/pet/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = Void.class) })
    UpdatePetWithFormResponse updatePetWithForm(@PathParam("petId") @ApiParam("ID of pet that needs to be updated") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);

    public static class UpdatePetWithFormResponse extends org.openapitools.api.support.ResponseWrapper {
        private UpdatePetWithFormResponse(Response delegate) {
            super(delegate);
        }
        public static UpdatePetWithFormResponse with405() {
            return new UpdatePetWithFormResponse(Response.status(405).build());
        }
        public static UpdatePetWithFormResponse withCustomResponse(Response response) {
            return new UpdatePetWithFormResponse(response);
        }
    }

    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    UploadFileResponse uploadFile(@PathParam("petId") @ApiParam("ID of pet to update") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream fileInputStream);

    public static class UploadFileResponse extends org.openapitools.api.support.ResponseWrapper {
        private UploadFileResponse(Response delegate) {
            super(delegate);
        }
        public static UploadFileResponse with200ApplicationJson(ModelApiResponse entity) {
            return new UploadFileResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static UploadFileResponse withCustomResponse(Response response) {
            return new UploadFileResponse(response);
        }
    }

    @POST
    @Path("/fake/{petId}/uploadImageWithRequiredFile")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image (required)", notes = "", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read:pets", description = "read your pets") })
         }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    UploadFileWithRequiredFileResponse uploadFileWithRequiredFile(@PathParam("petId") @ApiParam("ID of pet to update") Long petId, @FormParam(value = "requiredFile") InputStream requiredFileInputStream,@FormParam(value = "additionalMetadata")  String additionalMetadata);

    public static class UploadFileWithRequiredFileResponse extends org.openapitools.api.support.ResponseWrapper {
        private UploadFileWithRequiredFileResponse(Response delegate) {
            super(delegate);
        }
        public static UploadFileWithRequiredFileResponse with200ApplicationJson(ModelApiResponse entity) {
            return new UploadFileWithRequiredFileResponse(Response.status(200).header("Content-Type", "application/json").entity(entity).build());
        }
        public static UploadFileWithRequiredFileResponse withCustomResponse(Response response) {
            return new UploadFileWithRequiredFileResponse(response);
        }
    }
}
