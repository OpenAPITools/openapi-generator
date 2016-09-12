package com.ibm.ws.petstoresample.api;

import com.ibm.ws.petstoresample.model.Pet;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;

@Path("/pets")

@Api(description = "the pets API")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen", date = "2016-06-06T11:04:02.369-04:00")

public class PetsApi  {

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Add a new pet to the store", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = void.class) })
    public Response addPet(Pet body) {
    	return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Deletes a pet", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid pet value", response = void.class) })
    public Response deletePet(@HeaderParam("api_key") String apiKey,@PathParam("petId") Long petId) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value", response = Pet.class, responseContainer = "List") })
    public Response findPetsByStatus(@QueryParam("status") List<String> status) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/findByTags")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List", authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid tag value", response = Pet.class, responseContainer = "List") })
    public Response findPetsByTags(@QueryParam("tags") List<String> tags) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Find pet by ID", notes = "Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions", response = Pet.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        }),
        @Authorization(value = "api_key")
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = Pet.class),
        @ApiResponse(code = 404, message = "Pet not found", response = Pet.class) })
    public Response getPetById(@PathParam("petId") Long petId) {
    	return Response.ok().entity("magic!").build();
    }

    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Update an existing pet", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        })
    }, tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied", response = void.class),
        @ApiResponse(code = 404, message = "Pet not found", response = void.class),
        @ApiResponse(code = 405, message = "Validation exception", response = void.class) })
    public Response updatePet(Pet body) {
    	return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = void.class, authorizations = {
        @Authorization(value = "petstore_auth", scopes = {
            @AuthorizationScope(scope = "write_pets", description = "modify pets in your account"),
            @AuthorizationScope(scope = "read_pets", description = "read your pets")
        })
    }, tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input", response = void.class) })
    public Response updatePetWithForm(@PathParam("petId") String petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status) {
    	return Response.ok().entity("magic!").build();
    }
}

