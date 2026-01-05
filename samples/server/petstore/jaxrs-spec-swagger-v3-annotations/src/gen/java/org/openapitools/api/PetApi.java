package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.*;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
* Represents a collection of functions to interact with the API endpoints.
*/
@Path("/pet")
@Tag(name = "pet")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen", comments = "Generator version: 7.19.0-SNAPSHOT")
public class PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Add a new pet to the store", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Successful operation"),
        @ApiResponse(responseCode = "405", description = "Invalid input")
    })
    public Response addPet(@Valid @NotNull Pet pet) {
        return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{petId}")
    @Operation(summary = "Deletes a pet", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid pet value")
    })
    public Response deletePet(@PathParam("petId") Long petId) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid status value")
    })
    public Response findPetsByStatus(@QueryParam("status") @NotNull   List<String> status) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by tags", description = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid tag value")
    })
    public Response findPetsByTags(@QueryParam("tags") @NotNull   Set<String> tags) {
        return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found")
    })
    public Response getPetById(@PathParam("petId") Long petId) {
        return Response.ok().entity("magic!").build();
    }

    @PUT
    @Consumes({ "application/json", "application/xml" })
    @Operation(summary = "Update an existing pet", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Successful operation"),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception")
    })
    public Response updatePet(@Valid @NotNull Pet pet) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(summary = "Updates a pet in the store with form data", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "Successful operation"),
        @ApiResponse(responseCode = "405", description = "Invalid input")
    })
    public Response updatePetWithForm(@PathParam("petId") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status) {
        return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "")
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation")
    })
    public Response uploadFile(@PathParam("petId") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream _fileInputStream) {
        return Response.ok().entity("magic!").build();
    }
}
