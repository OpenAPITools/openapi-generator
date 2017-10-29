package io.swagger.api;

import java.io.File;
import io.swagger.model.ModelApiResponse;
import io.swagger.model.Pet;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.MediaType;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponses;
import io.swagger.annotations.ApiResponse;
import io.swagger.jaxrs.PATCH;
import javax.validation.constraints.*;
import javax.validation.Valid;

/**
 * Swagger Petstore
 *
 * <p>This spec is mainly for testing Petstore server and contains fake endpoints, models. Please do not use this for any other purpose. Special characters: \" \\
 *
 */
@Path("/")
@Api(value = "/", description = "")
public interface PetApi  {

    /**
     * Add a new pet to the store
     *
     * 
     *
     */
    @POST
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Add a new pet to the store", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input") })
    public void addPet(@Valid Pet body);

    /**
     * Deletes a pet
     *
     * 
     *
     */
    @DELETE
    @Path("/pet/{petId}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Deletes a pet", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid pet value") })
    public void deletePet(@PathParam("petId") Long petId, @HeaderParam("api_key") String apiKey);

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     */
    @GET
    @Path("/pet/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by status", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid status value") })
    public List<Pet> findPetsByStatus(@QueryParam("status") @NotNull List<String> status);

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     */
    @GET
    @Path("/pet/findByTags")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Finds Pets by tags", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
        @ApiResponse(code = 400, message = "Invalid tag value") })
    public List<Pet> findPetsByTags(@QueryParam("tags") @NotNull List<String> tags);

    /**
     * Find pet by ID
     *
     * Returns a single pet
     *
     */
    @GET
    @Path("/pet/{petId}")
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Find pet by ID", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = Pet.class),
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Pet not found") })
    public Pet getPetById(@PathParam("petId") Long petId);

    /**
     * Update an existing pet
     *
     * 
     *
     */
    @PUT
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Update an existing pet", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid ID supplied"),
        @ApiResponse(code = 404, message = "Pet not found"),
        @ApiResponse(code = 405, message = "Validation exception") })
    public void updatePet(@Valid Pet body);

    /**
     * Updates a pet in the store with form data
     *
     * 
     *
     */
    @POST
    @Path("/pet/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/xml", "application/json" })
    @ApiOperation(value = "Updates a pet in the store with form data", tags={ "pet",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 405, message = "Invalid input") })
    public void updatePetWithForm(@PathParam("petId") Long petId, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status);

    /**
     * uploads an image
     *
     * 
     *
     */
    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @ApiOperation(value = "uploads an image", tags={ "pet" })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class) })
    public ModelApiResponse uploadFile(@PathParam("petId") Long petId, @Multipart(value = "additionalMetadata", required = false)  String additionalMetadata,  @Multipart(value = "file" , required = false) Attachment fileDetail);
}

