package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;

import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import org.apache.cxf.jaxrs.ext.multipart.*;

import io.swagger.v3.oas.annotations.OpenAPIDefinition;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.info.Info;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;

/**
 * OpenAPI Petstore
 *
 * <p>This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
 *
 */
@Path("/pet")
@OpenAPIDefinition(
    info = @Info(
        title = "OpenAPI Petstore",
        description = "This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.",
        version = "1.0.0"
    )
)
public interface PetApi  {

    /**
     * Add a new pet to the store
     *
     * 
     *
     */
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "addPet", summary = "Add a new pet to the store",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public Pet addPet(Pet pet);

    /**
     * Deletes a pet
     *
     * 
     *
     */
    @DELETE
    @Path("/{petId}")
    @Operation(operationId = "deletePet", summary = "Deletes a pet",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "400", description = "Invalid pet value") })
    public void deletePet(@PathParam("petId") Long petId, @HeaderParam("api_key")  String apiKey);

    /**
     * Finds Pets by status
     *
     * Multiple status values can be provided with comma separated strings
     *
     */
    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "findPetsByStatus", summary = "Finds Pets by status",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid status value") })
    public List<Pet> findPetsByStatus(@QueryParam("status") List<String> status);

    /**
     * Finds Pets by tags
     *
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     *
     */
    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "findPetsByTags", summary = "Finds Pets by tags", deprecated = true,  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(array = @ArraySchema(schema = @Schema(implementation = Pet.class)))),
        @ApiResponse(responseCode = "400", description = "Invalid tag value") })
    public List<Pet> findPetsByTags(@QueryParam("tags") List<String> tags);

    /**
     * Find pet by ID
     *
     * Returns a single pet
     *
     */
    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "getPetById", summary = "Find pet by ID",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found") })
    public Pet getPetById(@PathParam("petId") Long petId);

    /**
     * Update an existing pet
     *
     * 
     *
     */
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    @Operation(operationId = "updatePet", summary = "Update an existing pet",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied"),
        @ApiResponse(responseCode = "404", description = "Pet not found"),
        @ApiResponse(responseCode = "405", description = "Validation exception") })
    public Pet updatePet(Pet pet);

    /**
     * Updates a pet in the store with form data
     *
     * 
     *
     */
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Operation(operationId = "updatePetWithForm", summary = "Updates a pet in the store with form data",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "405", description = "Invalid input") })
    public void updatePetWithForm(@PathParam("petId") Long petId, @Multipart(value = "name", required = false)  String name, @Multipart(value = "status", required = false)  String status);

    /**
     * uploads an image
     *
     * 
     *
     */
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(operationId = "uploadFile", summary = "uploads an image",  tags={  })
    @ApiResponses(value = { 
        @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))) })
    public ModelApiResponse uploadFile(@PathParam("petId") Long petId, @Multipart(value = "additionalMetadata", required = false)  String additionalMetadata,  @Multipart(value = "file" , required = false) Attachment _fileDetail);
}
