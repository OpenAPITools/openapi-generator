package com.wordnik.api;

import com.wordnik.swagger.annotations.*;

import com.wordnik.client.model.Pet;
import java.util.List;
import com.wordnik.api.NotFoundException;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/pet.json")
@Api(value = "/pet", description = "the pet API")
@Produces({"application/json"})
public class PetApi {
  @GET
  @Path("/{petId}")
  @ApiOperation(value = "Find pet by ID", notes = "Returns a pet based on ID", responseClass = "Pet")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response getPetById(
    @ApiParam(value = "ID of pet that needs to be fetched"
    ,required=true)@PathParam("petId") String petId
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @POST
  @Path("/")
  @ApiOperation(value = "Add a new pet to the store", notes = "", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response addPet(
    @ApiParam(value = "Pet object that needs to be added to the store"
    ,required=true) Pet body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @PUT
  @Path("/")
  @ApiOperation(value = "Update an existing pet", notes = "", responseClass = "void")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response updatePet(
    @ApiParam(value = "Pet object that needs to be updated in the store"
    ,required=true) Pet body
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @GET
  @Path("/findByStatus")
  @ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma seperated strings", responseClass = "List<Pet>")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response findPetsByStatus(
    @ApiParam(value = "Status values that need to be considered for filter"
    ,required=true, defaultValue="available")@QueryParam("status") String status
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  @GET
  @Path("/findByTags")
  @ApiOperation(value = "Finds Pets by tags", notes = "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.", responseClass = "List<Pet>")
  @ApiErrors(value = { @ApiError(code = 400, reason = "Invalid ID supplied"),
      @ApiError(code = 404, reason = "Pet not found") })
  public Response findPetsByTags(
    @ApiParam(value = "Tags to filter by"
    ,required=true)@QueryParam("tags") String tags
    )
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponse(ApiResponse.OK, "magic!")).build();
  }

  }

