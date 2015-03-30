package io.swagger.api;

import io.swagger.model.*;

import com.wordnik.swagger.annotations.ApiParam;

import com.sun.jersey.multipart.FormDataParam;

import io.swagger.model.Pet;
import java.io.File;

import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import com.sun.jersey.core.header.FormDataContentDisposition;
import com.sun.jersey.multipart.FormDataParam;

import javax.ws.rs.core.Response;
import javax.ws.rs.*;

@Path("/pet")
@com.wordnik.swagger.annotations.Api(value = "/pet", description = "the pet API")
public class PetApi {
  
  @PUT
  
  @Consumes({ "application/json", "application/xml" })
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Update an existing pet", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 405, message = "Validation exception"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "Pet not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied") })

  public Response updatePet(@ApiParam(value = "Pet object that needs to be added to the store"  ) Pet body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @POST
  
  @Consumes({ "application/json", "application/xml" })
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 405, message = "Invalid input") })

  public Response addPet(@ApiParam(value = "Pet object that needs to be added to the store"  ) Pet body)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/findByStatus")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma seperated strings", response = Pet.class, responseContainer = "List")
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid status value") })

  public Response findPetsByStatus(@ApiParam(value = "Status values that need to be considered for filter") @QueryParam("status") List<String> status)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/findByTags")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Finds Pets by tags", notes = "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "List")
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid tag value") })

  public Response findPetsByTags(@ApiParam(value = "Tags to filter by") @QueryParam("tags") List<String> tags)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @GET
  @Path("/{petId}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Find pet by ID", notes = "Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions", response = Pet.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 404, message = "Pet not found"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 200, message = "successful operation"),
    
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied") })

  public Response getPetById(@ApiParam(value = "ID of pet that needs to be fetched",required=true ) @PathParam("petId") Long petId)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @POST
  @Path("/{petId}")
  @Consumes({ "application/x-www-form-urlencoded" })
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 405, message = "Invalid input") })

  public Response updatePetWithForm(@ApiParam(value = "ID of pet that needs to be updated",required=true ) @PathParam("petId") String petId,
    @ApiParam(value = "Updated name of the pet" )@FormParam("name")  String name,
    @ApiParam(value = "Updated status of the pet" )@FormParam("status")  String status)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @DELETE
  @Path("/{petId}")
  
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "Deletes a pet", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 400, message = "Invalid pet value") })

  public Response deletePet(@ApiParam(value = ""  )@HeaderParam("api_key") String apiKey,
    @ApiParam(value = "Pet id to delete",required=true ) @PathParam("petId") Long petId)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
  @POST
  @Path("/{petId}/uploadImage")
  @Consumes({ "multipart/form-data" })
  @Produces({ "application/json", "application/xml" })
  @com.wordnik.swagger.annotations.ApiOperation(value = "uploads an image", notes = "", response = Void.class)
  @com.wordnik.swagger.annotations.ApiResponses(value = { 
    @com.wordnik.swagger.annotations.ApiResponse(code = 0, message = "successful operation") })

  public Response uploadFile(@ApiParam(value = "ID of pet to update",required=true ) @PathParam("petId") Long petId,
    @ApiParam(value = "Additional data to pass to server" )@FormParam("additionalMetadata")  String additionalMetadata,
    @ApiParam(value = "file to upload") @FormDataParam("file") InputStream inputStream,
    @ApiParam(value = "file detail") @FormDataParam("file") FormDataContentDisposition fileDetail)
      throws NotFoundException {
      // do some magic!
      return Response.ok().entity(new ApiResponseMessage(ApiResponseMessage.OK, "magic!")).build();
  }

  
}
