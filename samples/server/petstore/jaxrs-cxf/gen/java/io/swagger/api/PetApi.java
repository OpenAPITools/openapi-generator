package io.swagger.api;

import io.swagger.model.Pet;
import java.io.File;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

@Path("/v2")
public interface PetApi  {
    @PUT
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response updatePet(Pet body);
    @POST
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response addPet(Pet body);
    @GET
    @Path("/pet/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    public Response findPetsByStatus(@QueryParam("status") List<String> status);
    @GET
    @Path("/pet/findByTags")
    
    @Produces({ "application/json", "application/xml" })
    public Response findPetsByTags(@QueryParam("tags") List<String> tags);
    @GET
    @Path("/pet/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getPetById(@PathParam("petId") Long petId);
    @POST
    @Path("/pet/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/json", "application/xml" })
    public Response updatePetWithForm(@PathParam("petId") String petId,
    @Multipart(value = "name", required = false)  String name,
    @Multipart(value = "status", required = false)  String status);
    @DELETE
    @Path("/pet/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deletePet(@PathParam("petId") Long petId,
    @HeaderParam("api_key") String apiKey);
    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json", "application/xml" })
    public Response uploadFile(@PathParam("petId") Long petId,
    @Multipart(value = "additionalMetadata", required = false)  String additionalMetadata,
     @Multipart(value = "file", required = false) InputStream fileInputStream,
   @Multipart(value = "file" , required = false) Attachment fileDetail);
    @GET
    @Path("/pet/{petId}?testing_byte_array=true")
    
    @Produces({ "application/json", "application/xml" })
    public Response getPetByIdWithByteArray(@PathParam("petId") Long petId);
    @POST
    @Path("/pet?testing_byte_array=true")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response addPetUsingByteArray(byte[] body);
}

