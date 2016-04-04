package io.swagger.api;

import io.swagger.model.Pet;
import io.swagger.model.InlineResponse200;
import java.io.File;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

@Path("/")
public interface PetApi  {
    @POST
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response addPet(Pet body);
    @POST
    @Path("/pet?testing_byte_array=true")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response addPetUsingByteArray(byte[] body);
    @DELETE
    @Path("/pet/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deletePet(@PathParam("petId") Long petId,@HeaderParam("api_key") String apiKey);
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
    @GET
    @Path("/pet/{petId}?response=inline_arbitrary_object")
    
    @Produces({ "application/json", "application/xml" })
    public Response getPetByIdInObject(@PathParam("petId") Long petId);
    @GET
    @Path("/pet/{petId}?testing_byte_array=true")
    
    @Produces({ "application/json", "application/xml" })
    public Response petPetIdtestingByteArraytrueGet(@PathParam("petId") Long petId);
    @PUT
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response updatePet(Pet body);
    @POST
    @Path("/pet/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/json", "application/xml" })
    public Response updatePetWithForm(@PathParam("petId") String petId,@Multipart(value = "name", required = false)  String name,@Multipart(value = "status", required = false)  String status);
    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json", "application/xml" })
    public Response uploadFile(@PathParam("petId") Long petId,@Multipart(value = "additionalMetadata", required = false)  String additionalMetadata, @Multipart(value = "file", required = false) InputStream fileInputStream,
   @Multipart(value = "file" , required = false) Attachment fileDetail);
}

