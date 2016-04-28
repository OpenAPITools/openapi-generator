package io.swagger.api;

import io.swagger.model.Pet;
import java.io.File;
import io.swagger.model.ModelApiResponse;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

@Path("/")
public interface PetApi  {
    @POST
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    public Response addPet(Pet body);
    @DELETE
    @Path("/pet/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    public Response deletePet(@PathParam("petId") Long petId,@HeaderParam("api_key") String apiKey);
    @GET
    @Path("/pet/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    public Response findPetsByStatus(@QueryParam("status") List<String> status);
    @GET
    @Path("/pet/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    public Response findPetsByTags(@QueryParam("tags") List<String> tags);
    @GET
    @Path("/pet/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    public Response getPetById(@PathParam("petId") Long petId);
    @PUT
    @Path("/pet")
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    public Response updatePet(Pet body);
    @POST
    @Path("/pet/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/xml", "application/json" })
    public Response updatePetWithForm(@PathParam("petId") Long petId,@Multipart(value = "name", required = false)  String name,@Multipart(value = "status", required = false)  String status);
    @POST
    @Path("/pet/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    public Response uploadFile(@PathParam("petId") Long petId,@Multipart(value = "additionalMetadata", required = false)  String additionalMetadata, @Multipart(value = "file", required = false) InputStream fileInputStream,
   @Multipart(value = "file" , required = false) Attachment fileDetail);
}

