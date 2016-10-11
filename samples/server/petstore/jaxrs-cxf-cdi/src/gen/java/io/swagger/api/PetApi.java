package io.swagger.api;

import io.swagger.model.Pet;
import io.swagger.model.ModelApiResponse;
import java.io.File;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

@Path("/v2")
public interface PetApi  {
    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    Response addPet(Pet body);
    @DELETE
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    Response deletePet(@PathParam("petId") Long petId,@HeaderParam("api_key") String apiKey);
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    Response findPetsByStatus(@QueryParam("status") List<String> status);
    @GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    Response findPetsByTags(@QueryParam("tags") List<String> tags);
    @GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    Response getPetById(@PathParam("petId") Long petId);
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/xml", "application/json" })
    Response updatePet(Pet body);
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/xml", "application/json" })
    Response updatePetWithForm(@PathParam("petId") Long petId,@Multipart(value = "name", required = false)  String name,@Multipart(value = "status", required = false)  String status);
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    Response uploadFile(@PathParam("petId") Long petId,@Multipart(value = "additionalMetadata", required = false)  String additionalMetadata, @Multipart(value = "file", required = false) InputStream fileInputStream,
   @Multipart(value = "file" , required = false) Attachment fileDetail);
}

