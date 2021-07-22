package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.PetApiService;



import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.inject.Inject;

import javax.validation.constraints.*;
import javax.validation.Valid;
import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;

@Path("/pet")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public class PetApi  {

    @Inject PetApiService service;

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    public Response addPet( @NotNull @Valid Pet body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.addPet(body,securityContext);
    }
    @DELETE
    @Path("/{petId}")
    
    
    public Response deletePet( @PathParam("petId") Long petId,   @HeaderParam("api_key") String apiKey,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.deletePet(petId,apiKey,securityContext);
    }
    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    public Response findPetsByStatus( @NotNull  @QueryParam("status") List<String> status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.findPetsByStatus(status,securityContext);
    }
    @GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    public Response findPetsByTags( @NotNull  @QueryParam("tags") List<String> tags,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.findPetsByTags(tags,securityContext);
    }
    @GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    public Response getPetById( @PathParam("petId") Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getPetById(petId,securityContext);
    }
    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    public Response updatePet( @NotNull @Valid Pet body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.updatePet(body,securityContext);
    }
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    public Response updatePetWithForm( @PathParam("petId") Long petId, @FormParam("name") String name, @FormParam("status") String status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.updatePetWithForm(petId,name,status,securityContext);
    }
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    public Response uploadFile(MultipartFormDataInput input, @PathParam("petId") Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.uploadFile(input,petId,securityContext);
    }
}
