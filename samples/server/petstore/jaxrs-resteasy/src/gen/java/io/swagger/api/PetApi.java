package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.PetApiService;
import io.swagger.api.factories.PetApiServiceFactory;

import io.swagger.model.Pet;
import java.io.File;


import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;

@Path("/pet")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-02-04T01:58:20.368+07:00")

public class PetApi  {
   private final PetApiService delegate = PetApiServiceFactory.getPetApi();


    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response updatePet( Pet body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePet(body,securityContext);
    }

    @POST
    
    @Consumes({ "application/json", "application/xml" })
    @Produces({ "application/json", "application/xml" })
    public Response addPet( Pet body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPet(body,securityContext);
    }

    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/json", "application/xml" })
    public Response findPetsByStatus( @QueryParam("status") List<String> status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByStatus(status,securityContext);
    }

    @GET
    @Path("/findByTags")
    
    @Produces({ "application/json", "application/xml" })
    public Response findPetsByTags( @QueryParam("tags") List<String> tags,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByTags(tags,securityContext);
    }

    @GET
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getPetById( @PathParam("petId") Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetById(petId,securityContext);
    }

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    @Produces({ "application/json", "application/xml" })
    public Response updatePetWithForm( @PathParam("petId") String petId,@FormParam("name")  String name,@FormParam("status")  String status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePetWithForm(petId,name,status,securityContext);
    }

    @DELETE
    @Path("/{petId}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deletePet( @PathParam("petId") Long petId,@HeaderParam("api_key") String apiKey,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deletePet(petId,apiKey,securityContext);
    }

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json", "application/xml" })
    public Response uploadFile(MultipartFormDataInput input, @PathParam("petId") Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFile(input,petId,securityContext);
    }

    @GET
    @Path("/{petId}?testing_byte_array=true")
    
    @Produces({ "application/json", "application/xml" })
    public Response getPetByIdWithByteArray( @PathParam("petId") Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetByIdWithByteArray(petId,securityContext);
    }

}

