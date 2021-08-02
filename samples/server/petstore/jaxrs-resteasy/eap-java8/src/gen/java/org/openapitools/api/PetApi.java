package org.openapitools.api;

import java.util.List;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;

import org.jboss.resteasy.plugins.providers.multipart.MultipartFormDataInput;
import org.openapitools.model.Pet;

import io.swagger.annotations.ApiParam;

@Path("/pet")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyEapServerCodegen")
public interface PetApi  {

    @POST

    @Consumes({ "application/json", "application/xml" }) Response addPet( @NotNull @Valid Pet body,@Context SecurityContext securityContext);
    @DELETE
    @Path("/{petId}") Response deletePet( @PathParam("petId") Long petId,  @ApiParam(value = "" ) @HeaderParam("api_key") String apiKey,@Context SecurityContext securityContext);
    @GET
    @Path("/findByStatus")

    @Produces({ "application/xml", "application/json" }) Response findPetsByStatus( @NotNull  @QueryParam("status") List<String> status,@Context SecurityContext securityContext);
    @GET
    @Path("/findByTags")

    @Produces({ "application/xml", "application/json" }) Response findPetsByTags( @NotNull  @QueryParam("tags") List<String> tags,@Context SecurityContext securityContext);
    @GET
    @Path("/{petId}")

    @Produces({ "application/xml", "application/json" }) Response getPetById( @PathParam("petId") Long petId,@Context SecurityContext securityContext);
    @PUT

    @Consumes({ "application/json", "application/xml" }) Response updatePet( @NotNull @Valid Pet body,@Context SecurityContext securityContext);
    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" }) Response updatePetWithForm( @PathParam("petId") Long petId, @FormParam("name") String name, @FormParam("status") String status,@Context SecurityContext securityContext);
    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" }) Response uploadFile(MultipartFormDataInput input, @PathParam("petId") Long petId,@Context SecurityContext securityContext);
}
