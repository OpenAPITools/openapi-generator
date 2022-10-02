package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;


import java.io.InputStream;
import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/pet")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen")public interface PetApi {

    @POST
    @Consumes({ "application/json", "application/xml" })
    void addPet(@Valid @NotNull Pet body);

    @DELETE
    @Path("/{petId}")
    void deletePet(@PathParam("petId") Long petId,@HeaderParam("api_key")   String apiKey);

    @GET
    @Path("/findByStatus")
    @Produces({ "application/xml", "application/json" })
    List<Pet> findPetsByStatus(@QueryParam("status") @NotNull   List<String> status);

    @GET
    @Path("/findByTags")
    @Produces({ "application/xml", "application/json" })
    Set<Pet> findPetsByTags(@QueryParam("tags") @NotNull   Set<String> tags);

    @GET
    @Path("/{petId}")
    @Produces({ "application/xml", "application/json" })
    Pet getPetById(@PathParam("petId") Long petId);

    @PUT
    @Consumes({ "application/json", "application/xml" })
    void updatePet(@Valid @NotNull Pet body);

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    void updatePetWithForm(@PathParam("petId") Long petId,@FormParam(value = "name")  String name,@FormParam(value = "status")  String status);

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    ModelApiResponse uploadFile(@PathParam("petId") Long petId,@FormParam(value = "additionalMetadata")  String additionalMetadata, @FormParam(value = "file") InputStream _fileInputStream);
}
