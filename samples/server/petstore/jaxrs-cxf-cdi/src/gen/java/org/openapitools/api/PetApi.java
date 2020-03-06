package org.openapitools.api;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import org.openapitools.api.PetApiService;

import javax.ws.rs.*;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;

import io.swagger.v3.oas.annotations.*;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.media.*;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.InputStream;

import org.apache.cxf.jaxrs.ext.multipart.Attachment;
import org.apache.cxf.jaxrs.ext.multipart.Multipart;

import java.util.Map;
import java.util.List;
import javax.validation.constraints.*;
@Path("/pet")
@RequestScoped

@Tag(name = "the pet API")




public class PetApi  {

  @Context SecurityContext securityContext;

  @Inject PetApiService delegate;


    @POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "" 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "405", description = "Invalid input" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response addPet(@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body) {
        return delegate.addPet(body, securityContext);
    }

    @DELETE
    @Path("/{petId}")
    
    
    @Operation(summary = "Deletes a pet", description = "" 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "400", description = "Invalid pet value" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response deletePet(@Parameter(in = ParameterIn.PATH,description = "Pet id to delete",required=true) @PathParam("petId") Long petId, @Schema(description = "" )@HeaderParam("api_key") String apiKey) {
        return delegate.deletePet(petId, apiKey, securityContext);
    }

    @GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by status", description = "Multiple status values can be provided with comma separated strings" 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content( schema = @Schema(implementation = Pet.class) )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid status value" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response findPetsByStatus( @NotNull @Schema(description = "Status values that need to be considered for filter",required=true)  @QueryParam("status") List<String> status) {
        return delegate.findPetsByStatus(status, securityContext);
    }

    @GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by tags", description = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing." 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content( schema = @Schema(implementation = Pet.class) )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid tag value" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response findPetsByTags( @NotNull @Schema(description = "Tags to filter by",required=true)  @QueryParam("tags") List<String> tags) {
        return delegate.findPetsByTags(tags, securityContext);
    }

    @GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find pet by ID", description = "Returns a single pet" 
        , security = {
             
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Pet.class))  )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "Pet not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response getPetById(@Parameter(in = ParameterIn.PATH,description = "ID of pet to return",required=true) @PathParam("petId") Long petId) {
        return delegate.getPetById(petId, securityContext);
    }

    @PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing pet", description = "" 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "400", description = "Invalid ID supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "Pet not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "405", description = "Validation exception" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response updatePet(@Parameter(description = "Pet object that needs to be added to the store" ,required=true) Pet body) {
        return delegate.updatePet(body, securityContext);
    }

    @POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "" 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet",  },
    responses = { 
        @ApiResponse(responseCode = "405", description = "Invalid input" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response updatePetWithForm(@Parameter(in = ParameterIn.PATH,description = "ID of pet that needs to be updated",required=true) @PathParam("petId") Long petId, @FormParam(value = "name")  String name, @FormParam(value = "status")  String status) {
        return delegate.updatePetWithForm(petId, name, status, securityContext);
    }

    @POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "" 
        , security = {
              @SecurityRequirement(name = "modify pets in your account", scopes ={"write:pets"}),
             @SecurityRequirement(name = "read your pets", scopes ={"read:pets"})
        }
    , tags={ "pet" },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = ModelApiResponse.class))  )}  )  })
    public Response uploadFile(@Parameter(in = ParameterIn.PATH,description = "ID of pet to update",required=true) @PathParam("petId") Long petId, @Multipart(value = "additionalMetadata", required = false)  String additionalMetadata,  @Multipart(value = "file", required = false) InputStream fileInputStream, @Multipart(value = "file" , required = false) Attachment fileDetail) {
        return delegate.uploadFile(petId, additionalMetadata, fileInputStream, fileDetail, securityContext);
    }
}
