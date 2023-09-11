package org.openapitools.api;

import org.openapitools.api.PetApiService;
import org.openapitools.api.factories.PetApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.io.File;
import org.openapitools.model.ModelApiResponse;
import org.openapitools.model.Pet;
import java.util.Set;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataParam;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import jakarta.servlet.ServletConfig;
import jakarta.ws.rs.core.Context;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.SecurityContext;
import jakarta.ws.rs.*;
import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

@Path("/pet")


@Tag(description = "the pet API", name = "")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class PetApi  {
   private final PetApiService delegate;

   public PetApi(@Context ServletConfig servletContext) {
      PetApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("PetApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (PetApiService) Class.forName(implClass).newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         }
      }

      if (delegate == null) {
         delegate = PetApiServiceFactory.getPetApi();
      }

      this.delegate = delegate;
   }

    @jakarta.ws.rs.POST
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Add a new pet to the store", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "405", description = "Invalid input", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Add a new pet to the store", notes = "", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response addPet(@Schema(description = "Pet object that needs to be added to the store", required = true) @NotNull @Valid  Pet pet,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.addPet(pet, securityContext);
    }
    @jakarta.ws.rs.DELETE
    @Path("/{petId}")
    
    
    @Operation(summary = "Deletes a pet", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "400", description = "Invalid pet value", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Deletes a pet", notes = "", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid pet value", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response deletePet(@Schema(description= "Pet id to delete", required = true) @PathParam("petId") @NotNull  Long petId,@Schema(description = "" )@HeaderParam("api_key") String apiKey,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deletePet(petId, apiKey, securityContext);
    }
    @jakarta.ws.rs.GET
    @Path("/findByStatus")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by status", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
            @ApiResponse(responseCode = "400", description = "Invalid status value", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Finds Pets by status", notes = "Multiple status values can be provided with comma separated strings", response = Pet.class, responseContainer = "List", authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "List"),
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid status value", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response findPetsByStatus(@Schema(description = "Status values that need to be considered for filter", required = true) @QueryParam("status") @NotNull @Valid  List<String> status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByStatus(status, securityContext);
    }
    @jakarta.ws.rs.GET
    @Path("/findByTags")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Finds Pets by tags", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
            @ApiResponse(responseCode = "400", description = "Invalid tag value", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Finds Pets by tags", notes = "Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.", response = Pet.class, responseContainer = "Set", authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class, responseContainer = "Set"),
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid tag value", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response findPetsByTags(@Schema(description = "Tags to filter by", required = true) @QueryParam("tags") @NotNull @Valid  Set<String> tags,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.findPetsByTags(tags, securityContext);
    }
    @jakarta.ws.rs.GET
    @Path("/{petId}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Find pet by ID", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = Pet.class))),
            @ApiResponse(responseCode = "400", description = "Invalid ID supplied", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "Pet not found", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Find pet by ID", notes = "Returns a single pet", response = Pet.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "api_key")
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Pet.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response getPetById(@Schema(description= "ID of pet to return", required = true) @PathParam("petId") @NotNull  Long petId,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getPetById(petId, securityContext);
    }
    @jakarta.ws.rs.PUT
    
    @Consumes({ "application/json", "application/xml" })
    
    @Operation(summary = "Update an existing pet", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "400", description = "Invalid ID supplied", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "Pet not found", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "405", description = "Validation exception", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Update an existing pet", notes = "", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid ID supplied", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 404, message = "Pet not found", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 405, message = "Validation exception", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response updatePet(@Schema(description = "Pet object that needs to be added to the store", required = true) @NotNull @Valid  Pet pet,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePet(pet, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/{petId}")
    @Consumes({ "application/x-www-form-urlencoded" })
    
    @Operation(summary = "Updates a pet in the store with form data", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "Successful operation", content = @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "405", description = "Invalid input", content = @Content(schema = @Schema(implementation = Void.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "Updates a pet in the store with form data", notes = "", response = Void.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "Successful operation", response = Void.class),
//        
//        @io.swagger.annotations.ApiResponse(code = 405, message = "Invalid input", response = Void.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response updatePetWithForm(@Schema(description= "ID of pet that needs to be updated", required = true) @PathParam("petId") @NotNull  Long petId,@Schema(description = "Updated name of the pet")  @FormParam("name")  String name,@Schema(description = "Updated status of the pet")  @FormParam("status")  String status,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updatePetWithForm(petId, name, status, securityContext);
    }
    @jakarta.ws.rs.POST
    @Path("/{petId}/uploadImage")
    @Consumes({ "multipart/form-data" })
    @Produces({ "application/json" })
    @Operation(summary = "uploads an image", description = "", 
        responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = @Content(schema = @Schema(implementation = ModelApiResponse.class))),
            }
    , tags={ "pet", }) 
//    ==> Previous Swagger1 annotations <== RESOLVED WITH THE ABOVE FOR SWAGGER 2.X   
//    @io.swagger.annotations.ApiOperation(value = "uploads an image", notes = "", response = ModelApiResponse.class, authorizations = {
//        @io.swagger.annotations.Authorization(value = "petstore_auth", scopes = {
//            @io.swagger.annotations.AuthorizationScope(scope = "write:pets", description = "modify pets in your account"),
//            @io.swagger.annotations.AuthorizationScope(scope = "read:pets", description = "read your pets")
//        })
//    }, tags={ "pet", })
//    @io.swagger.annotations.ApiResponses(value = {
//        
//        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = ModelApiResponse.class)
//        
//    })
//    ==> Previous Swagger1 annotations <== HOW TO DEAL WITH THIS PART?
//    
    public Response uploadFile(@Schema(description= "ID of pet to update", required = true) @PathParam("petId") @NotNull  Long petId,@Schema(description = "Additional data to pass to server")@FormDataParam("additionalMetadata")  String additionalMetadata,
 @FormDataParam("file") FormDataBodyPart _fileBodypart ,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.uploadFile(petId, additionalMetadata, _fileBodypart, securityContext);
    }
}
