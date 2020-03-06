package org.openapitools.api;

import java.util.List;
import org.openapitools.model.User;
import org.openapitools.api.UserApiService;

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
@Path("/user")
@RequestScoped

@Tag(name = "the user API")




public class UserApi  {

  @Context SecurityContext securityContext;

  @Inject UserApiService delegate;


    @POST
    
    
    
    @Operation(summary = "Create user", description = "This can only be done by the logged in user." , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response createUser(@Parameter(description = "Created user object" ,required=true) User body) {
        return delegate.createUser(body, securityContext);
    }

    @POST
    @Path("/createWithArray")
    
    
    @Operation(summary = "Creates list of users with given input array", description = "" , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response createUsersWithArrayInput(@Parameter(description = "List of user object" ,required=true) List<User> body) {
        return delegate.createUsersWithArrayInput(body, securityContext);
    }

    @POST
    @Path("/createWithList")
    
    
    @Operation(summary = "Creates list of users with given input array", description = "" , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response createUsersWithListInput(@Parameter(description = "List of user object" ,required=true) List<User> body) {
        return delegate.createUsersWithListInput(body, securityContext);
    }

    @DELETE
    @Path("/{username}")
    
    
    @Operation(summary = "Delete user", description = "This can only be done by the logged in user." , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "User not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response deleteUser(@Parameter(in = ParameterIn.PATH,description = "The name that needs to be deleted",required=true) @PathParam("username") String username) {
        return delegate.deleteUser(username, securityContext);
    }

    @GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Get user by user name", description = "" , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = User.class))  )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid username supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "User not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response getUserByName(@Parameter(in = ParameterIn.PATH,description = "The name that needs to be fetched. Use user1 for testing.",required=true) @PathParam("username") String username) {
        return delegate.getUserByName(username, securityContext);
    }

    @GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Logs user into the system", description = "" , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = String.class))  )}  ) , 
        @ApiResponse(responseCode = "400", description = "Invalid username/password supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response loginUser( @NotNull @Schema(description = "The user name for login",required=true)  @QueryParam("username") String username,  @NotNull @Schema(description = "The password for login in clear text",required=true)  @QueryParam("password") String password) {
        return delegate.loginUser(username, password, securityContext);
    }

    @GET
    @Path("/logout")
    
    
    @Operation(summary = "Logs out current logged in user session", description = "" , tags={ "user",  },
    responses = { 
        @ApiResponse(responseCode = "200", description = "successful operation" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response logoutUser() {
        return delegate.logoutUser(securityContext);
    }

    @PUT
    @Path("/{username}")
    
    
    @Operation(summary = "Updated user", description = "This can only be done by the logged in user." , tags={ "user" },
    responses = { 
        @ApiResponse(responseCode = "400", description = "Invalid user supplied" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  ) , 
        @ApiResponse(responseCode = "404", description = "User not found" , content = { @Content(  array = @ArraySchema(schema = @Schema(implementation = Void.class))  )}  )  })
    public Response updateUser(@Parameter(in = ParameterIn.PATH,description = "name that need to be deleted",required=true) @PathParam("username") String username, @Parameter(description = "Updated user object" ,required=true) User body) {
        return delegate.updateUser(username, body, securityContext);
    }
}
