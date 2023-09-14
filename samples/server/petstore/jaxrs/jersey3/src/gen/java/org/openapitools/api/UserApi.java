package org.openapitools.api;

import org.openapitools.api.UserApiService;
import org.openapitools.api.factories.UserApiServiceFactory;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;

import java.util.Date;
import org.openapitools.model.User;

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

@Path("/user")


@Tag(description = "the user API", name = "")
@jakarta.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class UserApi  {

   private final UserApiService delegate;

   public UserApi(@Context ServletConfig servletContext) {

      UserApiService delegate = null;
      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("UserApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (UserApiService) Class.forName(implClass).getDeclaredConstructor().newInstance();
            } catch (Exception e) {
               throw new RuntimeException(e);
            }
         }
      }

      if (delegate == null) {
         delegate = UserApiServiceFactory.getUserApi();
      }
      this.delegate = delegate;
   }


    @jakarta.ws.rs.POST
    @Consumes({ "application/json" })
    @Operation(summary = "Create user", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response createUser(@Schema(description = "Created user object", required = true) @NotNull @Valid  User user,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUser(user, securityContext);
    }

    @jakarta.ws.rs.POST
    @Path("/createWithArray")
    @Consumes({ "application/json" })
    @Operation(summary = "Creates list of users with given input array", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response createUsersWithArrayInput(@Schema(description = "List of user object", required = true) @NotNull @Valid  List<User> user,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUsersWithArrayInput(user, securityContext);
    }

    @jakarta.ws.rs.POST
    @Path("/createWithList")
    @Consumes({ "application/json" })
    @Operation(summary = "Creates list of users with given input array", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response createUsersWithListInput(@Schema(description = "List of user object", required = true) @NotNull @Valid  List<User> user,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUsersWithListInput(user, securityContext);
    }

    @jakarta.ws.rs.DELETE
    @Path("/{username}")
    @Operation(summary = "Delete user", description = "", responses = {
            @ApiResponse(responseCode = "400", description = "Invalid username supplied", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "User not found", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response deleteUser(@Schema(description= "The name that needs to be deleted", required = true) @PathParam("username") @NotNull  String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteUser(username, securityContext);
    }

    @jakarta.ws.rs.GET
    @Path("/{username}")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Get user by user name", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = User.class))),
            @ApiResponse(responseCode = "400", description = "Invalid username supplied", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "User not found", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response getUserByName(@Schema(description= "The name that needs to be fetched. Use user1 for testing.", required = true) @PathParam("username") @NotNull  String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getUserByName(username, securityContext);
    }

    @jakarta.ws.rs.GET
    @Path("/login")
    @Produces({ "application/xml", "application/json" })
    @Operation(summary = "Logs user into the system", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = String.class))),
            @ApiResponse(responseCode = "400", description = "Invalid username/password supplied", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response loginUser(@Schema(description = "The user name for login") @QueryParam("username") @NotNull  String username,@Schema(description = "The password for login in clear text") @QueryParam("password") @NotNull  String password,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.loginUser(username, password, securityContext);
    }

    @jakarta.ws.rs.GET
    @Path("/logout")
    @Operation(summary = "Logs out current logged in user session", description = "", responses = {
            @ApiResponse(responseCode = "200", description = "successful operation", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response logoutUser(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.logoutUser(securityContext);
    }

    @jakarta.ws.rs.PUT
    @Path("/{username}")
    @Consumes({ "application/json" })
    @Operation(summary = "Updated user", description = "", responses = {
            @ApiResponse(responseCode = "400", description = "Invalid user supplied", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            @ApiResponse(responseCode = "404", description = "User not found", content = 
                @Content(schema = @Schema(implementation = Void.class))),
            }, tags={ "user", }) 
    public Response updateUser(@Schema(description= "name that need to be deleted", required = true) @PathParam("username") @NotNull  String username,@Schema(description = "Updated user object", required = true) @NotNull @Valid  User user,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateUser(username, user, securityContext);
    }
}
