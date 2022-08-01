package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.UserApiService;
import org.openapitools.api.factories.UserApiServiceFactory;

import io.swagger.annotations.ApiParam;
import io.swagger.jaxrs.*;

import java.util.List;
import java.time.OffsetDateTime;
import org.openapitools.model.User;

import java.util.Map;
import java.util.List;
import org.openapitools.api.NotFoundException;

import java.io.InputStream;

import org.glassfish.jersey.media.multipart.FormDataParam;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;

import javax.servlet.ServletConfig;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;
import javax.validation.constraints.*;
import javax.validation.Valid;

@Path("/user")


@io.swagger.annotations.Api(description = "the user API")
@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaJerseyServerCodegen")
public class UserApi  {
   private final UserApiService delegate;

   public UserApi(@Context ServletConfig servletContext) {
      UserApiService delegate = null;

      if (servletContext != null) {
         String implClass = servletContext.getInitParameter("UserApi.implementation");
         if (implClass != null && !"".equals(implClass.trim())) {
            try {
               delegate = (UserApiService) Class.forName(implClass).newInstance();
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

    @javax.ws.rs.POST
    
    
    
    @io.swagger.annotations.ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    public Response createUser(@ApiParam(value = "Created user object", required = true) @NotNull @Valid  User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUser(body, securityContext);
    }
    @javax.ws.rs.POST
    @Path("/createWithArray")
    
    
    @io.swagger.annotations.ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    public Response createUsersWithArrayInput(@ApiParam(value = "List of user object", required = true) @NotNull @Valid  List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUsersWithArrayInput(body, securityContext);
    }
    @javax.ws.rs.POST
    @Path("/createWithList")
    
    
    @io.swagger.annotations.ApiOperation(value = "Creates list of users with given input array", notes = "", response = Void.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    public Response createUsersWithListInput(@ApiParam(value = "List of user object", required = true) @NotNull @Valid  List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUsersWithListInput(body, securityContext);
    }
    @javax.ws.rs.DELETE
    @Path("/{username}")
    
    
    @io.swagger.annotations.ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    public Response deleteUser(@ApiParam(value = "The name that needs to be deleted", required = true) @PathParam("username") @NotNull  String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteUser(username, securityContext);
    }
    @javax.ws.rs.GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = User.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    public Response getUserByName(@ApiParam(value = "The name that needs to be fetched. Use user1 for testing.", required = true) @PathParam("username") @NotNull  String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getUserByName(username, securityContext);
    }
    @javax.ws.rs.GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    @io.swagger.annotations.ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = String.class),
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid username/password supplied", response = Void.class)
    })
    public Response loginUser(@ApiParam(value = "The user name for login", required = true) @QueryParam("username") @NotNull  String username,@ApiParam(value = "The password for login in clear text", required = true) @QueryParam("password") @NotNull  String password,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.loginUser(username, password, securityContext);
    }
    @javax.ws.rs.GET
    @Path("/logout")
    
    
    @io.swagger.annotations.ApiOperation(value = "Logs out current logged in user session", notes = "", response = Void.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 200, message = "successful operation", response = Void.class)
    })
    public Response logoutUser(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.logoutUser(securityContext);
    }
    @javax.ws.rs.PUT
    @Path("/{username}")
    
    
    @io.swagger.annotations.ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = Void.class, tags={ "user", })
    @io.swagger.annotations.ApiResponses(value = {
        @io.swagger.annotations.ApiResponse(code = 400, message = "Invalid user supplied", response = Void.class),
        @io.swagger.annotations.ApiResponse(code = 404, message = "User not found", response = Void.class)
    })
    public Response updateUser(@ApiParam(value = "name that need to be deleted", required = true) @PathParam("username") @NotNull  String username,@ApiParam(value = "Updated user object", required = true) @NotNull @Valid  User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateUser(username, body, securityContext);
    }
}
