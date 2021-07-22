package org.openapitools.api;

import org.openapitools.model.*;
import org.openapitools.api.UserApiService;



import java.util.List;
import org.openapitools.model.User;

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

@Path("/user")


@javax.annotation.Generated(value = "org.openapitools.codegen.languages.JavaResteasyServerCodegen")
public class UserApi  {

    @Inject UserApiService service;

    @POST
    
    
    
    public Response createUser( @NotNull @Valid User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.createUser(body,securityContext);
    }
    @POST
    @Path("/createWithArray")
    
    
    public Response createUsersWithArrayInput( @NotNull @Valid List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.createUsersWithArrayInput(body,securityContext);
    }
    @POST
    @Path("/createWithList")
    
    
    public Response createUsersWithListInput( @NotNull @Valid List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.createUsersWithListInput(body,securityContext);
    }
    @DELETE
    @Path("/{username}")
    
    
    public Response deleteUser( @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.deleteUser(username,securityContext);
    }
    @GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    public Response getUserByName( @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.getUserByName(username,securityContext);
    }
    @GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    public Response loginUser( @NotNull  @QueryParam("username") String username, @NotNull  @QueryParam("password") String password,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.loginUser(username,password,securityContext);
    }
    @GET
    @Path("/logout")
    
    
    public Response logoutUser(@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.logoutUser(securityContext);
    }
    @PUT
    @Path("/{username}")
    
    
    public Response updateUser( @PathParam("username") String username, @NotNull @Valid User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return service.updateUser(username,body,securityContext);
    }
}
