package io.swagger.api;

import io.swagger.model.*;
import io.swagger.api.UserApiService;
import io.swagger.api.factories.UserApiServiceFactory;

import io.swagger.model.User;
import java.util.*;


import java.util.List;
import io.swagger.api.NotFoundException;

import java.io.InputStream;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.*;

@Path("/user")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaResteasyServerCodegen", date = "2016-02-04T01:58:20.368+07:00")

public class UserApi  {
   private final UserApiService delegate = UserApiServiceFactory.getUserApi();


    @POST
    
    
    @Produces({ "application/json", "application/xml" })
    public Response createUser( User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUser(body,securityContext);
    }

    @POST
    @Path("/createWithArray")
    
    @Produces({ "application/json", "application/xml" })
    public Response createUsersWithArrayInput( List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUsersWithArrayInput(body,securityContext);
    }

    @POST
    @Path("/createWithList")
    
    @Produces({ "application/json", "application/xml" })
    public Response createUsersWithListInput( List<User> body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.createUsersWithListInput(body,securityContext);
    }

    @GET
    @Path("/login")
    
    @Produces({ "application/json", "application/xml" })
    public Response loginUser( @QueryParam("username") String username, @QueryParam("password") String password,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.loginUser(username,password,securityContext);
    }

    @GET
    @Path("/logout")
    
    @Produces({ "application/json", "application/xml" })
    public Response logoutUser(@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.logoutUser(securityContext);
    }

    @GET
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getUserByName( @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.getUserByName(username,securityContext);
    }

    @PUT
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    public Response updateUser( @PathParam("username") String username, User body,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.updateUser(username,body,securityContext);
    }

    @DELETE
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deleteUser( @PathParam("username") String username,@Context SecurityContext securityContext)
    throws NotFoundException {
        return delegate.deleteUser(username,securityContext);
    }

}

