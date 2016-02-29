package io.swagger.api;

import io.swagger.model.User;
import java.util.*;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

@Path("/v2")
public interface UserApi  {
    @POST
    @Path("/user")
    
    @Produces({ "application/json", "application/xml" })
    public Response createUser(User body);
    @POST
    @Path("/user/createWithArray")
    
    @Produces({ "application/json", "application/xml" })
    public Response createUsersWithArrayInput(List<User> body);
    @POST
    @Path("/user/createWithList")
    
    @Produces({ "application/json", "application/xml" })
    public Response createUsersWithListInput(List<User> body);
    @GET
    @Path("/user/login")
    
    @Produces({ "application/json", "application/xml" })
    public Response loginUser(@QueryParam("username") String username,
    @QueryParam("password") String password);
    @GET
    @Path("/user/logout")
    
    @Produces({ "application/json", "application/xml" })
    public Response logoutUser();
    @GET
    @Path("/user/{username}")
    
    @Produces({ "application/json", "application/xml" })
    public Response getUserByName(@PathParam("username") String username);
    @PUT
    @Path("/user/{username}")
    
    @Produces({ "application/json", "application/xml" })
    public Response updateUser(@PathParam("username") String username,
    User body);
    @DELETE
    @Path("/user/{username}")
    
    @Produces({ "application/json", "application/xml" })
    public Response deleteUser(@PathParam("username") String username);
}

