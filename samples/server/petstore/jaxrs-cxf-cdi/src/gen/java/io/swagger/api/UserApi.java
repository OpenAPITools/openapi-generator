package io.swagger.api;

import io.swagger.model.User;
import java.util.List;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import org.apache.cxf.jaxrs.ext.multipart.*;

@Path("/v2")
public interface UserApi  {
    @POST
    
    
    @Produces({ "application/xml", "application/json" })
    Response createUser(User body);
    @POST
    @Path("/createWithArray")
    
    @Produces({ "application/xml", "application/json" })
    Response createUsersWithArrayInput(List<User> body);
    @POST
    @Path("/createWithList")
    
    @Produces({ "application/xml", "application/json" })
    Response createUsersWithListInput(List<User> body);
    @DELETE
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    Response deleteUser(@PathParam("username") String username);
    @GET
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    Response getUserByName(@PathParam("username") String username);
    @GET
    @Path("/login")
    
    @Produces({ "application/xml", "application/json" })
    Response loginUser(@QueryParam("username") String username,@QueryParam("password") String password);
    @GET
    @Path("/logout")
    
    @Produces({ "application/xml", "application/json" })
    Response logoutUser();
    @PUT
    @Path("/{username}")
    
    @Produces({ "application/xml", "application/json" })
    Response updateUser(@PathParam("username") String username,User body);
}

