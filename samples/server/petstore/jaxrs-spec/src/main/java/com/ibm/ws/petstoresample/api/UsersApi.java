package com.ibm.ws.petstoresample.api;

import com.ibm.ws.petstoresample.model.User;
import java.util.List;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;

import io.swagger.annotations.*;

import java.util.List;

@Path("/users")

@Api(description = "the users API")


@javax.annotation.Generated(value = "class io.swagger.codegen.languages.JavaJAXRSSpecServerCodegen", date = "2016-06-06T11:04:02.369-04:00")

public class UsersApi  {

    @POST
    
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Create user", notes = "This can only be done by the logged in user.", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response createUser(User body) {
    	return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithArray")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response createUsersWithArrayInput(List<User> body) {
    	return Response.ok().entity("magic!").build();
    }

    @POST
    @Path("/createWithList")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Creates list of users with given input array", notes = "", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response createUsersWithListInput(List<User> body) {
    	return Response.ok().entity("magic!").build();
    }

    @DELETE
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Delete user", notes = "This can only be done by the logged in user.", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid username supplied", response = void.class),
        @ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response deleteUser(@PathParam("username") String username) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Get user by user name", notes = "", response = User.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = User.class),
        @ApiResponse(code = 400, message = "Invalid username supplied", response = User.class),
        @ApiResponse(code = 404, message = "User not found", response = User.class) })
    public Response getUserByName(@PathParam("username") String username) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/login")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Logs user into the system", notes = "", response = String.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = String.class),
        @ApiResponse(code = 400, message = "Invalid username/password supplied", response = String.class) })
    public Response loginUser(@QueryParam("username") String username,@QueryParam("password") String password) {
    	return Response.ok().entity("magic!").build();
    }

    @GET
    @Path("/logout")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Logs out current logged in user session", notes = "", response = void.class, tags={ "user",  })
    @ApiResponses(value = { 
        @ApiResponse(code = 200, message = "successful operation", response = void.class) })
    public Response logoutUser() {
    	return Response.ok().entity("magic!").build();
    }

    @PUT
    @Path("/{username}")
    
    @Produces({ "application/json", "application/xml" })
    @ApiOperation(value = "Updated user", notes = "This can only be done by the logged in user.", response = void.class, tags={ "user" })
    @ApiResponses(value = { 
        @ApiResponse(code = 400, message = "Invalid user supplied", response = void.class),
        @ApiResponse(code = 404, message = "User not found", response = void.class) })
    public Response updateUser(@PathParam("username") String username,User body) {
    	return Response.ok().entity("magic!").build();
    }
}

